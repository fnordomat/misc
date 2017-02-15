#!/usr/bin/python3

# Probably not very useful yet. Just an idea.

# The idea is that your tor client should not sit idle when you're not active, immediately betraying that you're not active. It's not meant to be used all the time & use up resources of the network, but could be handy sometimes.

# It's 3 tasks. If server fails, then program should exit. If server fails with errno 98, then switch to a different port. Set up hidden service only when that port is known. Only client should wait for publication of service.

# Once operational, the program sends random amounts of random traffic to the hidden service at random intervals. Feel free to implement more realistic-looking protocols.

# dependencies
#   PySocks
#   stem
#   pycrypto

import time

import sys

import stem
from stem import connection
from stem.control import Controller

import Crypto
from Crypto import Random

import signal
import multiprocessing
from multiprocessing import Process, Queue

# config settings
CONTROL_PORT = 9151
SOCKS_PORT = 9150
DEFAULT_MEANWAIT = 2.1
DEFAULT_MEANBYTES = 16384

import socket
import socks

import math

import re
import getopt

socks.setdefaultproxy(socks.PROXY_TYPE_SOCKS5, '127.0.0.1', SOCKS_PORT)

extport = 80 # set any port for the hidden service
# hidden service is public.

# any good? won't they be able to fit the distribution?
class Exp:
    def __init__(self, lambd = 1.0):
        self.l = lambd

    def cdf(self, x):
        return (lambda x: 1 - math.exp(- self.l * x))(x)

    def sample(self, randomstream, rprecisionbytes = 2, sprecision = 16):
        r = randomstream.read(rprecisionbytes) #
        val = 0.0
        num = 1
        while r != b'':
            byte = r[0] # python3
            r = r[1:]
            for i in range(8):
                bit = (byte & (1 << i)) >> i
                con = bit * 1.0 / (1 << num)
                val += con
                num += 1

        lower = 0.0
        upper = None
        point = 1.0
        for i in range(sprecision):
            if self.cdf(point) == val:
                return point
            if self.cdf(point) > val:
                upper = point
                point = (upper + lower) / 2.0
            if self.cdf(point) < val:
                if not upper:
                    point *= 2
                else:
                    lower = point
                    point = (upper + lower) / 2.0

        return point

def handler(signal, frame):
    print ("Exiting thread")
    sys.exit(0)

class CoverSender:

    def __init__(self, onion):
        self.r = Random.new()
        self.s = socks.socksocket()
        self.address = onion

    def run(self, meanwait = DEFAULT_MEANWAIT, meanbytes = DEFAULT_MEANBYTES
):
        """
        To be run as a thread / process
        
        Keyword args:
        meanwait -- mean waiting time between bursts (in seconds)
        meanbytes -- mean #bytes to send per burst
        """
        signal.signal(signal.SIGINT, handler)

        Random.atfork()

        # distribution parameters
        waitdistr = Exp(1.0 / meanwait)  #   waiting time
        lendistr  = Exp(1.0 / meanbytes) # message length

        while True:

            try:
                self.s = socks.socksocket()
                self.s.setproxy(socks.PROXY_TYPE_SOCKS5,
                                "127.0.0.1", SOCKS_PORT, True)
                print (" c Connect to %s:%d ..." % (self.address, extport))
                self.s.connect((self.address, extport))

                msglen = int(math.floor(lendistr.sample(self.r)))

                msg = self.r.read(msglen)
                print (" c Attempting to send %d bytes ..." % msglen)

                numsent = 0
                while numsent < msglen:
                    numsent += self.s.send(msg[numsent:])

                print (" c Done, closing socket")
                self.s.close()

            except Exception as e:
                print (" c Client error: %s" % e)

            pausea = waitdistr.sample(self.r)
            
            print (" c Sleep %.2fs ..." % pausea)
            time.sleep(pausea)

            
class CoverServer:

    def __init__(self, q):
        self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.outputq = q

    def run(self):
        signal.signal(signal.SIGINT, handler)

        s = self.socket
        
        while True:
            try:
                s.bind(('127.0.0.1', 0))
                break
            except Exception as e:
                # this should'nt happen at all for port chosen by OS
                if ("%s" % e)[:10] == "[Errno 98]":
                    f = 1.0
                    print (" s Bind failure: %s (retry %d)" % (e, f))
                    time.sleep(f)
                else:
                    print (" s Bind failure: %s" % e)
                    raise e

        self.outputq.put({"myport":s.getsockname()[1]})

        s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        s.listen(600)

        while True:
            try:
                c, addr = s.accept()
                print (' s Got connection')
                # ... all in the same thread ...

                while True:
                    x = c.recv(1024)
                    print (" s Recvd bytes %d" % len(x))

                    # pay attention to str != bytes
                    if x == b'':
                        print (' s End of transmission') # for whatever reason
                        break
                
                c.close()

            except Exception as e:
                print (" s Server error: %s" % e)

class StemSetup:
    def __init__(self, outq, inq):
        self.outputq = outq
        self.inputq  = inq

    def run(self, myport, await = True):
        signal.signal(signal.SIGINT, handler)

        print(' * Connecting to tor controller')
        with Controller.from_port(port=CONTROL_PORT) as controller:
            try:
                controller.authenticate()
                print(' * Authenticated, starting hs')

                response = controller.create_ephemeral_hidden_service({extport: myport}, await_publication = await)

                if await:
                    print(" * Published service %s.onion" % response.service_id)
                else:
                    print(" * Created service %s.onion" % response.service_id)            
                    
                address = "%s.onion" % response.service_id

                self.outputq.put({"onion":address})
               
                while True:
                    item = self.inputq.get()
                    if item == "quit":
                        print (" * Quit hs controller thread")
                        return

            except stem.connection.MissingPassword:
                pw = getpass.getpass("Controller password: ")
                
                try:
                    controller.authenticate(password = pw)
                except stem.connection.PasswordAuthFailed:
                    print("Unable to authenticate, password is incorrect")
                    sys.exit(1)
                    
            except stem.connection.AuthenticationFailure as exc:
                print("Unable to authenticate: %s" % exc)
                sys.exit(1)

def main(options, meanwait=DEFAULT_MEANWAIT, meanbytes=DEFAULT_MEANBYTES):
    o = options

    commq3 = None
    onion = ''

    processes1 = []
    processes2 = [] # exit only after processes1 quit

    if 'remote' in o.keys():
        onion = o['remote']

    if o['server']:
        commq1 = Queue()
        cS = CoverServer(commq1)
        pS = Process(target=CoverServer.run, args=(cS,))
        print(" * Starting server listener")
        pS.start()
        myport = commq1.get()['myport']
        print(" * Server up at localport %d" % myport)

        commq2 = Queue()
        commq3 = Queue() # used to signal end of work
        cstem = StemSetup(commq2, commq3)
        pstem = Process(target=StemSetup.run, args=(cstem, myport, options['await']))
        pstem.start()
        onion = commq2.get()['onion']

        processes2.append(pstem)
        processes1.append(pS)

    if o['client'] and len(onion):
        print(" * Init cover traffic sender (client to %s)" % onion)
        cs = CoverSender(onion)
        ps = Process(target=CoverSender.run, args=(cs, meanwait, meanbytes))
        print(" * Starting cover traffic sender")
        ps.start()
        processes1.append(ps)

    if o['client'] and not len(onion):
        print(" * Won't start cover traffic sender, no service given")

    for p1 in processes1:
        p1.join()

    if o['server']:
        commq3.put("quit")

    for p in processes2:
        p.join()


def usage():
    print ("You're using it wrong, see source code")

if __name__ == "__main__":

    try:
        opts, args = getopt.getopt(sys.argv[1:],
            'w:b:hsaAc:',
            ['meanwait=', "meanbytes=", 'help', 'server', "autoclient", "await", 'client=' ])
    except getopt.GetoptError as e:
        print (e)
        usage()
        sys.exit(2)
    
    meanwait =  DEFAULT_MEANWAIT
    meanbytes = DEFAULT_MEANBYTES

    o = {'await':False, 'server':False, 'client':False}

    for opt, arg in opts:
#        print (opt, arg)
        if opt in ('-h', '--help'):
            usage()
            sys.exit(2)

        if opt in ('-s', '--server'):
            o['server'] = True

        if opt in ('-a', '--autoclient'):
            o['client'] = True

        if opt in ('-A', '--await'):
            o['await'] = True        

        if opt in ('-c', '--client'):
            o['client'] = True
            o['remote'] = arg
            if not re.match("\.onion$", o['remote']):
                o['remote'] += '.onion'

        if opt in ('-b', '--meanbytes'):
            try:
                meanbytes = int(arg)
            except Exception as e:
                usage()
                sys.exit(2)

        if opt in ('-w', '--meanwait'):
            try:
                meanwait = float(arg)
            except Exception as e:
                usage()
                sys.exit(2)

    try:
        main(o, meanwait=meanwait, meanbytes=meanbytes)

    except KeyboardInterrupt as e:
        print ("Got ctrl-c, exiting more or less gracefully")

