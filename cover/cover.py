# Probably not very useful yet. Just an idea.

# The idea is that your tor client should not sit idle when you're not active, immediately betraying that you're not active.

# It's 3 tasks. If server fails, then program should exit. If server fails with errno 98, then switch to a different port. Set up hidden service only when that port is known. Only client should wait for publication of service.

# Then random amounts of random traffic is sent to the hidden service at random intervals.

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

CONTROL_PORT = 9151
SOCKS_PORT = 9150

import socket
import socks

import math

socks.setdefaultproxy(socks.PROXY_TYPE_SOCKS5, '127.0.0.1', SOCKS_PORT)

extport = 22 # set any port for the hidden service
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
        while r != "":
            byte = ord(r[0])
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
    print "Exiting thread"
    sys.exit(0)


class CoverSender:

    def __init__(self, address):
        self.r = Random.new()
        self.s = socks.socksocket()
        self.address = address

    def run(self):

        signal.signal(signal.SIGINT, handler)

        Random.atfork()

        # distribution parameters
        meanwait  = 10.0   # mean waiting time in seconds
        meanbytes = 8000   # mean number of bytes to send
        waitdistr = Exp(1.0 / meanwait)  #   waiting time
        lendistr  = Exp(1.0 / meanbytes) # message length

        while True:
            pausea = waitdistr.sample(self.r)

            print " c Sleep %.2fs ..." % pausea
            time.sleep(pausea)

            try:
                self.s = socks.socksocket()
                self.s.setproxy(socks.PROXY_TYPE_SOCKS5,
                                "127.0.0.1", SOCKS_PORT, True)
                print " c Connect to %s:%d ..." % (self.address, extport)
                self.s.connect((self.address, extport))

                msglen = int(math.floor(lendistr.sample(self.r)))

                msg = self.r.read(msglen)
                print " c Attempting to send %d bytes ..." % msglen

                numsent = 0
                while numsent < msglen:
                    numsent += self.s.send(msg[numsent:])

                print " c Done, closing socket"
                self.s.close()

            except Exception, e:
                print " c Client error: %s" % e
            
class CoverServer:

    def __init__(self, q):
        self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.commq = q

    def run(self):
        signal.signal(signal.SIGINT, handler)
#        signal.signal(signal.SIGINT, signal.SIG_IGN)

        s = self.socket
        
        while True:
            try:
                s.bind(('127.0.0.1', 0))
                break
            except Exception, e:
                # this should'nt happen at all for port chosen by OS
                if ("%s" % e)[:10] == "[Errno 98]":
                    f = 1.0
                    print " s Bind failure: %s (retry %d)" % (e, f)
                    time.sleep(f)
                else:
                    print " s Bind failure: %s" % e
                    raise e

        self.commq.put({"myport":s.getsockname()[1]})

        s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        s.listen(600)

        while True:
            try:
                c, addr = s.accept()
                print ' s Got connection'
                while True:
                    x = c.recv(1024)
                    print " s Recvd bytes ", len(x)

                    if x == '':
                        print ' s End of transmission' # for whatever reason
                        break
                
                c.close()

            except Exception, e:
                print " s Server error: %s" % e

class StemSetup:
    def run(self, myport, await = True):
        print(' * Connecting to tor controller')
        with Controller.from_port(port=CONTROL_PORT) as controller:
            try:
                controller.authenticate()
                print(' * Authenticated to controller, starting hs')
                
                if await:
                    response = controller.create_ephemeral_hidden_service({extport: myport}, await_publication = True)
                    print(" * Published service %s.onion" % response.service_id)
                else:
                    response = controller.create_ephemeral_hidden_service({extport: myport}, await_publication = False)
                    print(" * Created service %s.onion" % response.service_id)            
                    
                address = "%s.onion" % response.service_id

                cs = CoverSender(address)
                
                ps = Process(target=CoverSender.run, args=(cs,))

                print(" * Starting cover traffic sender")
                ps.start()
                
                ps.join()
                    
                print(" * Shutting down our hidden service")
                
                
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

def main():

    commq = Queue()
    cS = CoverServer(commq)
    pS = Process(target=CoverServer.run, args=(cS,))
    print(" * Starting server listener")
    pS.start()

    myport = commq.get()['myport']
    print(" * Server up at localport %d" % myport)
    cstem = StemSetup()
    pstem = Process(target=StemSetup.run, args=(cstem, myport, True))
    pstem.start()

    for process in [pS,pstem]:
        process.join()


if __name__ == "__main__":
    try:
        main()

    except KeyboardInterrupt, e:
        print "Got ctrl-c, exiting more or less gracefully"

