#!/usr/bin/env python

"""
Collects data (mainly certificates) for documentation of TLS MitM at Tor exits.
Does not by itself attempt to validate certificates or check for deviations.

Dependencies: sqlite3, stem, PySocks, PyOpenSSL
              antsy (for ANSI on terminal)
"""

#
# Initialize:
#
# python CheckExit.py -3 # (creates certs.db)
# sqlite3 certs.db '.dump'
#
# Use (for example):
#
# python CheckExit.py -H foobar.com
# python CheckExit.py -H jabber.foobar.com -q xmpp
#
# or with exits' ntor keys (found in microdescriptor, ends in =)
#
# python CheckExit.py -H foobar.com -O <key> -O <key> ...
#

#
# select exits.fingerprint, obs.success from exits inner join obs on (obs.exit_id = exits.id) where obs.success != 'yes' order by exits.id;
#

#
# This is far from finished.
#
# FIXME: we hit some timeouts after some connections. is it Tor refusing to make too many circuits? -> we ought to start our own Tor process and configure it appropriately.
# TODO: timeout earlier AND find the underlying problem
# TODO: we must also record failed attempts because of failed TLS/SSL handshakes! Could be downgrade attacks?
# TODO: InvalidRequest: Unknown circuit (reproduce this?)
# TODO: clean up on keyboardinterrupt
# TODO: investigate problem with other circuits than intended being destroyed? attachstream applies to circuits created by other applications?? -> ought to start own Tor process
# FIXME: don't use uniform random selection, use the normal mechanism and then just extend the circuit

import sys
import getopt
import os.path
import traceback
import logging
import random
import datetime

import socks
import socket
import OpenSSL
import sqlite3

import stem
from stem.descriptor import DocumentHandler, parse_file
from stem.descriptor.router_status_entry import *
from stem.descriptor.remote import DescriptorDownloader
from stem.control import Controller
from stem.util import conf, connection
from stem import CircuitExtensionFailed

import antsy

RANDOM = random.SystemRandom()

def usage():
    print "\nUsage:"
    print "%s [options]" %  sys.argv[0]
    print "--host / -H <host>          must be given"
    print "--port / -P <port>          defaults to default value of protocol"
    print "--protocol / -q <protocol>  if not given, TLS. valid options are xmpp"
    print "--ntor-onion-key / -O <ntor-onion-key>  use this exit. may be given more than once"
    print "--initdb / -3               use once to init database ./certs.db"
    print "--controlport / -p <port>   use this tor instance. defaults to 9051"
    print "--help / -h                 just show this help screen"

class Protocol:
  def __init__(self):
      pass
  def what(self):
      pass

class Protocol_TLS(Protocol):
  def __init__(self):
      Protocol.__init__(self)
      self.context = OpenSSL.SSL.Context(\
          OpenSSL.SSL.TLSv1_2_METHOD)
      self.context.set_timeout(10)
      def callback(connobj, x509obj, errno, errdepth, ret):
          return True
      self.context.set_verify(OpenSSL.SSL.VERIFY_PEER | OpenSSL.SSL.VERIFY_CLIENT_ONCE, callback)
      self.result = None
      self.host = None
      self.port = 443
  def what(self):
      return "TLS"
  def execute(self, soxsock, pre_tls=None):
      print "Socket Connect ..."
      soxsock.connect((self.host, self.port))
      if not (pre_tls == None):
          pre_tls.execute()
      sslsock = OpenSSL.SSL.Connection(self.context, soxsock)
      sslsock.set_connect_state()
      print "Doing TLS Handshake ..."
      sslsock.do_handshake()
      print "Cipher list starts with %s ..." % sslsock.get_cipher_list()[:5]
      print "Extracting Certificate ..."
      peercert = sslsock.get_peer_certificate()#_cert_chain()
      sslsock.close()
      self.result = peercert
      return self.result

class Protocol_StartTLS_XMPP(Protocol):
  def __init__(self):
      Protocol.__init__(self)
      self.port = 5222
      self.result = None
      self.host = None
      self.sslprot = Protocol_TLS()
  def what(self):
      return "XMPP/STARTTLS"
  def execute(self, soxsock):
      class starttls():
          def __init__(self, host, sock):
              self.host = host
              self.sock = sock
          def execute(self):
              sock = self.sock
              print "Greeting XMPP server ..."
              # the to= ... chooses a virtual host, apparently
              sock.sendall("<stream:stream xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams' to='%s' version='1.0'>" % self.host)
              print "Doing STARTTLS ..."
              def recv_all(sock):
                  r = ""
                  while 1:
                      data = sock.recv(4096)
                      if not data: break
                      r += data
                      if (len(data)< 4096): break
                  return r
              r = recv_all(sock)
              print r
#              sock.sendall("<starttls xmlns=\"urn:ietf:params:xml:xmpp-tls\"/>")
              sock.sendall("<starttls xmlns=\"urn:ietf:params:xml:ns:xmpp-tls\"/>")
              r = recv_all(sock)
              print r
      self.sslprot.host = self.host
      self.sslprot.port = self.port
      self.result = self.sslprot.execute(soxsock, pre_tls=starttls(self.host, soxsock))
      return self.result

def try_connect(controller, circstart, myexit, dbconn, protocol):
#    logging.debug("Attempting connection via %s " % myexit)
    print "Attempting connection to %s:%d via exit %s... (exit policy %s) " % (protocol.host, protocol.port, myexit.digest[:6], myexit.exit_policy)

    cur = dbconn.cursor()

    path = circstart + [myexit.ntor_onion_key]

    circuit_id = controller.new_circuit(path, await_build=True)

    def attach_stream(stream):
        if stream.status == 'NEW':
            controller.attach_stream(stream.id, circuit_id)

    controller.add_event_listener(attach_stream, stem.control.EventType.STREAM)

    try:
        socksport = int(controller.get_conf("SocksPort").split(" ")[0])
        controller.set_conf('__LeaveStreamsUnattached', '1')

        socks.setdefaultproxy(socks.PROXY_TYPE_SOCKS5, '127.0.0.1', socksport)

        soxsock = socks.socksocket()
#        soxsock.settimeout ( 20 )

        try:
            print "Saving Connection Info ..."
            # can clean up later if interrupted
            # select exits.id from exits where not exists (select * from obs where obs.exit_id = exits.id);
            cur.execute("SELECT (id) FROM exits WHERE (fingerprint==?)", (myexit.ntor_onion_key,))
            idu = cur.fetchone()
            if (idu == None):
                cur.execute("INSERT INTO exits (fingerprint, microdescriptor) VALUES (?,?)", (myexit.ntor_onion_key,str(myexit)))
                cur.execute("SELECT (id) FROM exits WHERE (fingerprint==?)", (myexit.ntor_onion_key,))
                idu = cur.fetchone()
                print "Inserted new entry into EXITS table"
            else:
                print "Found existing entry in EXITS table"

            protocol.execute(soxsock)

            peercert = protocol.result

            pem = OpenSSL.crypto.dump_certificate(OpenSSL.crypto.FILETYPE_PEM,peercert)

            digest = peercert.digest("sha1")

            print "Saving Certificate Info ..."

            cur.execute("SELECT (id) FROM certs WHERE (digest==?)", (digest,))
            idc = cur.fetchone()
            if (idc == None):
                cur.execute("INSERT INTO certs (digest, x509) VALUES (?,?)", (digest,str(pem)))
                cur.execute("SELECT (id) FROM certs WHERE (digest==?)", (digest,))
                idc = cur.fetchone()

                msg = "Inserted new entry into CERTS table"
                if antsy.supports_color():
                    print antsy.fg(msg, "red")
                else:
                    print msg

            else:
                msg = "Found existing entry in CERTS table"
                if antsy.supports_color():
                    print antsy.fg(msg, "blue")
                else:
                    print msg

            cur.execute("INSERT INTO obs (what, host, port, cert_id, exit_id, success, recorded) VALUES (?,?,?,?,?,?,?)", (protocol.what(), protocol.host, protocol.port, idc[0], idu[0], "yes", datetime.datetime.utcnow()))

            dbconn.commit()

        except OpenSSL.SSL.Error as excuse:
            cur.execute("INSERT INTO obs (what, host, port, exit_id, success, recorded) VALUES (?,?,?,?,?,?)", (protocol.what(), protocol.host, protocol.port, idu[0], "no: " + str(excuse), datetime.datetime.utcnow()))
            dbconn.commit()
            print excuse
            # results like
            # (-1, "Unexpected EOF")
            # [('SSL routines', 'SSL3_GET_RECORD', 'wrong version number')]

        soxsock.close()

    except socks.GeneralProxyError as e: # timeout
        print e

    finally:
        controller.remove_event_listener(attach_stream)
        controller.reset_conf('__LeaveStreamsUnattached')

    controller.close_circuit(circuit_id)

def initdb():
    conn = sqlite3.connect('certs.db')
    conn.execute('CREATE TABLE EXITS (id integer PRIMARY KEY, fingerprint TEXT NOT NULL, microdescriptor TEXTNOT NULL)')
    conn.execute('CREATE TABLE CERTS (id integer PRIMARY KEY, digest TEXT NOT NULL, x509 TEXT)')
    conn.execute('CREATE TABLE OBS (id integer PRIMARY KEY, what TEXT, host TEXT NOT NULL, port INTEGER NOT NULL, cert_id INTEGER, exit_id INTEGER NOT NULL, success TEXT, recorded DATE)')
    conn.commit()

def main(controller, protocol, chosen_exits=None):
    conn = sqlite3.connect('certs.db')

#    descriptors = [ x for x in parse_file(consensuspath, validate=True) ]

    # FIXME: we should rely on the normal circuit creation mechanism instead
    try:
        print "Getting microdescriptors from Tor ..."
        descriptors = [ x for x in controller.get_microdescriptors() ]
            
        exits = [ x for x in descriptors
                if x.exit_policy.is_exiting_allowed() ]

        valid_exits = [ x for x in exits
                      if x.exit_policy.can_exit_to(port = protocol.port) ]

        print "Found %d relays, %d seem to be exits, %d exit to port %d" % (len(descriptors), len(exits), len(valid_exits), protocol.port)

        if chosen_exits:
            sample = [ x for x in exits if x.ntor_onion_key in chosen_exits ] # check if still up?
        else:
            HOWMANY = 13
            sample = RANDOM.sample(valid_exits, HOWMANY)

        circstart = [ x.ntor_onion_key for x in RANDOM.sample(descriptors, 2) ]

        for it in sample:
            try:
                try_connect(controller, circstart, myexit=it, dbconn=conn, protocol=protocol)
            except CircuitExtensionFailed as e:
                print e
        
    except Exception as e:
        print e

    conn.commit()
    conn.close()

if __name__ == '__main__':
  try:
    host = None
    port = None
    chosen_exits = []
    controlport = 9051
    protocol = Protocol_TLS()
    # -R number of random exits
    opts, args = getopt.getopt(sys.argv[1:], "3hH:P:p:q:O:", ["initdb", "help", "host=", "port=", "controlport", "protocol", "ntor-onion-key=", "exit="])
    for o, a in opts:
        if o in ("-O", "--ntor-onion-key", "--exit"):
            chosen_exits += [a]
        if o in ("-q", "--protocol"):
            if (a == "xmpp"):
                protocol = Protocol_StartTLS_XMPP()
        if o in ("-3", "--initdb"):
            initdb()
        if o in ("-h", "--help"):
            usage()
            sys.exit()
        elif o in ("-p", "--controlport"):
            controlport = int(a)
        elif o in ("-H", "--host"):
            if ":" in a:
                host = a.split(":")[0]
                port = int(a.split(":")[1])
            else:
                host = a
        elif o in ("-P", "--port"):
            port = int(a)
    if (host == None):
      print "No host given."
      usage()
      sys.exit(1)

    controller = stem.control.Controller.from_port(port = controlport)
    controller.authenticate()

    if not(port == None):
        protocol.port = port

    protocol.host = host    

    if (chosen_exits == []):
        chosen_exits = None
    main(controller=controller, protocol=protocol, chosen_exits=chosen_exits)
  except getopt.GetoptError as err:
    print(err)
    usage()
    sys.exit(2)
  except SystemExit as err:
    pass
  except:
    msg = "failed with:\n\n%s" % traceback.format_exc()
    logging.error(msg)
