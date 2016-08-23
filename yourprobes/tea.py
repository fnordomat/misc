#!/usr/bin/env python2

#
# Reads the ESSIDS from probe requests aloud, for the shock value.
#

# Requirements: A wifi driver that supports monitor mode.
# python2, festival, tcpdump, a tool for channel hopping

# Acknowledge the true character of deceptively imperceptible technology!

# Sending out probe requests all the time in client mode doesn't even make much sense from an engineering standpoint. It's a gratuitously dopey standard that serves no discernible purpose except for wasting battery power and making you trackable. AP beacons should suffice!

import subprocess
import re
import os
import sys
import prepoui

import threading
from threading import Thread

basedir = "."

IFACE = "mon0"
THRESHOLD = -65

def install_lexrules(sayer):
    sayer.stdin.write("""
    (lex.add.entry '( "wifi" n ((( w ay ) 1) (( f ay ) 0))) )\n
    """)
    sayer.stdin.flush()

class X:
    def launchpcap(self):
        self.pcap = subprocess.Popen(["sudo", "tcpdump", "-evnl", "-I", "-i", IFACE, "type mgt subtype probe-req"], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        while (1):
            line = self.pcap.stdout.readline()
            if line == '':
                break
            self.process(line)

    def process(self, line):
        xre = r'([^\s]+) ([^\s]+) tsft (?:short preamble )?([^\s]+) Mb/s ([^\s]+) MHz ([^\s]+) ([^\s]+)dB signal (?:antenna )?.* BSSID:([^\s]+) DA:([^\s]+) SA:([^\s]+) (.+)'
        try:
            m = re.match(xre, line).groups()
            time = m[0]
            hhmmss = m[0][0:8]
            us = m[1]
            freq = m[2]
            dB = m[5]
            bssid = m[6]
            da = m[7]
            sa = m[8]
            ouisa = m[8][0:2]+m[8][3:5]+m[8][6:8]
            sarest = m[8][9:11]+m[8][12:14]+m[8][15:17]
            manusa = self.t.definitive(ouisa)
            what = m[9]
            m2 = re.match(r'Probe Request \((.*)\)', what)
            if m2:
#                subprocess.Popen(["aplay", "alert.wav"], stderr=subprocess.PIPE, stdout=subprocess.PIPE)
                name = m2.groups()[0]
                if (sa, name) in self.seen:
                    try:
                        dbval = int(dB)
                        if dbval > self.seendB[sa]:
                            self.seendB[sa] = dbval
                            if dbval > THRESHOLD:
                                print (manusa,sarest,name,dB), "approaching"
                    except ValueError, ve:
                        print ve
                else:
#                    subprocess.Popen(["aplay", "alert.wav"], stderr=subprocess.PIPE, stdout=subprocess.PIPE)
                    self.seen.add((sa, name))
                    try:
                        dbval = int(dB)
                        self.seendB[sa] = dbval
                    except ValueError, ve:
                        print ve
                    print (manusa, sarest, name, dB)
                    if name != '':
                        self.say(name)

        except AttributeError, ae:
            print ae
            print line

    def say(self, name):
        escaped = name.replace('"', '\\"')
        self.sayer.stdin.write("(SayText \"%s\")\n" % escaped) 
        self.sayer.stdin.flush()
    def launchsayer(self):
        self.sayer = subprocess.Popen(["festival"], stdin=subprocess.PIPE, stderr=subprocess.PIPE, stdout=subprocess.PIPE)
        install_lexrules(self.sayer)
    def __init__(self):
        try:
            self.t = prepoui.Thingum() # DB conn
            self.seen = set() # already seen probes
            self.seendB = {}  # signal strengths of remembererd probes
        except KeyboardInterrupt, ki:
            print ki
            sys.exit(0)
        except Exception, e:
            print e
            sys.exit(-1)

    def run(self):
        try:
            self.launchsayer()
            self.launchpcap()
        except KeyboardInterrupt, ki:
            print ki
            sys.exit(0)
        except Exception, e:
            print e
            sys.exit(-1)

if __name__ == "__main__":
    """
    Squat.
    """

    x = X()
    x.run()
