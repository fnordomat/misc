#!/usr/bin/env python2

# prepare the index of numbers formerly known as OUIs (or was it NONs?)
# which are allocated to vendors

import re
import sys
import sqlite3

# F  = "/etc/aircrack-ng/oui.txt"
F  = "/usr/share/ieee-data/oui.txt"

# create / use this file in the current directory
DB = "manufacturers.db"

# Catchier names.
def normalize_stupid_manufacturer_designation(name):
    if re.match(r'Apple', name, re.I):
        return "Crapple"
    if re.match(r'Microsoft', name, re.I):
        return "Microshaft"
    if re.match(r'Huawei', name, re.I):
        return "Huawei"
    # Intel inside: the most creepily accurate marketing claim
    if re.match(r'Intel', name, re.I):
        return "Intel"
    if re.match(r'Sony', name, re.I):
        return "Sony"
    if re.match(r'Murata', name, re.I):
        return "Murata"
    if re.match(r'shenzhen', name, re.I):
        return "China-Plaste"
    if re.match(r'Texas Instruments', name, re.I):
        return "TI"
    if re.match(r'Liteon', name, re.I):
        return "Liteon"
    if re.match(r'^LG', name, re.I):
        return "LG"
    if re.match(r'^HTC', name, re.I):
        return "HTC"
    if re.match(r'Samsung', name, re.I):
        return "Samsung"
    return "Generic"

class Thingum:
    def connect_db(self):
        con = sqlite3.connect(DB)
        con.row_factory = sqlite3.Row
        return con

    def definitive(self, ouisa):
        manusa = self.regurgitate(ouisa)
        if manusa:
            manusa_definitive = \
                normalize_stupid_manufacturer_designation(manusa[0]['name'])
            return manusa_definitive
        else:
            return ouisa + "?"

    def regurgitate(self, oui):
        con = self.connect_db()
        c = con.cursor()
        
        c.execute("SELECT name FROM manu WHERE oui = ?", (oui,))
        r = c.fetchall()

        return r

    def prep(self, filename):
        file = open(filename)
        con  = self.connect_db()
        c = con.cursor()
        try:
            c.execute("""CREATE TABLE manu (oui STRING(6),
		     name VARCHAR(255))""")
        except Exception, e:
            print e

        xre = r'([0-9A-Fa-f][0-9A-Fa-f]).([0-9A-Fa-f][0-9A-Fa-f]).([0-9A-Fa-f][0-9A-Fa-f])\s*\(hex\)\s*(.*)'
        for line in file.readlines():
            print line
            try:
                m = re.match(xre, line).groups()
                (x, y, z) = (m[0], m[1], m[2])
                name = m[3]
                c = con.cursor()
                c.execute("INSERT INTO manu VALUES (?,?)",
                          ((x+y+z).lower(), name))
                print m
            # if the oui.txt contains superfluous lines
            # error messages are expected. ignore them.
            except AttributeError, ae:
                print ae
            except Exception, e:
                print e
        con.commit()

if __name__=="__main__":
    t = Thingum()
# first uncomment this
#    t.prep(F)
# afterwards this
    print t.definitive(sys.argv[1])

