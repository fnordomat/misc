#!/usr/bin/python3

import subprocess     
import re               
import lxml  
from lxml import html 
import urllib.request  
import urllib3                                 
                                      
headers = {}
headers['User-Agent'] = 'Mozilla/5.0'
pool = urllib3.PoolManager()       
dummy='google.com'

f1 = pool.request(method='GET', url=dummy, headers=headers)
print("%s" % f1.getheaders())
htmr = f1.data
hdoc = html.document_fromstring(htmr)

inputs = hdoc.xpath('//input') 
kvs = [(elt.attrib.get('name'), elt.attrib.get('value')) for elt in inputs if elt.attrib.get('type') == 'hidden']
chk = [(elt.attrib.get('name'), "checked") for elt in inputs if elt.attrib.get('type') == 'checkbox']

forms = hdoc.xpath('//form') 
href = forms[0].attrib.get('action')

fields = dict(kvs)
fields[chk[0][0]] = chk[0][1]

print(fields)
print(href)

f3  = pool.request_encode_body(method='POST', url=href, headers=headers, fields=fields)
print(f3.getheaders())
print(f3.data)
