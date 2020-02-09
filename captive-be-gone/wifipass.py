#!/usr/bin/env python3

# curl --stderr - --trace - -v -L -A '' http://foobar.com | tee step0001
# # -> the href in there, including the mac address
# curl --stderr - --trace - -v -L -A '' -c kookie.jar "https://passman01.wifipass.org/w2p/login-url-real.php?id=BLABLA&domain=controleur.wifipass.org&mac=XX-XX-XX-XX-XX-XX&page=http%3A%2F%foobar.com%2F" | tee step0002
# # -> the _token value in there
# curl --stderr - --trace - -v -L -A '' -c kookie.jar 'https://passman01.wifipass.org/w2p/formulaire_fin.php?id=BLABLA&domain=controleur.wifipass.org' -F "registration[id1]=1" -F "registration[id2]=243" -F "registration[newsLetterType]=FOOFOO" -F "registration[email]=foo@bar.com" -F "_token=XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX" -F "accept=true"  --post301 --post302 --post303  | tee step0003
# # took this path again, ended in a cycle:
# # curl --stderr - --trace - -v -L -A '' -c kookie.jar 'https://passman01.wifipass.org/w2p/formulaire_fin.php?id=BLABLA&domain=controleur.wifipass.org' -F "username=ZZZZZZZZZZ" -F "password=WWWWWW"  -F "_token=YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY"  --post301 --post302 --post303  | tee step0004
# # => a different token value, go figure:
# curl --stderr - --trace - -v -L -A '' -c kookie.jar 'https://controleur.wifipass.org/goform/HtmlLoginRequest' -F "username=ZZZZZZZZZZ" -F "password=WWWWWW"  -F "_token=YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY"  --post301 --post302 --post303  | tee step0005

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
print("%s", f1.getheaders())
htmr = f1.data
hdoc = html.document_fromstring(htmr)

a = hdoc.xpath('//a[@href]')[0]
href = a.attrib.get('href')

f0 = pool.request(method='GET', url=href, headers=headers)
print("%s", f0.getheaders())
rd = f0.data
hdoc2 = html.document_fromstring(rd)

inputs = hdoc2.xpath('//input') 
kvs = [(elt.attrib.get('name'), elt.attrib.get('value')) for elt in inputs if elt.attrib.get('type') != 'checkbox']
chk = [(elt.attrib.get('id'), elt.attrib.get('checked')) for elt in inputs if elt.attrib.get('type') == 'checkbox']

print(kvs, chk)
fields = dict(kvs + chk)
rem = []
for k in fields:
    if fields.get(k) == None:
        if re.search('email', k):
# this appears to be required
            fields[k] = "foo@bar.com"
        else:
            rem += [k]
# also works:           fields[k] = '0'
for k in rem:
    fields.pop(k)

forms = hdoc2.xpath('//form') 

pr = urllib.request.urlparse(href)

actions = [f.action for f in forms]
newreqs = []

for a in actions:
    pf = urllib.request.urlparse(a)
    if pf.scheme == '' and pf.netloc == '':
        newreq = pr.scheme + '://' + pr.netloc + '/' + \
          re.match('(.*?)/[^/]*', pr.path)[0] + '/' + \
          pf.path + '?' + pf.query 
        newreqs += [newreq]
    else:
        newreqs += [pf.scheme + '://' + pf.netloc + pf.path]

a2s = hdoc2.xpath('//a[@href]')
loginx = [a2.attrib.get('href') for a2 in a2s][0]

loginrequest = newreqs[0]

print(loginrequest, fields)

f3a = pool.request_encode_body(method='POST', url=newreqs[1], headers=headers, fields=fields)
print(f3a.getheaders())
print(f3a.data)
hdoc3a = html.document_fromstring(f3a.data)
inputs3a = hdoc3a.xpath('//input') 
kvs3a = [(elt.attrib.get('name'), elt.attrib.get('value')) for elt in inputs3a if elt.attrib.get('type') != 'checkbox']
fields3a = dict(kvs3a)

rem = []
for k in fields3a:
    if fields3a.get(k) == None:
        rem += [k]
for k in rem:
    fields3a.pop(k)

f3  = pool.request_encode_body(method='POST', url=loginrequest, headers=headers, fields=fields3a)
print(f3.getheaders())
print(f3.data)

# hdoc3 = html.document_fromstring(f3.data)
# we don't need the contents, except optionally to make sure that it worked.
# print(f3.data)
