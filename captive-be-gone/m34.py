#!/usr/bin/env python3

import subprocess
import re
import lxml
from lxml import html
import urllib
import urllib.request
import urllib3
import base64

class Foo:
    def __init__(self):
        self.headers = {}
        self.headers['User-Agent'] = 'Mozilla/5.0'
        self.pool = urllib3.PoolManager()
        self.cookies = {}

    def perform_xhr_post(self, url, extra_headers=[], fields={}):
        pool = self.pool
        headers = self.headers.copy()
        headers['X-Requested-With'] = 'XMLHttpRequest'
        for (k,v) in extra_headers:
            headers[k] = v
        xs = list(self.cookies.items())
        headers['Cookie'] = "; ".join(["%s=%s" % (k,v) \
                                       for (k,v) in xs \
                                       if k != 'PHPSESSID'])

        print("request POST %s" % url)
        print("request headers: %s" % headers)
        print("request fields: %s" % fields)

        f = pool.request_encode_body(method='POST',
                                     url=url,
                                     fields=fields,
                                     encode_multipart=False,
                                     headers=headers,
                                     redirect=False)
        return self.postprocess(f)

    # called by perform_get and perform_xhr_post after executing the request
    # f = the request object
    def postprocess(self, f):
        print("response headers: %s" % f.getheaders())

        if f.get_redirect_location():
            print("redirect: %s" % f.get_redirect_location())

        for cookie1 in f.getheaders().get_all('Set-Cookie'):
            cookies1 = re.findall('([^= ]*)=([^; ]*); ', cookie1)
            for (k,v) in cookies1:
                # what's the exact syntax? we don't really care. just remove the accidentally captured strings
                if not (re.match('[Mm]ax-[Aa]ge', k)  or re.match('path', k)):
                    if self.cookies.get(k):
                        print("replacing cookie %s with %s" % (k, v) )
                    else:
                        print("setting cookie %s to %s" % (k, v) )

                    if 'himalaya-site-ident' == k:
                        print("himalaya-site-ident: ",base64.b64decode(v.replace('%3D', '=')))

                    self.cookies[k] = v
        print(f.data)
        print()
        return f

    def perform_get(self, url, redirect=False):
        pool = self.pool
        headers = self.headers.copy()

        headers['Cookie'] = "; ".join(["%s=%s" % (k,v) for (k,v) in self.cookies.items()])

        print("request GET %s" % url)
        print("request headers: %s" % headers)
        f = pool.request(method='GET', url=url, headers=headers, redirect=redirect)

        return self.postprocess(f)        

print("let's go")

foo = Foo()

dummy='http://detectportal.firefox.com/success.txt'
f1 = foo.perform_get(dummy)

url = f1.get_redirect_location()
f2 = foo.perform_get(url)

url = f2.get_redirect_location()
f2 = foo.perform_get(url)

f2_headers = {k.lower(): v for k, v in f2.getheaders().items()}
url = f2_headers.get('location')
f3 = foo.perform_get(url)

tokenstring = re.findall(b'"token":"([^"]+)"', f3.data)[0]
scenestring = re.findall(b'"id":"([^"]+)"', f3.data)[0]

sceneurl = b"https://accor.conn4.com/scenes/%s/" % scenestring
# WTF, shouldn't urllib3 be using "bytes" everywhere????
url = sceneurl.decode()
f4aa = foo.perform_get(url)

headers4 = {}
# headers['Referer'] = 'https://accor.conn4.com/'
headers4['Referer'] = sceneurl.decode()
headers4['Cookie'] = "; ".join(["%s=%s" % (k,v) for (k,v) in foo.cookies.items()])
headers4['X-Requested-With'] = 'XMLHttpRequest'
headers4['Origin'] = 'https://accor.conn4.com'
# if we made a mistake before arriving here, the server uses this value for Allow-Origin instead:
# headers4['Origin'] = 'https://portal-eu-ffm01.conn4.com'
headers4['Accept'] = '*/*'
# unnecessary:
# headers4['Cache-Control'] = 'no-cache'
# headers4['Accept-Language'] = 'en-US,en;q=0.5'
# headers4['Connection'] = 'keep-alive'
headers4['Content-Type'] = 'application/x-www-form-urlencoded; charset=UTF-8'
headers4['Host'] = 'accor.conn4.com'

url = 'https://accor.conn4.com/wbs/api/v1/create-session/'
form = {}
form['session_id'] = ''
form['with-tariffs'] = '1'
form['locale'] = 'en_US'
form['authorization'] = "token=%s" % tokenstring.decode()
f4a = foo.perform_xhr_post(url, extra_headers=list(headers4.items()), fields=form)
sessions = re.findall(b'"session":"([^"]+)"', f4a.data)[0].decode()
print(sessions)
# oh, it's the same as the PHPSESSIONID. go figure

url = 'https://accor.conn4.com/_time?t=1577515801949'
foo.perform_xhr_post(url)
# "get" in original trace, but "post" works fine

# firefox did this, but it's unnecessary (maybe this is the case where mac address hasn't changed)
# url = 'https://accor.conn4.com/wbs/api/v1/login/free/'
# form = {}
# form['authorization'] = 'session=%s' % sessions
# f4 = foo.perform_xhr_post(url, extra_headers=list(headers4.items()), fields=form)

url = 'https://accor.conn4.com/wbs/api/v1/register/free/'
form = {}
form['authorization'] = 'session=%s' % sessions
form['registration_type'] = 'terms-only'
form['registration[terms]'] = '1'
f5 = foo.perform_xhr_post(url, extra_headers=list(headers4.items()), fields=form)
