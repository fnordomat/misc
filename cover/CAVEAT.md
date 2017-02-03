# CAVEAT

Generate some superficial cover traffic over tor. The idea is that your tor client should not sit idle when you're not active, immediately betraying that you're not active.

Problem is that traffic correlation attacks can be very sophisticated and just adding some bursts of cover traffic doesn't help against all of them. If different entry nodes are used for the cover vs. real circuits, it doesn't help at all. however, only a few guards are normally used at a time (true?) Then it may help obfuscate things a little.

It's just an idea. Don't get lulled into a false sense of security. We're massively outgunned by a lawless cabal of immensely dangerous global adversaries, never forget that.

+ It won't work because same amount of data going in and out is highly suspicious. Remote hidden services must be used. **(TODO)**


## carefully examine existing research

https://blog.torproject.org/blog/traffic-correlation-using-netflows

https://security.stackexchange.com/questions/147402/how-do-traffic-correlation-attacks-against-tor-users-work

https://www.nrl.navy.mil/itd/chacs/sites/edit-www.nrl.navy.mil.itd.chacs/files/pdfs/13-1231-2077.pdf

https://www.freehaven.net/anonbib/cache/wpes11-panchenko.pdf

http://www.hamilton.ie/saman/ieee-icc15.pdf

 - matching packet timestamp sequences may be possible even when multiple connections are present

## more research needed

