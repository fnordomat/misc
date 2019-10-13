# misc
Miscellaneous handy and/or weird stuff.

## haskell
This folder contains stuff written in [haskell](https://www.haskell.org/). None of it is intended for production.

## rust
I like learning new languages. This folder contains stuff written in [rust](https://www.rust-lang.org).
- rust/dupes: find probable duplicate files by SHA256 - this one is actually useful. Moved to [its own repo](https://github.com/fnordomat/dupes)
- rust/ecc_test: trying out the Curve25519 / [Ristretto](https://ristretto.group/) implementation [curve25519_dalek](https://dalek.rs/) library [available here](https://github.com/dalek-cryptography/curve25519-dalek)
- rust/ot_poc: oblivious transfer protocol using RSA, as described on [Wikipedia](https://en.wikipedia.org/wiki/Oblivious_transfer)

## satsolver
A very simple, easy to understand DPLL SAT solver in C. It was written on an android device while waiting for the bus, etc. ... originally intended to golf it [IOCCC](http://ioccc.org/)-style.

## captive-be-gone
Automate once, save time every time. A growing collection of shell scripts which save the savvy user from ever having to undergo the ignominy of clicking through a captive portal again.
- captive-be-gone/WIFIonICE.sh: Deutsche Bahn ICE trains / icomera AB
- captive-be-gone/m3connect.sh: Accor group hotels / m3connect
- captive-be-gone/CDWiFi.sh:    Czech trains / ombord(?)

## cover
- cover/cover.py: Attempt to produce some cover traffic while using Tor.
Actually I'm not at all sure how useful this approach is. I think I've already spotted an obvious flaw.

## torx
- torx/CheckExit.py: collect tls certificates through tor (sometimes there are MitM attempts at exits and it's widely considered a good idea to document and [report](https://blog.torproject.org/blog/how-report-bad-relays) these). You might also be interested in other people's tools such as [exitmap](https://github.com/NullHypothesis/exitmap).

## trackography
Unique identifiers considered harmful.
- yourprobes/tea.py: a very simple script that reads probed ESSIDs aloud using text-to-speech. use case: get clueless users to acknowledge the dangers posed by excessive chattiness of their WIFI devices.

## emacs
Highlighting iptables rules.

