# misc
Miscellaneous stuff, ranging from handy to useless.

## aaaa
This folder contains:
- aaaa/magpo.py: turn a text or word list into virtual [magnetic poetry](https://en.wikipedia.org/wiki/Magnetic_Poetry). The idea is to open the resulting random SVG in an interactive editor such as [inkscape](https://inkscape.org/) and play with the words on the screen.

## haskell
This folder contains stuff written in [haskell](https://www.haskell.org/). None of it is intended for production.
- haskell/comprviz: compresses input text file with gzip and outputs a HTML file where the background color of each character depends on the coding efficiency (red background = high number of bits, green = low number, blue channel indicates whether a backreference was used)
- haskell/cyk: Cocke-Younger-Kasami like algorithm for experiments with context-free languages.
- haskell/editdistviz: visualisation of edit distance (the [Levenshtein string metric](https://en.wikipedia.org/wiki/Levenshtein_distance)). the distance between two words is computed and the intermediary results are shown in a beautiful diagram. Output format is SVG.

## rust
I like learning new languages. This folder contains stuff written in [rust](https://www.rust-lang.org).
- rust/dupes: find probable duplicate files by SHA256 - this one is actually useful. Moved to [its own repo](https://github.com/fnordomat/dupes)
- rust/ecc_test: trying out the Curve25519 / [Ristretto](https://ristretto.group/) implementation [curve25519_dalek](https://dalek.rs/) library [available here](https://github.com/dalek-cryptography/curve25519-dalek)
- rust/ot_poc: oblivious transfer protocol using RSA, as described on [Wikipedia](https://en.wikipedia.org/wiki/Oblivious_transfer). Disclaimer: I'm not a professional cryptographer! This might be all wrong.

## scheme
This folder contains stuff written in scheme, a classical, extremely flexible, dynamically typed family of programming languages.
- scheme/wb.scm: [Weight-Balanced Trees](https://en.wikipedia.org/wiki/Weight-balanced_tree) implementation in scheme, and a proof of concept unit test facility (works with [chicken](https://www.call-cc.org) but unfortunately define-syntax macros aren't part of the standard).

## satsolver
A very simple, easy to understand DPLL SAT solver in C. It was written on an android device while waiting for the bus, etc. ...

## captive-be-gone
A growing collection of shell scripts which save me from ever having to undergo the ignominy of clicking through a captive portal again. Caution: work in progress, unsanitized input might be used in unsafe ways. You've been warned.
- captive-be-gone/WIFIonICE.sh: Deutsche Bahn ICE trains / icomera AB
- captive-be-gone/CDWiFi.sh:    Czech trains / ombord(?)
- captive-be-gone/wifipass.py:  "wifipass.org"
- captive-be-gone/m3connect.sh: Accor group hotels / m3connect (works for some m3connect networks only)
- captive-be-gone/m34.py:       Accor group hotels / m3connect (works in Leipzig)

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

