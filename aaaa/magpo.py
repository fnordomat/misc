#!/usr/bin/env python

import svgwrite
import random
import sys

if __name__=="__main__":

    random.seed()

    # TODO only initialize if it doesn't exist, don't overwrite
    my_svg = svgwrite.Drawing(filename = "words.svg", size = ("800px", "600px"))
    text_style = "font-size:%ipx; font-family:%s" % (12, "Courier New")

    for l in sys.stdin.readlines():
        for k in l[:-1].split(" "):
            x = random.randint(0,800)
            y = random.randint(0,600)
            text1 = my_svg.text(k,
                            insert=(x, y),
                            fill="black",
                            style=text_style)
            my_svg.add(text1)

    my_svg.save()

