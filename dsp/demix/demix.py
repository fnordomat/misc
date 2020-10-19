#!/usr/bin/env python3

# Attempt very simple 2-signal separation (demixing) of a stereo .wav file.
# https://en.wikipedia.org/wiki/Signal_separation


import scipy
from os.path import dirname, join as pjoin
from scipy.io import wavfile
import scipy.io
import scipy
import scipy.optimize
import math
import numpy as np


class Optipro:
    def __init__(self, wave):
        self.wave = wave
    def combi1(self, p1, p2):
        return math.cos(p1) * self.wave[:,0] + math.sin(p1) * self.wave[:,1]
    def combi2(self, p1, p2):
        return math.sin(p2) * self.wave[:,0] + math.cos(p2) * self.wave[:,1]

    def score(self, p1, p2):
        combination1 = self.combi1(p1, p2)
        combination2 = self.combi2(p1, p2)
        p12 = np.dot(combination1, combination2)
        return abs(p12)


def main(wav_fname):
    samplerate, data = wavfile.read(wav_fname, mmap=True)
    nchan = data.shape[1]
    length = data.shape[0] / samplerate
    print(f"channels = {nchan} length = {length}s")

    data = data.astype(np.float32)
    datax = data[:]

    op = Optipro(data)

    import matplotlib.pyplot as plt

    xs = np.linspace(0, math.pi, 20)
    ys = np.linspace(0, math.pi, 20)

    # https://stackoverflow.com/questions/22774726/numpy-evaluate-function-on-a-grid-of-points
    def f(x, y):
        return op.score(x, y)

    X, Y = np.meshgrid(xs, ys)
    # print([X, Y].reshape)
    Z = np.fromiter(map(f, X.ravel(), Y.ravel()), X.dtype).reshape(X.shape)

    plt.contourf(X, Y, Z, 64, alpha=.75, cmap='jet')
    contours = plt.contour(X, Y, Z, 4, colors='black')
    plt.clabel(contours, inline=True, fontsize=8)
    plt.show()

    z = scipy.optimize.minimize(lambda x: op.score(x[0], x[1]), [0.1, 0.2],
                                method="BFGS",
                                options={'finite_diff_rel_step':1e-4, 'norm':2},
                                jac='3-point')
    print(z)

    p1 = z.x[0]
    p2 = z.x[1]

    
    print([p1, p2])
    print("at 0,0: ", op.score(0.0, 0.0))
    print(op.score(p1, p2))

    putsol0 = op.combi1(p1, p2)
    putsol1 = op.combi2(p1, p2)

    morigl = math.sqrt(np.dot(data[:,0], data[:,0]))
    morigr = math.sqrt(np.dot(data[:,1], data[:,1]))
    morig = (morigl + morigr)/2.0
    n0 = math.sqrt(np.dot(putsol0, putsol0))
    n1 = math.sqrt(np.dot(putsol1, putsol1))
    print(morigl, morigr, morig, n0, n1, morig/n0, morig/n1)

    putsol0 *= morig/n0
    putsol1 *= morig/n1

    m0 = max(abs(putsol0))
    m1 = max(abs(putsol1))
    putsol0 /= m0
    putsol1 /= m1

    scipy.io.wavfile.write("source0.wav", samplerate, putsol0)
    scipy.io.wavfile.write("source1.wav", samplerate, putsol1)


if __name__=="__main__":
    import sys
    filename = "input.wav"
    if len(sys.argv) > 1:
        filename = sys.argv[1]
    main(filename)
