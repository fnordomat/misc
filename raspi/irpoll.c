/**
 * Receive and "decode" some TV remote control signals using TSOP48xx
 *
 * Single-threaded operation, attempting decoding and matching every time a pause
 * of 30ms between signal toggles is detected.
 *
 * "decoding" = binning toggles into periods of 1/38000s, registering a "1" if
 * the signal went or remained high and a "0" if it went or remained low during
 * the period.
 *
 * "matching" = string comparison :-)
 *
 */

#define GPIO4_VALUE "/sys/class/gpio/gpio4/value"
#define SYSVALUE_BUFSIZE 1
#define INPUT_BUFSIZE 1024

// # set up GPIO pins (POUT 18 for status check LED, PIN 4 connected to TSOP48xx's output)
// echo 4 > /sys/class/gpio/export 
// echo in > /sys/class/gpio/gpio4/direction
// echo 18 > /sys/class/gpio/export 
// echo out > /sys/class/gpio/gpio18/direction 

// # compile
// gcc irpoll.c -o irpoll

// # do something with the output, e.g.
// # pipe (turning off pesky buffering)
// stdbuf -i0 -o0 -e0 ./irpoll | stdbuf -i0 -o0 -e0 egrep --color=never --line-buffered ^K > /root/fifo
// # from remote system:
// ssh -6 fe80::[remainder of local address]%[network iface] -l root 'cat /root/fifo'

#include <fcntl.h>
#include <stdio.h>
#include <sys/poll.h>
#include <unistd.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

// TOSHIBA CT-90326
const char RED[]      = "##############_______#_#_#_#_#_#_#___#_#___#___#___#___#___#___#_#___#_#_#_#___#_#_#___#_#___#___#___#_#___#___#_#___#";
const char GREEN[]    = "##############_______#_#_#_#_#_#_#___#_#___#___#___#___#___#___#_#___#___#_#_#___#_#_#___#_#_#___#___#_#___#___#_#___#";
const char VOL_UP[]   = "##############_______#_#_#_#_#_#_#___#_#___#___#___#___#___#___#_#___#_#___#_#___#___#_#_#_#___#_#___#_#_#___#___#___#";
const char VOL_DOWN[] = "##############_______#_#_#_#_#_#_#___#_#___#___#___#___#___#___#_#___#_#___#___#___#___#_#_#_#___#_#_#_#_#___#___#___#";
const char VOL_MUTE[] = "##############_______#_#_#_#_#_#_#___#_#___#___#___#___#___#___#_#___#_#_#_#_#___#_#_#_#___#___#___#___#_#___#___#___#";


void try_finalize(const double timings[], size_t* mark1, size_t* mark2) {

    for (size_t k = *mark1; k != *mark2; k = (k + 1) % INPUT_BUFSIZE) {
        if (timings[k] > 50000 && (1024 + *mark2 - k) % INPUT_BUFSIZE > 1) {
            *mark1 = k + 1;
        }
    }

    double process[INPUT_BUFSIZE] = {0};
    size_t howmany = (1024 + *mark2 - *mark1) % INPUT_BUFSIZE - 1;
    memcpy(process, timings + *mark1, howmany);

    double sum = 0.0;
    for (size_t i = 0; i < howmany; ++i) {
        sum += timings[(*mark1 + i) % INPUT_BUFSIZE];
    }

    if (howmany > 10) {

        char sel[1024] = {0};

        int u = 1;
        size_t opos = 0;
#define KU (2.631578e1)
        for (size_t i = 0; i < howmany; ++i) {
            double val = timings[(*mark1 + i) % INPUT_BUFSIZE];
            for (double c = 0.0; c < val / KU; c += KU) {
                printf("%s", u == 1 ? "#" : "_");
                opos += sprintf(sel + opos, "%s", u == 1 ? "#" : "_");
            }
            u = 1 - u;
        }
        sprintf(sel + opos, 0);
        printf("\n");
        fflush(stdout);

        if (!strcmp(sel, GREEN)) {
            system("echo 1 > /sys/class/gpio/gpio18/value");
        }
        if (!strcmp(sel, RED)) {
            system("echo 0 > /sys/class/gpio/gpio18/value");
        }

        if (!strcmp(sel, VOL_UP)) {
            printf("KEY VOL_UP\n");
            fflush(stdout);
        }
        if (!strcmp(sel, VOL_DOWN)) {
            printf("KEY VOL_DOWN\n");
            fflush(stdout);
        }
        if (!strcmp(sel, VOL_MUTE)) {
            printf("KEY VOL_MUTE\n");
            fflush(stdout);
        }
    }

    *mark1 = *mark2;
}


int main()
{
    struct pollfd fds[1];
    printf("Opening GPIO pIN 4\n");
    fflush(stdout);
    int gpio4 = open(GPIO4_VALUE, O_RDONLY);
    if (gpio4 == -1) {
        printf("unable to open %s\n", GPIO4_VALUE);
        fflush(stdout);
        return -1;
    }
    printf("GPIO4 file descriptor = %d\n", gpio4);
    fflush(stdout);
    fds[0].fd = gpio4;
    fds[0].events = POLLPRI | POLLIN;

    char buf[SYSVALUE_BUFSIZE] = {0};

    int i = read(fds[0].fd, buf, SYSVALUE_BUFSIZE);
    if (!i) {
        printf("unable to read from %s\n", GPIO4_VALUE);
        fflush(stdout);
        return -1;
    }

    double timings[INPUT_BUFSIZE] = {0};
    size_t mark1 = 0, mark2 = 0;

    char oldval = buf[0];
    printf("%s %c\n", GPIO4_VALUE, buf[0]);
    fflush(stdout);
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    double timestamp = (double)ts.tv_sec * 1000000.0 + (double)ts.tv_nsec / 1000.0;
    printf("init usec: %lf\n", timestamp);
    fflush(stdout);

    while (1) {
        poll(fds, 1, -1);
        if (fds[0].revents & (POLLPRI | POLLIN)) {
            lseek(fds[0].fd, 0, SEEK_SET);
            int i = read(fds[0].fd, buf, SYSVALUE_BUFSIZE);
            if (!i) {
                printf("unable to read from %s anymore\n", GPIO4_VALUE);
                fflush(stdout);
                return -1;
            }
            if (oldval != buf[0]) {
                oldval = buf[0];
                struct timespec ts_now;
                clock_gettime(CLOCK_MONOTONIC, &ts_now);
                double timestamp_now = (double)ts_now.tv_sec * 1000000.0 + (double)ts_now.tv_nsec / 1000.0;
                double tsdiff = timestamp_now - timestamp;
                timestamp = timestamp_now;

                timings[mark2] = tsdiff;
                mark2 = (mark2 + 1) % INPUT_BUFSIZE;

                if (tsdiff > 30000) {
                    try_finalize(timings, &mark1, &mark2);
                }
            }
        }
    }
    return 0;
}
