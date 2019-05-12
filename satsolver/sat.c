#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>

#define bits_t uint32_t
#define BS 32

struct clause {
    bits_t* pos;
    bits_t* neg;
};

struct prob {
    int numC;
    int numV;
    struct clause* cls;
};

struct assignment {
    bits_t* t;
    bits_t* f;
};

void mkAssignment(struct assignment* a, int x) {
    a->t = (bits_t*) malloc(x * sizeof(bits_t));
    a->f = (bits_t*) malloc(x * sizeof(bits_t));
}

void rmAssignment(struct assignment a) {
    free(a.t);
    free(a.f);
}

struct searchState {
    int stackDepth;
    int allocatedDepth;
    // A is for assigments
    struct assignment* stackA;
    // B is for branch-points
    int* stackB;
    // C is for clauses already satisfied, which no longer constrain:
    bits_t** stackC;
};

void extend1(struct searchState* s, int x, int y) {
    if (s->stackDepth == s->allocatedDepth) {
        s->allocatedDepth++;
        s->stackA = (struct assignment*) realloc(s->stackA, s->allocatedDepth*sizeof(struct assignment));
        s->stackB = (int*) realloc(s->stackB, sizeof(int)*(s->allocatedDepth-1));
        s->stackC = (bits_t**) realloc(s->stackC, s->allocatedDepth*sizeof(bits_t*));
    }
    mkAssignment(&s->stackA[s->stackDepth], x);
    s->stackB[s->stackDepth-1] = 0;
    s->stackC[s->stackDepth] = (bits_t*) malloc(sizeof(bits_t*) * y);
    if (s->stackDepth > 0) {
        memcpy(s->stackA[s->stackDepth].t, s->stackA[s->stackDepth-1].t, sizeof(bits_t)*x);
        memcpy(s->stackA[s->stackDepth].f, s->stackA[s->stackDepth-1].f, sizeof(bits_t)*x);
        memcpy(s->stackC[s->stackDepth],   s->stackC[s->stackDepth-1],   sizeof(bits_t)*y);
    }
    s->stackDepth++;
}

void next(struct searchState* s, int x, int y) {
    s->stackDepth--;
    if (s->stackDepth == 0) {
        return; // fail
    }
    int v = s->stackB[s->stackDepth-1];
    if (v > 0) {
        extend1(s, x, y);
        s->stackA[s->stackDepth-1].f[(v-1)/BS] |= 1 << ((v-1) % BS);
        s->stackB[s->stackDepth-2] = -v;
    } else {
        return next(s, x, y);
    }
}

int unitprop(struct prob p, struct assignment* assignment, bits_t* satisfiedClauses) {
    int count = 0;
    for (int c=0; c<p.numC; ++c) {
        if ((satisfiedClauses[c/BS] >> (c%BS)) & 1) {
            goto ignore;
        }
        int uav = 0;
        int v = 0;
        for (int i=0; i<(p.numV+BS-1)/BS && uav<2; ++i) {
            bits_t u = ~(assignment->f[i] ^ assignment->t[i]);
            bits_t u_p_not_n = u & p.cls[c].pos[i];
            bits_t u_n_not_p = u & p.cls[c].neg[i];
            for (int b=0; b<BS; ++b) {
                if ((u_p_not_n >> b) & 1) {
                    v = 1 + (BS*i + b);
                    uav ++;
                }
                else if ((u_n_not_p >> b) & 1) {
                    v = -1 - (BS*i + b);
                    uav ++;
                }
            }

            if (((assignment->f[i] & p.cls[c].neg[i]) != 0) ||
                ((assignment->t[i] & p.cls[c].pos[i]) != 0)) {
                satisfiedClauses[c/BS] |= 1 << (c%BS);
                goto ignore;
            }
        }
        if (uav == 1) {
            count++;
            if (v<0) {
                v = -v-1;
                assignment->f[v/BS] |= 1 << (v%BS);
            } else {
                v -= 1;
                assignment->t[v/BS] |= 1 << (v%BS);
            }
        } else if (!uav) {
            return -1;
        }
ignore:
        continue;
    }
    return count;
}

int dpll(struct prob p, struct searchState* s) {
    // 1. variables occurring only unipolarly
    int x = (p.numV+BS-1)/BS;
    int y = (p.numC+BS-1)/BS;

    bits_t xposneg[2 * x]; 
    memset(xposneg, 0, 2 * x);

    for (int c=0; c<p.numC; ++c) {
        if ((s->stackC[0][c/BS] >> (c%BS)) & 1) continue;
        for (int i=0; i<x; ++i) {
            xposneg[i] |= p.cls[c].pos[i];
            xposneg[x+i] |= p.cls[c].neg[i];
        }
    }
    for (int i=0; i<x; ++i) {
        s->stackA[0].t[i] = xposneg[i] & ~xposneg[x+i];
        s->stackA[0].f[i] = xposneg[x+i] & ~xposneg[i];
    }

mainloop:
    for (int ret = 1; ret > 0; ) {
        ret = unitprop(p, &s->stackA[s->stackDepth-1], s->stackC[s->stackDepth-1]);
        if (ret < 0) { // dead end
            next(s, x, y);
            if (s->stackDepth == 0) {
                return -1;
            } else {
                goto mainloop;
            }
        }
    }

    int leastUnassigned = p.numV;
    for (int i=0; i<p.numV; ++i) {
        bits_t u = ~( s->stackA[s->stackDepth-1].t[i/BS] | s->stackA[s->stackDepth-1].f[i/BS] );
        if (1 & (u >> (i%BS))) { leastUnassigned=i; break; }
    }

    if (leastUnassigned == p.numV) { return 1; }

    extend1(s, x, y);
    s->stackB[s->stackDepth-2] = 1 + leastUnassigned;
    s->stackA[s->stackDepth-1].t[leastUnassigned/BS] |= 1 << (leastUnassigned % BS);
    goto mainloop;
}

int main(int argc, char **v) {
    int numC = argc - 1;
    char varNames[27] = {0};

    int numV = 0;
    for (int k=0; k<numC; ++k) {
        for (int i=0; v[k+1][i] != 0; ++i) {
            char c = v[k+1][i] & 0xdf;
            int x = 0;
            for (; varNames[x] != c && varNames[x] != 0; ++x);
            if (varNames[x] == 0) {
                numV++; varNames[x] = c;
            }
        }
    }

    int x = (numV+BS-1)/BS;
    int y = (numC+BS-1)/BS;

    struct clause* cls = (struct clause*) malloc(numC * sizeof(struct clause));

    struct prob p = { numC, numV, cls };
    struct searchState* state = (struct searchState*)malloc(sizeof(struct searchState));
    state->stackDepth = 1;
    state->allocatedDepth = 1;
    state->stackB = NULL;
    state->stackA = (struct assignment*) malloc(sizeof(struct assignment));
    mkAssignment(&state->stackA[0], x);
    state->stackC = (bits_t**)malloc(sizeof(bits_t*));
    state->stackC[0] = (bits_t*)malloc(sizeof(bits_t) * y);
    
    for (int k=0; k<numC; ++k) {
        cls[k].pos = (bits_t*) malloc(sizeof(bits_t) * x);
        cls[k].neg = (bits_t*) malloc(sizeof(bits_t) * x);
        for (int i=0; v[k+1][i] != 0; ++i) {
            char c = v[k+1][i] & 0xdf;
            int pn = (v[k+1][i] & 0x20) >> 5;
            int x = 0;
            for (; varNames[x] != c; ++x);
            if (pn) {
                cls[k].pos[x/BS] |= 1 << (x%BS);
                if ((cls[k].neg[x/BS] >> (x%BS)) & 1) {
                    state->stackC[0][k/BS] |= 1 << (k%BS);
                }
            }
            else {
                cls[k].neg[x/BS] |= 1 << (x%BS);
                if ((cls[k].pos[x/BS] >> (x%BS)) & 1) {
                    state->stackC[0][k/BS] |= 1 << (k%BS);
                }
            }
        }
    }

    int ret = dpll(p, state);
    if (ret < 0) { printf("unsat: (...)\n"); }
    else {
        printf("sat: ");
        for (int i=0; i<numV; ++i) {
            if (1 & (state->stackA[state->stackDepth-1].f[i/BS] >> (i%BS))) {
                printf("%c", varNames[i]);
            }
            else if (1 & (state->stackA[state->stackDepth-1].t[i/BS] >> (i%BS))) {
                printf("%c", varNames[i] | 0x20);
            }
            else { printf("-"); }
        }
        printf("\n");
    }

    for (int k=0; k<numC; ++k) {
        free(cls[k].pos);
        free(cls[k].neg); 
    }
    free(cls);
    for (int i=0; i<state->allocatedDepth; ++i) {
        rmAssignment(state->stackA[i]);
        free(state->stackC[i]);
    }

    free(state->stackA);
    free(state->stackB);
    free(state->stackC);
    free(state);
    return 0;
}






