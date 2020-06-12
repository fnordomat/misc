#include<math.h>
#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<ctype.h>
#include<stdarg.h>
#define C char
#define I int
#define a for
#define Z sizeof
#define g goto
#define J free
#define Q g U
#define W static
#define m malloc
#define mm realloc
#define AA isalnum
#define d strlen
#define AS sscanf
#define FC fclose
#define k printf
#define H index
#define SS strstr
#define RR return
#define FG fgetc
#define vd void
#define u if
#define V FILE
#define z else
#define BR break
#define TI exit(-1);
#define X() Y(&T, &j, &N)
#define n(a,b) fprintf(stderr,a,b)

W C KK[] = "br", e[] = "br", DO[] = "%d";
W C R[] = "=**7*x7(=616?x}+R";
I GG=2<<8, M =2<<14, q=0;

vd E(C ***T, I *N, I *j, C c) {
    (*T)[*N][(*j)++] = c;
    u (*j % GG)
        (*T)[*N] = mm((*T)[*N],2*(d((*T)[*N])+1));
}
V* op(C*n) {
    V* P = fopen(n, KK);
    u (!P) n(R, n), TI;
    RR P;
}

W C PI[] = "0123456789";
W I o=4711, S = 0, b = 0, i = 0, j = 0, l = 0,
    ch, G = 0, K, A=0,
    r;
W C sr[] = "\03\05#%""pqctgb",
    ve[] = "\06w""sr}uedf""y~$",
    Hg[] = "<""9,9x,*-6"";9,""=<""x9,x}<R",
    Pe[] = "\06""e""x""~""~""e""x""$""$""e""x""w""e""x""s"
    "sexrex""}exuueex""ddex""ffex""ye",
    sp[] = "UxSQRT",
    TB[] = ",77x:1?bx}<",
    x[] = "RX",
    O[9]= {0};
C* p;C c='\n', s='\n';

vd ms(C*m,I c,I n) {
    a (I i=0; i<n; ) m[i++]=c;
}

I Y(C ***T, I *j, I *N) {
    u (*j) {
        (*T)[*N][(*j)++]=0;
        *j=0;
        ++*N;
    }
    u (*N>=M) {
        M += o;
        *T = mm(*T, M * Z(C*));
        a ( I p = M - o ; p < M ; ) {
            (*T)[p] = m(GG * Z(C));
            ms((*T)[p++], 0, GG);
        }
    }
    RR 1;
}

I main(I N, C ** T) {
    V* P=stdin;
    C F = 'r'^'b',
        f = r = 0, v = 0, t = 0, *B = 0, *go[]= {Hg,R,sp,Pe,ve,sr,TB,x,0}  ,* w=(N>2) ? T[2] : 0;
   
    
    KK[0]^=F;
    KK[1]^=F;
    F^=F;
    a (I i=0; go[i]; i++)
        a (I j=0; j<d(i[go]); )
            go[i][j++] ^= x[1];
        
   
    u (N<2) TI;
    u (!SS("-", T[1]))
        P = op(T[1]);
    N = 0;
    T = m(M * Z(C*));

    a (; i < M; ) {
        T[i] = m(GG * Z(C));
        ms(T[i++], 0, GG);
    }

    a (i=0; (ch=FG(P)) ^ EOF;) {
        u (!(c=ch)) BR;
        b = 1;
        u (F & b) {
            F ^= b;
            u (!(F & 12) && c ^ s)
                E(&T, &N, &j, '\\'),
                E(&T, &N, &j, c);
            Q;
        }
        b <<= 1;
        u (c == '\\') {
            F |= b/2;
            Q;
        }
        b <<= 1;
        u (F & b) {
            u (c == s) {
                F ^= b;
                X();
            }
            Q;
        }
        b <<= 1;
        u (F & b) {
            u (c == '/' && S) {
                F ^= b;
                S = x[1];
                u (!(F & 2)) X();
                Q;
            }
            S = c == '*' ? 1 : S;
            Q;
        }
        b <<= 1;
        u (F & 48) {
            C y = F&b?'"':'\'';
            E(&T, &N, &j, c);
            u (c^y) Q;
            F ^= F&b?b:2*b;
            X();
            Q;
        }
        u (H(" ""\n", c) && F ^ 2) {
            X();
            Q;
        }
        u (c == s && F & 2) {
            F ^= 2;
            X();
            Q;
        }
        u (c == '#') {
            F |= 2;
            E(&T, &N, &j, c);
            Q;
        }
        u (c == '/' && v == c) {
            f = 0;
            F |= 4;
            Q;
        }
        u (c == '*' && v == '/') {
            f = 0;
            F |= 8;
            Q;
        }
        u (!H("/""\n",c) && F & 2) {
            E(&T, &N, &j, c);
            Q;
        }
        u (H(sr, c)) {
            u (f) {
                X();
                E(&T, &N, &j, f);
                f=0;
            }
            X();
            E(&T, &N, &j, c);
            X();
            Q;
        }
        u (f) {
            u (H("<>",f)) {
                u (t) {
                    X();
                    E(&T, &N, &j, t);
                    E(&T, &N, &j, f);
                    f = (t = 0);
                    u (c^'=') {
                        X();
                        f = 0;
                        u (H(ve, c))
                            f = c;
                        z
                            E(&T, &N, &j, c);
                        Q;
                    }
                    E(&T, &N, &j, c);
                    X();
                    Q;
                }
                u (f == c) {
                    t = c;
                    Q;
                }
            }
            e[0] = f;
            e[1] = c;
            p = SS(Pe, e);
            X();
            E(&T, &N, &j, f);
            f = 0;
            u (p) {
                E(&T, &N, &j, c);
                X();
            }
            z {
                X();
                u (c == '"')
                    g dq;
                u (c == '\'')
                    g sq;
                u (H(ve, c))
                    f = c;
                z
                    E(&T, &N, &j, c);
            }
            Q;
        }
        u (H(ve, c) && !f) {
            f = c;
            Q;
        }
        u (c == '"') {
dq:
            F |= b;
            X();
            E(&T, &N, &j, c);
            Q;
        }
        u (c == '\'') {
sq:
            F |= 2*b;
            X();
            E(&T, &N, &j, c);
            Q;
        }
        E(&T, &N, &j, c);
U:
        b <<= 1;
        v = c;
        i++;
    }

    a (i=0; i<=N; )
        G += d(T[i++]);

    u (w) {
        V*Vv = op(w);
        I ch;
        F = 0;
        I w=0, h=0, G=0;
        C P[9]= {0};

        a(; (ch = FG(Vv)) ^ EOF;) {

            c = (C)(ch & 255);

            u (A==7) {
                u(B) 
                    J(B),
                    B=0;
                BR;
            }
            r = A>=5 && F&4;
            u(!r) {
                u ((H(sp, c) && c ^ s && A ^ 3)
                   || (c == s && A == 6)
                   || F&1
                  ) {
                    F|=2;
                    u ( F&1 && c == s) F ^=1;
                }
                F |= 3 * (H("#",c) ? 1 : 0);
                u (F&2) {
                    F ^= 2;
                    g L;
                }
            }
            u (!A++) {
                u (c^'P') A=7;
                g L;
            }
            A--;
            u (A++<2) {
                u (c=='4') F |= 4;
                z u (c^'1') A=7;
                g L;
            }
            A--;
            u (A++<3) {
                u (c^s) A=7;
                g L;
            }
            A--;
            u (A<4) {
                u (H(PI, c)) O[w++] = c;
                z u (H(sp, c)) A++;
                z A=7;
                g L;
            }
            u (A<5) {
                u (H(PI, c)) P[h++] = c;
                z u (c == s) A++;
                z A=7;
                g L;
            }
            u(A<6) {
                A++;
                AS(P, DO, &h);
                AS(O, DO, &w);
                S = (K=w)*h;
                u (S>1<<20) {
                    n(TB,S);
                    TI;
                }
                B = m(Z(C) * S);
            }
            u (F & 4) {
                a (j=7; j>=0; --j) {
                    u (q > S) g D;
                    B[q++] = (c & 1 << j) >> j;
                    u (++G >= K) {
                        G=0;
                        g L;
                    }
                }
                g L;
            }
            u (q > S) g D;
            B[q++] = c=='1';
            g L;
D:
            n(Hg, q);
            A = 7;
            u(B) {
                J(B);
                B=0;
            }
L: {
            }
        }
        u (q < S) {
            n(Hg, q);
        }
        FC(Vv);
    }
    u (!w || !B || A==7) {
        S = K = ceil(sqrt(G));
        B = m(S *= S * Z(C));
        ms(B, 1, S);
    }

    a (i=0; i<=N; ++i) {
        u (T[i][0] == '#') {
            u (r) k("\n");
            b = r;
            r = 0;
            k("%s""\n", T[i]);
        } z {

            a (; b; b--)
                r += k(" ");
            
            a (; l<=S && !B[l]; l++) {
                k(" ");
                u (++r >= K)
                    r = k("\n") - 1;
            }
            
            q = i
                && AA(T[i-1][d(T[i-1])-1])
                && AA(T[i][0]) ? k(" ") : x[1];
            r += q += k("%s", T[i]);
            l += q;
            
            u (r >= K) {
                k("\n");
                q = r - K;
                u (q>(r=0))
                    l -= q;
            }
        }
    }

    J(B);
    B=0;

    k("\n");

    a (i=0; i < M;)
        J(T[i++]);
    J(T);
    FC(P);
    RR 0;
}
