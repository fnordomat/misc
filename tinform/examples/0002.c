#include<math.h>
#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<ctype.h>
#include<stdarg.h>
#define C char
#define I int
#define Z sizeof
#define Q goto endo

const static I o=4711;
const static C* R = "Error while opening %s\n";
const static C D[] = "0123498765";
const static C str[] = "{}()[];,?:";
const static C ve[] = "/^+*%-=<>!&|";
const static C Pe[] = "=="
                      " "
                      "^="
                      " "
                      "&"
                      "& |"
                      "| &"
                      "= |"
                      "= /"
                      "= +"
                      "+ -"
                      "- +"
                      "= *"
                      "= %"
                      "= -"
                      "= "
                      "<<"
                      ">>"
                      "<="" >""= ""!= ";

#define X() Y(&T, &j, &N, &M)

void Y(C ***T, I *j, I *N, I *M) {
    if (*j) {
        (*T)[*N][(*j)++]=0;
        (*j)=0;
        (*N)++;
    }
    if (*N>=*M) {
        *M += o;
        *T = realloc(*T, *M * Z(C*));
        for ( I op = *M - o ; op < *M ; ++op ) {
            (*T)[op] = malloc(256 * Z(C));
            memset((*T)[op], 0, 256);
        }
    }
}

void E(C ***T, I *N, I *j, C c) {
    (*T)[*N][(*j)++] = c;
    if ((*j) % 256) {
        (*T)[*N] = realloc((*T)[*N],
                           2*(strlen((*T)[*N])+1));
    }
}

void toko(FILE* P, C* pbm) {
    C **T;
    I M = 8192L;
    T = malloc(M * Z(C*));
    I N = 0;
    for (I i=0; i < M; ++i) {
        T[i] = malloc(256 * Z(C));
        memset(T[i], 0, 256);
    }

    C F = 0x0;

    I S= 0;
    C f='\0';
    C prev ='\0';
    C t = 0;

    I j=0;
    C c = '\0';
    I ch;
    I i=0;
    while (EOF!= (ch=fgetc(P))) {
        c = ch;

        if (c == 0) break;
        if (1 & F & 1) {
            if (c == '\n') {
                F ^= 1;
                Q;
            }
            else {
                F ^= 1;
                if (!(F & 4) && !(F & 8)) {
                    E(&T, &N, &j, '\\');
                    E(&T, &N, &j, c);
                }
                Q;
            }
        }
        if (c == '\\') {
            F |= 1;
            Q;
        }
        if (F & 4) {
            if (c == '\n') {
                F ^= 4;
                X();
                Q;
            } else {
                Q;
            }
        }
        if (F & 8) {
            if (c == '/' && S) {
                F ^= 8;
                S = 0;
                if (!(F & 2)) X();
                Q;
            }
            if (c == '*') {
                S = 1;
                Q;
            } else {
                Q;
            }
        }
        if (F & 32) {
            if (c == '\'') {
                E(&T, &N, &j, '\'');
                F ^= 32;
                X();
                Q;
            } else {
                E(&T, &N, &j, c);
                Q;
            }
        }
        if (F & 16) {
            if (c == '"') {
                E(&T, &N, &j, '"');
                F ^= 16;
                X();
                Q;
            } else {
                E(&T, &N, &j, c);
                Q;
            }
        }
        if (c == ' ' && !(F & 2)) {
            X();
            Q;
        }
        if (c == '\n' && !(F & 2)) {
            X();
            Q;
        }
        if (c == '\n' && (F & 2)) {
            F ^= 2;
            X();
            Q;
        }
        if (c == '#') {
            F |= 2;
            E(&T, &N, &j, c);
            Q;
        }
        if (c == '/' && prev == '/') {
            f = '\0';
            F |= 4;
            Q;
        }
        if (c == '*' && prev == '/') {
            f = '\0';
            F |= 8;
            Q;
        }
        if (c != '/' && c != '\n' && (F & 2)) {
            E(&T, &N, &j, c);
            Q;
        }
        // todo unite? once we're sure
        // todo obfuscate strings
        // todo replace macros with jumps and call/cc
        if (!f && index(str, c)) {
            X();
            E(&T, &N, &j, c);
            X();
            Q;
        }
        if (f) {
            if (f == '<' || f == '>') {
                if (t) {
                    X();
                    E(&T, &N, &j, t);
                    E(&T, &N, &j, f);
                    f = 0;
                    t = 0;
                    if (c == '=') {
                        E(&T, &N, &j, c);
                        X();
                    } else {
                        X();
                        if (index(ve, c)) {
                            f = c;
                        } else {
                            f = 0;
                            E(&T, &N, &j, c);
                        }
                    }
                    Q;
                }
                if (!t && f == c) {
                    t = c;
                    Q;
                }
            }
            C e[4] = {f, c, ' ', 0};
            C* p = strstr(Pe, e);
            if (p) {
                X();
                E(&T, &N, &j, f);
                E(&T, &N, &j, c);
                f = 0;
                X();
                Q;
            } else {
                X();
                E(&T, &N, &j, f);
                X();
                if (c == '"') {
                    f =0;
                    goto dqon;
                }
                if (c == '\'') {
                    f =0;
                    goto sqon;
                }
                if (index(ve, c)) {
                    f = c;
                } else {
                    f = 0;
                    E(&T, &N, &j, c);
                }
                Q;
            }
        }
        if (index(ve, c) && !f) {
            f = c;
            Q;
        }
        if (c == '"') {
dqon:
            F |= 16;
            X();
            E(&T, &N, &j, c);
            Q;
        }
        if (c == '\'') {
sqon:
            F |= 32;
            X();
            E(&T, &N, &j, c);
            Q;
        }
        E(&T, &N, &j, c);
endo:
        prev = c;
        i++;
    }

    I G = 0;
    for (I i=0; i<=N; ++i) {
        G += strlen(T[i]);
    }

    C *B;
    I K,A=0;
    if (pbm) {
        FILE* file=fopen(pbm, "rb");
        if (file == 0)
        {
            fprintf(stderr, R, pbm);
            exit(-1);
        }
        C ch;
        I fla=0x0;
        I w=0, h=0;
        C wid[0xf]= {0}, hei[0xf]= {0};
        I pos=0;
        while((ch = fgetc(file)) != EOF) {
            if (isspace(ch) && ch != '\n' && A != 3) fla|=2;
            if (ch == '\n' && A == 6) fla|=2;
            if (fla&1) {
                if (ch == '\n') fla^=1;
                fla|=2;
            }
            if (index("#",ch)) {
                fla |= 1;
                fla |= 2;
            }
            if (fla&2) {
                fla^=2;
                continue;
            }
            switch (A) {
            case 0:
                if (ch=='P') A = 1;
                else A=7;
                break;
            case 1:
                if (ch=='1') A = 2;
                else A=7;
                break;
            case 2:
                if (ch=='\n') A = 3;
                else A=7;
                break;
            case 3:
                if (index(D, ch)) {
                    wid[w++] = ch;
                }
                else if (isspace(ch)) A = 4;
                else A=7;
                break;
            case 4:
                if (index(D, ch)) {
                    hei[h++] = ch;
                }
                else if (ch == '\n') A = 5;
                else A=7;
                break;
            case 5:
                sscanf(hei, "%d", &h);
                sscanf(wid, "%d", &w);
                B = malloc(Z(C) * w*h);
                S = w*h;
                K = w;
                A = 6; // no break;
            case 6:
                if (ch == '0') B[pos++] = 0;
                if (ch == '1') B[pos++] = 1;
                break;
            }
        }
        fclose(file);
    }
    if (!pbm || A==7) {
        K = ceil(sqrt(G));
        S = K*K;
        B = malloc(S * Z(C));
        memset(B, 0, S);
        for (I y=0; y<K; ++y) {
            for (I x=0; x<K; ++x) {
                B[y*K + x] = 1;
            }
        }
    }

    I pr = 0;
    I l = 0;
    I broken = 0;
    for (I i=0; i<=N; ++i) {
        if (T[i][0] == '#') {
            if (pr) printf("\n");
            broken = pr;
            pr = 0;
            printf("%s", T[i]);
            printf("\n");
            continue;
        }

        for (; broken>0; broken--) {
            printf(" ");
            pr++;
        }

        for (; l<=S && !B[l]; l++) {
            printf(" ");
            pr++;
            if (pr >= K) {
                pr = 0;
                printf("\n");
                continue;
            }
        }

        I p=0;

        if (i>0
                && isalnum(T[i-1][strlen(T[i-1])-1])
                && isalnum(T[i  ][0]))
        {
            p += printf(" ");
        }

        p += printf("%s", T[i]);
        pr += p;

        l += p;

        if (pr >= K) {
            printf("\n");
            I U = pr - K;
            if (U>0) {
                l -= U;
            }
            pr = 0;
        }
    }

    free(B);

    printf("\n");

    for (I i=0; i < M; ++i) {
        free(T[i]);
    }
    free(T);
    fclose(P);
}

I main(I argc, C ** argv) {
    if (argc<2) exit(-1);
    FILE* P;
    if (strstr("-", argv[1])) {
        P = stdin;
    } else {
        P=fopen(argv[1], "rb");
        if (P == 0)
        {
            fprintf(stderr, R, argv[1]);
            exit(-1);
        }

    }
    toko(P, (argc>2) ? argv[2] : 0);
}
