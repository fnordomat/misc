#define septok() if (j) { j=0; ntok++; }
#include<math.h>
#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<ctype.h>

double r = 5.5*4;

char** gloob(void *t[
], ...) {
    int k=0;
    k %= (k *= (k+= (k++)));
    k<<=k--;
    k|=k--;
    k -=--k<<1?3:2;
}

// we know this isn;'t, and cannot be perfect c tokenizer
void form(char* program) {
  // FIXME dynamic or fail safely
  char tokens[
      4096
      ][ 256];
  char states[4096][256];
  int ntok = 0;
//  char debug[4096][256] = {0};

  int dq= 0; int sq=0; // instring, inchar
  int ininclude =0;
  int slash= 0;int sharp=0;int bs=0;
  int eolc = 0;
  int longc = 0;
  int qmark = 0;
  char first='\0';
  char prev ='\0';
  char three = 0;

  // strings can be split up at a small cost  ... by user
  // excess / defect
  // comments? remove them
/*       // no guarantee for non well formed c program (or wild defin
es) */

  int j=0; char rach[]="// /*";
  char c = '\0';
  for (int i=0; i<strlen(program); i++) {

      prev = c;
      c = program[i];
      
      /* char status[256] = {0}; */
      /* int s = 0; */
      /* s += sprintf(status+s, "["); */
      /* if (bs)    s += sprintf(status+s, "\\"); */
      /* if (dq)    s += sprintf(status+s, "\""); */
      /* if (sq)    s += sprintf(status+s, "'"); */
      /* if (eolc)  s += sprintf(status+s, "/"); */
      /* if (longc) s += sprintf(status+s, "*"); */
      /* if (sharp) s += sprintf(status+s, "#"); */
      /* s += sprintf(status+s, "]"); */
      
      /* printf("%-8s", status); */

      /* char chda[256] = {0}; */
      /* s = 0; */
      /* if (!isprint(c)) { */
      /*     s += sprintf(chda+s, "0x%02X", c); */
      /* } else { */
      /*     s += sprintf(chda+s, "%c", c); */
      /* } */
      /* printf("%-8s", chda); */

      // break token = ntok++; j=0;
      if (c == 0) break;
      if (bs) {
          if (c == '\n') {
              bs = 0;
              continue;
          }
          else {
              bs = 0;
              if (!eolc && !longc) {
                  tokens[ntok][j++] = '\\';
                  tokens[ntok][j++] = c;
              }
              continue;
          }
      }
      if (c == '\\') {
          bs = 1;
          continue;
      }
      if (eolc) {
          if (c == '\n') {
              eolc = 0;
              septok();
              continue;
          } else {
              continue;
          }
      }
      if (longc) {
          if (c == '/' && longc==2) {
              longc = 0;
              if (!sharp) septok();
              continue;
          }
          if (c == '*') {
              longc = 2;
              continue;
          } else {
              continue;
          }
      }
      if (sq) {
          if (c == '\'') {
              tokens[ntok][j++] += '\'';
              sq = 0;
              septok();
              continue;
          } else {
              tokens[ntok][j++] += c; // addtok
              continue;
          }
      }
      if (dq) {
          if (c == '"') {
              tokens[ntok][j++] += '"';
              dq = 0;
              septok();
              continue;
          } else {
              tokens[ntok][j++] += c;
              continue;
          }
      }
      if (c == ' ' && !sharp && !sharp &&! sharp&&!sharp) {
          septok();
          continue;
      }
      if (c == '\n' && !sharp) {
          septok();
          continue;
      }
      if (c == '\n' && sharp) {
          sharp = 0;
          septok();
          continue;
      }
      if (c == '#') {
          sharp = 1;
          tokens[ntok][j++] = c;
          continue;
      }
      if (c == '/' && first == '/') {
          first = '\0';
          eolc = 1;
          continue;
      }
      if (c == '*' && first == '/') {
          first = '\0';
          longc = 1;
          continue;
      }
      if (c != '/' && c != '\n' && sharp) {
          tokens[ntok][j++] += c;
          continue;
      }
      // TODO how to treat .? has several special uses, varargs + decimal point
      if (!first && index("()[];,?:", c)) {
          septok();
          tokens[ntok][j++] = c;
          septok();
          continue;
      }
      if (first) {
          if (first == '<' || first == '>') {
              if (three) {
                  septok();
                  tokens[ntok][j++] = three;
                  tokens[ntok][j++] = first;
                  first = 0;
                  three = 0;
                  if (c == '=') {
                      tokens[ntok][j++] = c;
                      septok();
                  } else {
                      septok();
                      if (index("/^+*%-=<>!&|", c)) {
                          first = c;
                      } else {
                          first = 0;
                          tokens[ntok][j++] = c;
                      }
                  }
                  continue;
              }
              if (!three & first == c) {
                  three = c;
                  continue;
              }
          }
          char zee[4] = {first, c, ' ', 0};
          // TODO <<=, >>=
          // TODO trigraphs -> first
          char* p = strstr("== ^= && || &= |= /= ++ -- += *= %= -= << >> <= >= != ", zee);
          if (p) {
              // TODO replace with more general sliding windoof?
              septok();
              tokens[ntok][j++] = first;
              tokens[ntok][j++] = c;
              // printf("AHAH %c%c %s %lx\n", first, c, zee, p);
              first = 0;
              septok();
              continue;
          } else {
              septok();
              tokens[ntok][j++] = first;
              septok();
//              tokens[ntok][j++] = first;
              if (c == '"') { first =0; goto dqon; }
              if (c == '\'') { first =0; goto sqon; }
              if (index("/^+*%-=<>!&|", c)) {
                  first = c;
              } else {
                  first = 0;
//                  septok();
                  tokens[ntok][j++] = c;
              }
              continue;
          }
      }
      if (index("/^+*%-=<>!&|", c) && !first) {
          first = c;
          continue;
      }
      if (c == '"') {
dqon:
          dq = 1;
          septok();
          tokens[ntok][j++] = c;
          continue;
      }
      if (c == '\'') {
sqon:
          sq = 1;
          septok();
          tokens[ntok][j++] = c;
          continue;
      }
//      if (index("0123456789.", c) && !index("0123456789.", prev)) {
//          septok();
 //     }

////      if (first) { first = 0;          septok(); }
//      if (first) { first = 0; }
      tokens[ntok][j++] = c;

//      if (!isprint(c)) {
//          printf("((0x%02X))", c);
//      } else {
//          printf("((%c))", c);
//      }

  }
  
//  printf("\n");

  for (int i=0; i<ntok; ++i) {
//      printf("%-6d %s\n", i, tokens[i]);
      printf("%s\n", tokens[i]);
  }

  /* int l = strlen(program); */
  /* int k = ceil(sqrt(l)); */
  /* int z = 0; */
  /* for (int i=0; i<k; ++i) { */
  /*    for (int j=0; j<k; ++j) { */
  /*        printf("."); */
  /*    } */
  /*    printf("\n"); */
  /* } */
}
int main(int argc, char ** argv) {
  const unsigned int S=23;
  char * program = (char*)malloc(S*sizeof(char));
  int i=0;
  for(;;) {
      size_t bytes = fread(program+i, sizeof(char), S, stdin);
      // fwrite(program+i, sizeof(char), bytes, stdout);
      // fflush(stdout);
      if (bytes < S)
            if (feof(stdin)) {
            program[i+bytes] = 0;
                break;
            }
      i += bytes;
      program = (char*)realloc(program, (S+i)*sizeof(char));
  }
  form(program);
  free(program);
}
