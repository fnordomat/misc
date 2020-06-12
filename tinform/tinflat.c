#include<math.h>
#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<ctype.h>
#include<stdarg.h>

const static unsigned int tokinc=4096;

// #define septok() Y(&tokens, &j, &ntok, &tokensmax)

void Y(char ***tokens, int *j, int *ntok, int *tokensmax) {
    if (*j) {
        (*tokens)[*ntok][(*j)++]=0; (*j)=0; (*ntok)++;
    }
//    fprintf( stderr, "%ld %ld\n", *ntok, *tokensmax );
    if (*ntok>=*tokensmax) {
//        fprintf( stderr, "%ld >= %ld\n", *ntok, *tokensmax );
        *tokensmax += tokinc;
        *tokens = realloc(*tokens, *tokensmax * sizeof(char*));
        for ( int op = *tokensmax - tokinc ; op < *tokensmax ; ++op ) {
            (*tokens)[op] = malloc(256 * sizeof(char)); memset((*tokens)[op], 0, 256); 
        }
    }
}

// NotaBene: c can never be 0 before the end.
void exttoki(char ***tokens, int *ntok, int *j, char c) {
    (*tokens)[*ntok][(*j)++] = c;
    if ((*j) % 256) {
        // yeah I know length should be stored to make this efficient
        (*tokens)[*ntok] = realloc((*tokens)[*ntok],
                                   2*(strlen((*tokens)[*ntok])+1));
    }
}

void toko(char* program) {
  char **tokens;
//  int tokensmax = 8; // more reasonable: 8192;
  int tokensmax = 8192;
  tokens = malloc(tokensmax * sizeof(char*));
  int ntok = 0;
  for (int i=0; i < tokensmax; ++i) {
      tokens[i] = malloc(256 * sizeof(char));
      memset(tokens[i], 0, 256);
  }

  char fla = 0x0;

  int splash= 0;
  char first='\0';
  char prev ='\0';
  char three = 0;

  int j=0;
  char c = '\0';
  for (int i=0; i<strlen(program); i++) {

      prev = c;
      c = program[i];

      if (c == 0) break;
      if (fla & 1) {
          if (c == '\n') {
              fla ^= 1;
              continue;
          }
          else {
              fla ^= 1;
              if (!(fla & 4) && !(fla & 8)) {
                  exttoki(&tokens, &ntok, &j, '\\');
                  exttoki(&tokens, &ntok, &j, c);
              }
              continue;
          }
      }
      if (c == '\\') {
          fla |= 1;
          continue;
      }
      if (fla & 4) {
          if (c == '\n') {
              fla ^= 4;
              Y(&tokens, &j, &ntok, &tokensmax);
              continue;
          } else {
              continue;
          }
      }
      if (fla & 8) {
          if (c == '/' && splash) {
              fla ^= 8; splash = 0;
              if (!(fla & 2)) Y(&tokens, &j, &ntok, &tokensmax);
              continue;
          }
          if (c == '*') {
              splash = 1;
              continue;
          } else {
              continue;
          }
      }
      if (fla & 32) {
          if (c == '\'') {
              exttoki(&tokens, &ntok, &j, '\'');
              fla ^= 32; // sq = 0;
              Y(&tokens, &j, &ntok, &tokensmax);
              continue;
          } else {
              exttoki(&tokens, &ntok, &j, c);
              continue;
          }
      }
      if (fla & 16) {
          if (c == '"') {
              exttoki(&tokens, &ntok, &j, '"');
              fla ^= 16; //dq = 0;
              Y(&tokens, &j, &ntok, &tokensmax);
              continue;
          } else {
              exttoki(&tokens, &ntok, &j, c);
              continue;
          }
      }
      if (c == ' ' && !(fla & 2)){
          // !sharp && !sharp &&! sharp&&!sharp) {
          Y(&tokens, &j, &ntok, &tokensmax);
          continue;
      }
      if (c == '\n' && !(fla & 2)) {
          Y(&tokens, &j, &ntok, &tokensmax);
          continue;
      }
      if (c == '\n' && (fla & 2)) {
          fla ^= 2;
          Y(&tokens, &j, &ntok, &tokensmax);
          continue;
      }
      if (c == '#') {
          fla |= 2;
          exttoki(&tokens, &ntok, &j, c);
          continue;
      }
      if (c == '/' && prev == '/') {
          first = '\0';
          fla |= 4;
          continue;
      }
      if (c == '*' && prev == '/') {
          first = '\0';
          fla |= 8;
          continue;
      }
      if (c != '/' && c != '\n' && (fla & 2)) {
          exttoki(&tokens, &ntok, &j, c);
          continue;
      }
      if (index("{}()[];,?:", c)) {
          if (first) { Y(&tokens, &j, &ntok, &tokensmax); exttoki(&tokens, &ntok, &j, first); first = 0; }
          Y(&tokens, &j, &ntok, &tokensmax);
          exttoki(&tokens, &ntok, &j, c);
          Y(&tokens, &j, &ntok, &tokensmax);
          continue;
      }
      if (first) {
          if (first == '<' || first == '>') {
              if (three) {
                  Y(&tokens, &j, &ntok, &tokensmax);
                  exttoki(&tokens, &ntok, &j, three);
                  exttoki(&tokens, &ntok, &j, first);
                  first = 0;
                  three = 0;
                  if (c == '=') {
                      exttoki(&tokens, &ntok, &j, c);
                      Y(&tokens, &j, &ntok, &tokensmax);
                  } else {
                      Y(&tokens, &j, &ntok, &tokensmax);
                      if (index("/^+*%-=<>!&|", c)) {
                          first = c;
                      } else {
                          first = 0;
                          exttoki(&tokens, &ntok, &j, c);
                      }
                  }
                  continue;
              }
              if (!three && first == c) {
                  three = c;
                  continue;
              }
          }
          char zee[4] = {first, c, ' ', 0};
          char* p = strstr("== ^= && || &= |= /= ++ -- += *= %= -= << >> <= >= != ", zee);
          if (p) {
              Y(&tokens, &j, &ntok, &tokensmax);
              exttoki(&tokens, &ntok, &j, first);
              exttoki(&tokens, &ntok, &j, c);
              first = 0;
              Y(&tokens, &j, &ntok, &tokensmax);
              continue;
          } else {
              Y(&tokens, &j, &ntok, &tokensmax);
              exttoki(&tokens, &ntok, &j, first);
              Y(&tokens, &j, &ntok, &tokensmax);
              if (c == '"') { first =0; goto dqon; }
              if (c == '\'') { first =0; goto sqon; }
              if (index("/^+*%-=<>!&|", c)) {
                  first = c;
              } else {
                  first = 0;
                  exttoki(&tokens, &ntok, &j, c);
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
          fla |= 16; // dq = 1;
          Y(&tokens, &j, &ntok, &tokensmax);
          exttoki(&tokens, &ntok, &j, c);
          continue;
      }
      if (c == '\'') {
sqon:
          fla |= 32; // sq = 1;
          Y(&tokens, &j, &ntok, &tokensmax);
          exttoki(&tokens, &ntok, &j, c);
          continue;
      }
      //  if (prev && ((prev == '_' || isalpha(prev) && (c == '.'))) Y(&tokens, &j, &ntok, &tokensmax);
      // TODO this necessitates check for NUMBER before ...
      // "digit" is not enough ...
      exttoki(&tokens, &ntok, &j, c);
  }

  // print out flatly ...
  for (int i=0; i<=ntok; ++i) {
      printf("%s\n", tokens[i]);
  }
  fprintf(stdout, "\n");

  for (int i=0; i < tokensmax; ++i) { 
      free(tokens[i]); 
  } 
  free(tokens); 
}

int main(int argc, char ** argv) {
  const unsigned int S=23;
  char * program = (char*)malloc(S*sizeof(char));
  int i=0;
  for(;;) {
      size_t bytes = fread(program+i, sizeof(char), S, stdin);
      if (bytes < S)
            if (feof(stdin)) {
            program[i+bytes] = 0;
                break;
            }
      i += bytes;
      program = (char*)realloc(program, (S+i)*sizeof(char));
  }
  toko(program);
  free(program);
}
