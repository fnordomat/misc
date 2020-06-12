 
                              
#define septok() if (j) { j=0; ntok++; }
#include<math.h>
#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<ctype.h>
#include<stdarg.h>
static double r=5.5*4;struct pa{int x;char y;}_pa;char**gloob
(char*t[],...){va_list argp;va_start(argp,t);int k=!*t++*r;_pa.x
=7;if(k)return NULL;else return NULL;}void toko(char*program)
{char**tokens;tokens=malloc(4096*sizeof(char*));int ntok=0;for
(int i=0;i<4096;++i){tokens[i]=malloc(256*sizeof(char*));}char
 fla=0x0;gloob(tokens);int splash=0;char first='\0';char prev
='\0';char three=0;int j=0;char rach[]="// /*";rach[0]=rach[1
];char c='\0';for(int i=0;i<strlen(program);i++){prev=c;c=program
[i];if(c==0)break;if(fla&1){if(c=='\n'){fla^=1;continue;}else
{fla^=1;if(!(fla&4)&&!(fla&8)){tokens[ntok][j++]='\\';tokens[
ntok][j++]=c;}continue;}}if(c=='\\'){fla|=1;continue;}if(fla&
4){if(c=='\n'){fla^=4;septok();continue;}else{continue;}}if(fla
&8){if(c=='/'&&splash){fla^=8;splash=0;if(!(fla&2))septok();continue
;}if(c=='*'){splash=1;continue;}else{continue;}}if(fla&32){if
(c=='\''){tokens[ntok][j++]+='\'';fla^=32;septok();continue;}
else{tokens[ntok][j++]+=c;continue;}}if(fla&16){if(c=='"'){tokens
[ntok][j++]+='"';fla^=16;septok();continue;}else{tokens[ntok]
[j++]+=c;continue;}}if(c==' '&&!(fla&2)){septok();continue;}if
(c=='\n'&&!(fla&2)){septok();continue;}if(c=='\n'&&(fla&2)){fla
^=2;septok();continue;}if(c=='#'){fla|=2;tokens[ntok][j++]=c;
continue;}if(c=='/'&&prev=='/'){first='\0';fla|=4;continue;}if
(c=='*'&&prev=='/'){first='\0';fla|=8;continue;}if(c!='/'&&c!=
'\n'&&(fla&2)){tokens[ntok][j++]+=c;continue;}if(!first&&index
("()[];,?:",c)){septok();tokens[ntok][j++]=c;septok();continue
;}if(first){if(first=='<'||first=='>'){if(three){septok();tokens
[ntok][j++]=three;tokens[ntok][j++]=first;first=0;three=0;if(
c=='='){tokens[ntok][j++]=c;septok();}else{septok();if(index(
"/^+*%-=<>!&|",c)){first=c;}else{first=0;tokens[ntok][j++]=c;
}}continue;}if(!three&&first==c){three=c;continue;}}char zee[
4]={first,c,' ',0};char*p=strstr("=="" ""^="" ""&& || &= |= /= ++ -- += *= %= -= << >> <= >= != "
,zee);if(p){septok();tokens[ntok][j++]=first;tokens[ntok][j++
]=c;first=0;septok();continue;}else{septok();tokens[ntok][j++
]=first;septok();if(c=='"'){first=0;goto dqon;}if(c=='\''){first
=0;goto sqon;}if(index("/^+*%-=<>!&|",c)){first=c;}else{first
=0;tokens[ntok][j++]=c;}continue;}}if(index("/^+*%-=<>!&|",c)
&&!first){first=c;continue;}if(c=='"'){dqon:fla|=16;septok();
tokens[ntok][j++]=c;continue;}if(c=='\''){sqon:fla|=32;septok
();tokens[ntok][j++]=c;continue;}tokens[ntok][j++]=c;}int K=0
;for(int i=0;i<=ntok;++i){K+=strlen(tokens[i]);}K=ceil(sqrt(K
));int S=K*K;char bitmap[S];memset(bitmap,0,S);for(int y=0;y<
K;++y){for(int x=0;x<K;++x){int b =(x>K/2)^(y>K/2);if(b)bitmap
[y*K+x]=1;}}
#define zuzu 3
 int  excess =0;int pr =0;int l =0;int p =0;for(int i =0;i<=ntok
;++i){for(;l<=S&&((!bitmap[l])||excess>0);l++){fprintf(stdout
," ");excess--;pr++;if(l%K==0){fprintf(stdout,"\n");pr=0;}}if
(tokens[i][0]=='#'&&pr){fprintf(stdout,"\n");pr=0;}if(i>0&&isalnum
(tokens[i-1][strlen(tokens[i-1])-1])&&isalnum(tokens[i][strlen
(tokens[i])-1])){p=fprintf(stdout," ");pr+=p;excess+=p;}p=fprintf
(stdout,"%s",tokens[i]);pr+=p;if(!tokens[i][0]=='#'){int l0 =
l;for(;l<=S&&l<=l0+strlen(tokens[i]);l++){if(!bitmap[l]){excess
++;}}}if(tokens[i][0]=='#'||pr>=K){fprintf(stdout,"\n");pr=0;
}}fprintf(stdout,"\n");for(int i =0;i<=ntok;++i){free(tokens[
i]);}free(tokens);}int main (int argc ,char**argv){const unsigned
  int  S =23;char*program=(char*)malloc(S*sizeof(char));int i
 =0;for(;;){size_t bytes =fread(program+i,sizeof(char),S,stdin
);if(bytes<S)if(feof(stdin)){program[i+bytes]=0;break;}i+=bytes
;program=(char*)realloc(program,(S+i)*sizeof(char));}toko(program
);free(program);}
