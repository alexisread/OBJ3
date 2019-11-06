#include <stdio.h>

main(argc,argv)
int argc;
char **argv;
{
  char line[500];
  char *ptr;
  int emit,emithidden;

  if (1 < argc) {
    if (NULL == freopen(argv[1],"r",stdin)) {
      perror("examples");
      exit(-1);
    }
  }

  emit = 0;
  emithidden = 0;
  while (1) {
    if (NULL == gets(line)) break;
    ptr = line;
    while (' ' == *ptr || '\t' == *ptr) ptr++;
    if (strequal("\\bobj",ptr)) { emit = 1; }
    else
    if (emit && strequal("\\eobj",ptr)) { emit = 0; printf("\n"); }
    else
    if (strequal("%\\bobj",ptr)) { emithidden = 1; }
    else
    if (emithidden && strequal("%\\eobj",ptr)) { emithidden = 0; printf("\n"); }
    else {
      if (emit) putline(line);
      else
      if (emithidden)
        { if ('%' == *line) putline(line+1); else putline(line); }
    }
  }
}

int
strequal(a,b)
register char *a,*b;
{
  while (*a && *b && *a == *b) { a++; b++; }
  return (*a == *b);
}

putline(line)
register char *line;
{
  while (*line) {
    if (*line != '\\' || !*(line+1) || (*(line+1) != '{' && *(line+1) != '}'))
        putchar(*line);
    line++; }
  putchar('\n');
}
