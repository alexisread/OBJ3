/*
 * Print out character position in file with context
 *
 * Use:
 *	showpos [[-Context] File Character-position]...
 *
 * Please forward bug reports to: winkler@csl.sri.com
 */

#include <stdio.h>

int context;
int linelen;

main(argc,argv)
int argc;
char *argv[];
{
  register i;
  char *file;
  int posn,res;

  if (argc<=1) {
    printf("Usage: showpos [[-Context] File Character-position]...\n");
    exit(0);
  }

  linelen = 70;
  context = 100;
  for (i=1; i<argc;i++)
    if (argv[i][0] == '-') {
      sscanf(&argv[i][1],"%d",&context);
    } else {
      file = argv[i];
      i++;
      if (i == argc) {
	printf("showpos: error in arguments; missing position\n");
	exit(-1);
      }
      res = sscanf(argv[i],"%d",&posn);
      if (res == 0 || res == EOF) {
	printf("showpos: error in arguments; bad position\n");
	exit(-1);
      }
      showpos(file,posn);
    }
}

showpos(file,posn)
char *file;
int posn;
{
  FILE *f;
  int pos,first,i,flag,last,num;
  register int ch,chpos;

  if ((f = fopen(file, "r")) == NULL) {
    fprintf(stderr, "showpos: Can't open %s\n", file);
    return(0);
  }
  if (context < posn) pos = posn - context; else pos = 0;
  last = posn + context;
  fseek(f,pos,0);
  if (0 < pos) { printf(" ....."); chpos = 6; } else chpos = 0;
  first = pos;
  flag = 0;
  while (EOF != (ch = fgetc(f)) && pos <= last) {
    putchar(ch);
    pos++;
    if (ch == '\n' || ch == '\r' || ch == '\014') {
      if (!flag && posn <= pos) {
	num = chpos+posn-pos;
	for (i=num;0<i;i--) putchar('-');
	printf("^=%d",posn);
	num++; while (posn>9) { posn /= 10; num++; }
	for (i=num;i<linelen;i++) putchar('-');
	putchar('\n');
	flag = 1;
      }
      chpos = 0;
    } else
    if (ch == '\t') chpos += 8 - (chpos % 8);
    else
      chpos++;
  }
  if (ch != EOF) printf(".....\n"); else if (0 != chpos) putchar('\n');
  if (ch != EOF) {
    if (!flag) {
      num=chpos+posn-pos-1;
      for (i=num;0<i;i--) putchar('-');
      printf("^=%d",posn);
      /* num++; while (posn>9) { posn /= 10; num++; }
      for (i=num;i<linelen;i++) putchar('-'); */
      putchar('\n');
    }
  } else {
      if (!flag || pos == first) printf("[beyond eof]\n");
  }
}
