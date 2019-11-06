/*
 * Extract region specified by character positions from a file
 *
 * Use:
 *	region filename start [+]end
 *
 * with "+" end is an offset from start
 *
 */

#include <stdio.h>
#include <sys/file.h>

main(argc,argv,envp)
int argc;
char **argv, **envp;
{
  char *filename;
  int start,end;
  FILE *infile;
  int cnt,chr;

  if (argc < 4) { printf("Usage: region filename start [+]end\n"); exit(0); }

  filename = argv[1];
  sscanf(argv[2],"%d",&start);
  if (*(argv[3]) == '+') {
    sscanf(1+(argv[3]),"%d",&end);
    end += start;
  } else
    sscanf(argv[3],"%d",&end);

  if ( ( infile = fopen(filename,"r") ) == NULL ) {
    printf("Can't open the file '%s'.  Giving up.\n",filename);
    exit(-1); }

  fseek(infile,start-1,L_SET);
  cnt = end - start;
  while (0 < cnt--) {
    chr = fgetc(infile);
    if (chr == EOF) break;
    putchar(chr);
  }
  exit(0);
}
