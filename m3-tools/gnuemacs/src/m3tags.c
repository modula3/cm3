/*
  David Goldberg, goldberg@parc.xerox.com, Jan 31, 1992.

  Parses a Modula-3 program, and writes an FTAG format tags
  file on stdout.   Based on m3pp from SRC.

  Usage: tags [options] file1 file2 .....

  Options are:
  -q  put name and qualified name in tags file (otherwise just name)
  -e  don't put exception names in tags file
  -t  don't put type names in tags file
  -v  don't put variable names in tags file
  -c  don't put CONST defs in tags file
  -o  oldstyle: use ETAG format tag file instead of FTAG
  -   take the list of files from standard input
*/

#include <stdio.h>

int notypes, novars, noconst, noexcepts, oldstyle, qnames, fromstdin;

int main(argc, argv)
     char **argv;
{
  char line[10000];

    insertKeywords();
    while (argc > 1) {
	if (argv[1][0] == '-')
		switch(argv[1][1]) {
		case 'q':  qnames = 1; break;
		case 'e':  noexcepts = 1; break;
		case 't':  notypes = 1; break;
		case 'v':  novars = 1; break;
		case 'c':  noconst = 1; break;
		case 'o':  oldstyle = 1; break;
		case '\0': fromstdin = 1; break;
		}
	else
		file(argv[1]);
	argc--;
	argv++;
    }
    if (fromstdin) {
      while (gets (line)) {
	file (line); }}

    return (0);
}

static int first = 1;

file(name)
    char *name;
{
  FILE *fp;
  
  if ((fp = fopen(name, "r")) == 0) {
    fprintf(stderr, "Can't open %s\n", name);
    exit(1); }
  
  setinput(fp);
  initialize();
  initializeLex();  

  if (yyparse () == 1) {
    fprintf(stderr, "Parse failed on %s\n", name); }
  else {
    if (!oldstyle && first) {
      printf("<<FTAGS-1.0>>");
      first = 0; }
    printit(name); }
  fclose(fp);
}
