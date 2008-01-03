/* Copyright (C) 1989, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */
/*                                                             */
/* Last modified on Mon Jan  9 10:18:15 PST 1995 by kalsow     */
/*      modified on Tue Oct 13 17:17:05 PDT 1992 by muller     */


extern etext;
extern edata;
#ifdef __alpha
  extern __ldr_data;
  extern end;
#endif

#include <stdio.h>
#include <sys/file.h>
#include <fcntl.h>

extern char * getenv ();

/* We do not include marker.h here; indeed, we do not want this piece
   of code to be covered !! For the same reason, the marker is broken
   into pieces. */

char *marker2 = "Coverage 1.0";

typedef union int_chars {
  char s[4];
  int i;
} MarkerWord;
 

void exit (n)
    int n;
{
    report_coverage ();
    _cleanup ();
    _exit (n);
}

report_coverage ()
{
  int *l, *start;
  char *output_file_name;
  int output_file;
  unsigned long first_global, last_global;
  int state;
  MarkerWord mark1, mark3;
  int marker2_len = strlen (marker2);
  int marker2_ilen = marker2_len / sizeof (int);

  /* build word-aligned markers. */
  mark1.s[0] = '<';  mark1.s[1] = '<';  mark1.s[2] = '<';  mark1.s[3] = '<';
  mark3.s[0] = '>';  mark3.s[1] = '>';  mark3.s[2] = '>';  mark3.s[3] = '>';

  /* open the output file */
  output_file_name = getenv ("COVERAGE_DATABASE");
  if (output_file_name == NULL) output_file_name = "coverage.out";
  output_file = open (output_file_name, O_WRONLY | O_APPEND | O_CREAT, 0644);
  if (output_file == -1) {
    fprintf (stderr, "coverage: cannot report coverage to %s\n",
             output_file_name);
    exit (1); }

  /* get an aligned pointer to the data segment */
#ifdef mips
  first_global = 0x10000000;
  last_global  = (long) &edata;
#else
#ifdef __alpha
  first_global = (long) &__ldr_data /* 0x140000000 */;
  last_global  = (long) &end;
#else
  first_global = (long) &etext; 
  last_global  = (long) &edata;
#endif
#endif
  first_global /= sizeof (int);  first_global *= sizeof (int);

  state = 0;  /* outside a segment */

  /* scan the global data segment */
  for (l = (int*)first_global;  l < (int*)last_global;  l++) {
    /* look for a header */
    if ((*l == mark1.i)
       && (strncmp (l+1, marker2, marker2_len) == 0)) {
      /* we found the beginning of a segment */
      l += marker2_ilen + 1;
      state = 1;
      start = l;
    };

    /* look for a trailer */
    if ((*l == mark3.i)
       && (strncmp (l-marker2_ilen, marker2, marker2_len) == 0)) {
      /* we found the end of a segment */
      if (state == 1) {
        /* write the segment */
	unsigned long i = (l - marker2_ilen) - start;
	i *= sizeof (int);
	write (output_file, &i, sizeof (long));
	write (output_file, start, i);
      };
      state = 0;
      l++;
    };
  }; /* for */

  close (output_file);
}

