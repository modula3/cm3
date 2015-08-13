/* Copyright (C) 1989, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */
/*                                                             */
/* Last modified on Mon Jan  9 10:18:15 PST 1995 by kalsow     */
/*      modified on Tue Oct 13 17:17:05 PDT 1992 by muller     */


extern etext;
extern edata;
extern end;
#ifdef __alpha
  extern __ldr_data;
  extern end;
#endif

#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <sys/file.h>
#include <fcntl.h>

#include <setjmp.h>
#include <signal.h>

extern char * getenv ();

/* We do not include marker.h here; indeed, we do not want this piece
   of code to be covered !! For the same reason, the marker is broken
   into pieces. */

char *marker2 = "Coverage 1.0";

typedef union int_chars {
  char s[8];
  long i;
} MarkerWord;

sigset_t sigset;
sigjmp_buf mark;
long *saved;

void catcher( int );
void p( long start, long end, int dir );
void recover( void );


void exit (int n) {
    report_coverage ();
    _exit (n);
}

//Try and determine the bounds of the data segment between etext and edata

int MapSeg(long p0, long p1, int dir) {

    int result;

    /*
     * Block the SIGSEGV signals.  This set of
     * signals will be saved as part of the environment
     * by the sigsetjmp() function.
     */
     saved = 0;

     sigemptyset( &sigset );
     sigaddset( &sigset, SIGSEGV );
     sigprocmask( SIG_SETMASK, &sigset, NULL );

     if( sigsetjmp( mark, 1 ) != 0 ) {
         recover();
         result = 0;
     }
     else {

         p(p0,p1,dir);
         sigprocmask( SIG_SETMASK, NULL, &sigset );
         result = -1;
    }
    return( result );
}

void p( long p0, long p1, int dir ) {

    long ptmp;
    long *wd;
    long maybesegv;
    char tmp;

    struct sigaction sigact;
    int error=0;

    /* Send signal handler in case error condition is detected */

    sigemptyset( &sigact.sa_mask );
    sigact.sa_flags = 0;
    sigact.sa_handler = catcher;
    sigaction( SIGSEGV, &sigact, NULL );

    sigdelset( &sigset, SIGSEGV );
    sigprocmask( SIG_SETMASK, &sigset, NULL );


    if (dir == 0) {
      ptmp = p1;
      for (wd = (long *)p0; wd < (long *)ptmp; wd++) {
        p1 = (long)wd;
        saved = wd;
        maybesegv = *wd; //possibly generate fault
      }
    } else {
      ptmp = p0;
      for (wd = (long *)p1; wd > (long *)ptmp; wd--) {
        p0 = (long)wd;
        saved = wd;
        maybesegv = *wd; //possibly generate fault
      }
    }
}

void recover( void ) {
    sigprocmask( SIG_SETMASK, NULL, &sigset );
}

void catcher( int signo ) {
    siglongjmp( mark, -1 );
}

void CheckSegment(int output_file, long begin, long end) {

  char *c;
  char *start;
  int state;
  long marker2_len = strlen (marker2);
  long marker2_ilen = marker2_len / sizeof (long);

  state = 0;

  for (c = (char *) begin; c < (char *) end; c++) {

    if ( (*c == '<') && (*(c+1) == '<') && (*(c+2) == '<') && (*(c+3) == '<') 
         && (strncmp ((const char *)c+4, marker2, marker2_len) == 0)) {

      //Found a start marker
      c += marker2_len + 4;
      state = 1;
      start = c;
    }

    if ( (*c == '>') && (*(c+1) == '>') && (*(c+2) == '>') && (*(c+3) == '>') 
      && (strncmp ((const char *)c-marker2_len, marker2, marker2_len) == 0)) {

      //Found and end marker
      if (state == 1) {
        /* write the segment */
        unsigned long segLen = (c - marker2_len) - start;

        write (output_file, &segLen, sizeof (long));
        write (output_file, start, segLen);
      };
      state = 0;
      c++;
    }
  }
}

report_coverage () {

  char *output_file_name;
  int output_file;
  unsigned long first_global, last_global, end_global;
  long s,e;

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
  end_global   = (long) &end;
#endif
#endif

  //printf("etext %p edata %p end %p \n",first_global,last_global,end_global);

/*
//the segment from the end of the text to somewhere into the data segment
  has tags but they are not the ones we want.

  s = first_global;
  e = last_global;
  printf("range start %p end %p \n",s,e);
  s /= sizeof (long);  s *= sizeof (long);
  e /= sizeof (long);  e *= sizeof (long);
  printf("range start %p end %p \n",s,e);

  MapSeg(s,e,0);
  if (saved != 0) e = (long)saved;
  printf("range start %p end %p \n",s,e);

  CheckSegment(output_file,s,e);
*/

  s = first_global;
  e = last_global;

  s /= sizeof (long);  s *= sizeof (long);
  e /= sizeof (long);  e *= sizeof (long);
  //printf("range start %p end %p \n",s,e);

  MapSeg(s,e,1);
  if (saved != 0) s = (long)saved + 8;
  //printf("range start %p end %p \n",s,e);

  CheckSegment(output_file,s,e);

  close (output_file);
}

