/*************************************************************************
 *
 *  mymalloc.c
 *
 *   Debugging malloc.
 *
 *  Copyright (c) 1997 California Institute of Technology
 *  All rights reserved.
 *  Department of Computer Science
 *  Pasadena, CA 91125.
 *
 *  Author: Mika Nystrom <mika@cs.caltech.edu>
 *
 *  Permission to use, copy, modify, and distribute this software
 *  and its documentation for any purpose and without fee is hereby
 *  granted, provided that the above copyright notice appear in all
 *  copies. The California Institute of Technology makes no representations
 *  about the suitability of this software for any purpose. It is
 *  provided "as is" without express or implied warranty. Export of this
 *  software outside of the United States of America may require an
 *  export license.
 *
 *  $Id$
 *
 *************************************************************************/

/* this is for later debugging */
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>
#include <assert.h>
#define __IN_MYMALLOC_C

#define _ANSI_SOURCE       /* arrrgggghhhh */
# include "mymalloc.h"  
#undef _ANSI_SOURCE

#ifndef _DONT_MYMALLOC 
#define REDPATTERN "abcd"
#define REDZONE strlen(REDPATTERN)

static int mymalloc_where=0; /* for future use */
static int mblocks=0,fblocks=0,mbytes=0,fbytes=0;

/* this is a private data structure */

typedef struct mtabent {
  void *address;
  size_t size;
  int where;
  struct mtabent *next;
  char *file;
  int line;
} mtabent;

static   mtabent *mallocs=NULL;

static void print_unfree_stats(const mtabent *mtabs);

/* This is a semaphore for interrupt protection */
static int sem=0;

void *
_mycalloc (size_t number, size_t size, const char *file, const int line)
{
  void *retp;
  retp = _mymalloc(number*size, file, line);
  bzero(retp,number*size);
  return retp;
}

void *
_mymalloc (size_t size, const char *file, const int line)
{
  mtabent *mtp;  char *cp;  void *retp;

  do {
    retp=malloc(size+REDZONE);
    if (!retp) {
      (void)fprintf(stderr,"mymalloc failed---sleeping %d seconds\n\n",
		    FAIL_SLEEP);
      if (!FAIL_SLEEP) return (void *)0; /* no sleep */
      sleep(FAIL_SLEEP);
    }
  } while (!retp);
  cp= (char *)retp+size;
  (void)strncpy(cp,REDPATTERN,REDZONE); /* XXX does not copy trailing null */
  
  mtp= (mtabent *)malloc(sizeof(mtabent));
  mtp->file= strdup(file); mtp->line= line;
  mtp->address= retp; mtp->size= size; 
  mtp->where= mymalloc_where; /* XXX not yet */
  /* add to list, protected by sem */
  assert(!sem);
  sem=1;
  mtp->next= mallocs;
  mallocs= mtp;

  /* if at this point, the semaphore is zero then someone else
     has released it, and we have to quit! */

  if(!sem) {
#ifdef MYMALLOCDEBUG
    (void)printf("_mymalloc found sem false.. aborting\n");
#endif
    mymalloc_cleansig(0);
    assert(0); /* don't get here */
  }
  sem=0; /* ok you can have it again */
  
  mblocks++; mbytes+=size;
  return retp;
}

void *
_mycalloc1 (size_t size,const char *file, const int line)
{
  void *retval;
  retval= _mymalloc(size, file, line);
  bzero(retval,size);
  return retval;
}

void
myfree_f(void *ptr)
{
  /* this one is used for function pointers, when you need &myfree */
  _myfree(ptr, "FUNCTION_PTR", 0);
}

void
_myfree (void *ptr, const char *file, const int line)
{ 
  mtabent *mtp=mallocs, *mtpp= NULL;
  
  while (ptr != mtp->address) {
    mtpp= mtp;
    mtp= mtp->next;
    if (!mtp) {
      (void)fprintf(stderr,
		    "mymalloc botch: free of unmalloced space %#lx\n",
		    (unsigned long int)ptr);
      abort();
    }
  }
  assert(mtp);
  if(strncmp((char *)ptr+mtp->size,REDPATTERN,4)){
    (void)fprintf(stderr,"mymalloc botch: overwritten redzone at %#lx\n",
	    (unsigned long int)ptr+mtp->size);
      (void)fprintf(stderr, "allocated at %s: %d\n",mtp->file,mtp->line);
      abort(); 
  }

  assert(!sem);
  sem= 1; 

  /* this area is protected by semaphore */

  if (mtpp) mtpp->next=mtp->next;
  if (!mtpp) mallocs= mtp->next;
  fblocks++; fbytes+=mtp->size;

  assert(mtp->file);
  free(mtp->file);
  free(mtp);
  free(ptr);

  if(!sem) {
#ifdef MYMALLOCDEBUG
    (void)printf("_myfree found sem false.. aborting\n");
#endif
    mymalloc_cleansig(0);
    assert(0); /* don't get here */
  }
  sem=0; /* ok you can have it again */

}

void
mymalloc_cleansig(int a)
     /*ARGSUSED*/
{
  /* if we get here and sem is set then:
     the program was interrupted while updating the 
     mallocs queue.  We need to return to the program
     immediately.  By setting sem back to zero, we 
     signal to _mymalloc() that it was interrupted.
     This will cause it to call us again, at which
     time we will proceed. */

  signal(SIGINT,((void (*)(int))SIG_IGN));

  /* at this point, we MUST exit! */

  if (sem) {
#ifdef MYMALLOCDEBUG
    (void)fprintf(stdout,"Reached mymalloc_cleansig with sem set\n");
#endif

    /* when _mymalloc notices that the value of the semaphore has
       changed, it will call us back */

    sem=0; 
    return;
  }

  (void)fprintf(stdout,"\n");
  mymalloc_cleanup();   /* do normal stuff */
#ifdef _DOEXIT
  do_exit(0);
#else
  exit(0);              /* exit */
#endif
}

void
mymalloc_cleanup(void)
{
  mtabent *mtp, *nextmtp;
  size_t tot_size=0;
  int count=0;

  mtp= mallocs;
  if (!mtp) { printf("mymalloc: No malloc.\n"); return; }
  assert(mtp);
  do {
    tot_size+= mtp->size;
    count++;
    nextmtp= mtp->next;
    
  } while( (mtp = nextmtp) );
  (void)printf("mymalloc %d blocks %#x bytes\n",mblocks,mbytes);
  (void)printf("myfree   %d blocks %#x bytes\n",fblocks,fbytes);
  (void)printf("mymalloc: unfreed %d blocks total %#x bytes\n",count,tot_size);

  /* now print out various statistics */
  print_unfree_stats(mallocs);

  /* we are all done, now free it for redzone checking */
  mtp= mallocs;
  assert(mtp);
  do {
    nextmtp= mtp->next;
    myfree(mtp->address);
  } while ( (mtp= nextmtp) );

  return;
}

void *
_myrealloc (void *ptr, size_t size, const char *file, const int line)
{
  void *newptr;
  newptr= _mymalloc(size,file,line);
  
  if(!ptr) return newptr;
  assert(ptr);
  bcopy(ptr,newptr,size);
  myfree(ptr);
  return newptr;
}

char *
_mystrdup(const char *str, const char *file, const int line)
{
  char *newstr;
  size_t size;
  size= strlen(str)+1; /* one more for trailing null */
  newstr= (char*)_mymalloc(size, file, line);
  bcopy(str,newstr,size);
  assert(newstr[size-1]=='\0');  /* last character better be null */
  return newstr;
}

typedef struct Unfreed {
  int bytes; 
  char *file;
  int line,count;
} Unfreed;

static int
lookup_loc(const char *name, const int line, const Unfreed *table,
	   const int max)
{
  int i;
  for (i=0; i<max; i++) 
    if (table[i].line==line)
      if(!strcmp(name,table[i].file)) return i;

  /* fall-through */
  return -1;
}

static int
ltbytes (const void *a, const void *b) 
{  return ((const Unfreed *)b)->bytes - ((const Unfreed *)a)->bytes; }

static void
print_unfree_stats(const mtabent *mtabs)
{
  /* print some information about where unfreed blocks were allocated */
  Unfreed *unf=NULL;
  int nlocs=0;
  const mtabent *mtp;
  
  mtp= mtabs;

  /* first build table of files */
  do {
    char *myname= mtp->file;
    int myline= mtp->line;
    int ind;

    if ((ind=lookup_loc(myname,myline,unf,nlocs))<0) {
      /* extend table of pointers */
      if(unf) 
         unf= (Unfreed *)realloc((void *)unf,
                                 (unsigned)sizeof(Unfreed)*(nlocs+1));
      else
         unf= (Unfreed *)malloc((unsigned)sizeof(Unfreed));
      unf[nlocs].file= mtp->file; /* just a reference, no copying */
      unf[nlocs].line= mtp->line; 
      unf[nlocs].bytes= mtp->size;
      unf[nlocs].count= 1;
      nlocs++;
    }
    else {
      /* found entry, got index back */
      unf[ind].bytes+= mtp->size;
      unf[ind].count++;
    }
  } while ( (mtp= mtp->next) );
  /* at this point, unf contains an array of "Unfreed" structures,
     nlocs has size */
  qsort(unf,(unsigned)nlocs,sizeof(Unfreed),ltbytes);
  {
    int i;
    (void)fprintf(stderr,"\n\n   Summary of unfreed blocks by file and line\n");
    (void)fprintf(stderr,
	    " ==========================================================\n");

    for (i=0; i<_MYMALLOC_MAX_UNFREE_PRINT && i<nlocs; i++)
      fprintf(stderr,"%16s:%4d   %10d bytes  (%6d block%s)\n",
	      unf[i].file,unf[i].line,unf[i].bytes,unf[i].count,
	(unf[i].count==1)?"":"s");
    if(nlocs>_MYMALLOC_MAX_UNFREE_PRINT) 
      (void)fprintf(stderr,"(more... %d total locations) \n",nlocs);
  }
}

/* sometimes we need to free things that a foreign library has
   allocated for us .... */
void __free(void *ptr)
{
  free(ptr);
}

#endif /* _DONT_MYMALLOC */
