/*************************************************************************
 *
 *  (c) 1996 California Institute of Technology
 *  Department of Computer Science
 *  Pasadena, CA 91125.
 *  All Rights Reserved
 *
 *  $Id$
 *
 *************************************************************************/
#ifndef __MISC_H__
#define __MISC_H__

#include <stdlib.h>

#if !defined(__FreeBSD__) && !defined(__APPLE__)
#include <malloc.h>
#endif

extern void fatal_error (const char *s, ...);

#define MALLOC(a,b,c)  do { if(!(a=(b*)malloc(sizeof(b)*(c)))) fatal_error("malloc failed, size=%d", sizeof(b)*(c)); }while(0)
  /* allocate "c" number of elements of type "b" and assign the resulting
     storage to "a". */

#define REALLOC(a,b,c) do { if(!(a=(b*)realloc(a,sizeof(b)*(c)))) fatal_error("realloc failed, size=%d", sizeof(b)*(c)); } while (0)
  /* reallocate "c" number of elements of type "b" and assign the resulting
     storage to "a". */

#define FREE(a)  free(a)
  /* free storage for "a" */

#define Assert(a,b) do{if(!(a)) fatal_error("In file %s, line %d. %s", __FILE__, __LINE__, b); }while(0)
  /*  Assert "a". If assertion failed, print message "b" and die. */

char *Strdup (char *s);

#endif
