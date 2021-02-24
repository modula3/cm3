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
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include "misc.h"

#if __cplusplus
extern "C" {
#endif

/*-------------------------------------------------------------------------
 * print error message and die.
 *-----------------------------------------------------------------------*/
void fatal_error (const char *s, ...)
{
  va_list ap;

  fprintf (stderr, "FATAL: ");
  va_start (ap, s);
  vfprintf (stderr, s, ap);
  va_end (ap);
  fprintf (stderr, "\n");
  exit (1);
}

char *Strdup (char *s)
{
  char *t;
  size_t length;
  length = strlen(s) + 1;
  MALLOC (t, char, length);
  memcpy (t, s, length);
  return t;
}

#if __cplusplus
} /* extern "C" */
#endif
