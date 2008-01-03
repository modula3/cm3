/* Copyright (C) 1994, Digital Equipment Corporation        */
/* All rights reserved.                                     */
/* See the file COPYRIGHT for a full description.           */
/*                                                          */
/* Last modified on Fri Feb 18 14:13:48 PST 1994 by kalsow  */

#include <sys/types.h>
#include <time.h>
#include <utime.h>

/* utimes() : set access and modify times for a file.
 *
 * Implemented by calling utime(2). Note that the micro-second
 * parts of the times are ignored.
 */
int utimes(file, tvp)
  char *file;
  struct timeval *tvp;
{ struct utimbuf t;

  t.actime  = tvp [0].tv_sec;
  t.modtime = tvp [1].tv_sec;

  return utime(file, &t);
}

