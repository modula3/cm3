/* Copyright (C) 1993, Digital Equipment Corporation                         */
/* All rights reserved.                                                      */
/* See the file COPYRIGHT for a full description.                            */

#include <sys/types.h>

extern void bcopy();

void *memmove(dst, src, count)
  void *dst;
  void *src;
  size_t count;
{
  bcopy((char *)src, (char *)dst, (int)count);

  return src;
}

/**
  According to the SunOS manpages, bcopy correctly handles
  a possible overlap, it just doesn't return anything.

    - Bert Laverman
**/

