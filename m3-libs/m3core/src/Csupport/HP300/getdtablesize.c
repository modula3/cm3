/* Copyright (C) 1994, Digital Equipment Corporation        */
/* All rights reserved.                                     */
/* See the file COPYRIGHT for a full description.           */
/*                                                          */
/* Last modified on Fri Feb 18 14:13:57 PST 1994 by kalsow  */

#include <sys/types.h>
#include <sys/resource.h>

/* getdtablesize() : Return the maximum number of available file numbers.
 *
 * Implemented through getrlimit(2).
 */
int getdtablesize()
{ struct rlimit rip;

  (void) getrlimit(RLIMIT_NOFILE, &rip);

  return rip.rlim_cur;
}
