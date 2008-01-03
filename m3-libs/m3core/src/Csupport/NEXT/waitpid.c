/* Copyright (C) 1994, Digital Equipment Corporation              */
/* All rights reserved.                                           */
/* See the file COPYRIGHT for a full description.                 */
/*                                                                */
/* Last modified on Fri Sep  9 08:29:27 PDT 1994 by kalsow        */
/*                                                                */
/* contributed by Thomas Neumann <tom@smart.ruhr.de>, 07.09.1994  */

#include <sys/types.h>
#include <sys/wait.h>

int
waitpid(int pid, int *stat_loc, int options) {
  return wait4(pid, (union wait *)stat_loc, options, (struct rusage *)0);
}
