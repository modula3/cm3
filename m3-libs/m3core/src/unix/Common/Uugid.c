/* Copyright (C) 1990, Digital Equipment Corporation.         */
/* All rights reserved.                                       */
/* See the file COPYRIGHT for a full description.             */

#ifndef INCLUDED_M3CORE_H
#include "m3core.h"
#endif

#ifndef _WIN32

#undef M3MODULE /* Support concatenating multiple .c files. */
#define M3MODULE Uugid
M3WRAP0(m3_uid_t, geteuid)
M3WRAP2(int, setreuid, m3_uid_t, m3_uid_t)
M3WRAP0(m3_gid_t, getegid)

static int
havegroup(gid_t qgid)
{
  gid_t list[NGROUPS_MAX];
  int   n;
  int   i;
  
  for (i = 0; i < NGROUPS_MAX; ++i)
    list[i] = 0;

  n = getgroups(NGROUPS_MAX, list);
  
  if (n < 0) return -1;
  
  for (i = 0; i < n; ++i)
    if (qgid == list[i])
      return 1;
  
  return 0;
}

M3WRAP1(int, havegroup, m3_gid_t)

#endif
