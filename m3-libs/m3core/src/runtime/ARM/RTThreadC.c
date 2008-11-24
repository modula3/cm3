/* Copyright (C) 1990, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */
/* Last modified on Mon Dec  6 16:08:07 PST 1993 by kalsow     */
/*      modified on Tue Jan 19 15:20:48 PST 1993 by burrows    */

/* This file implements the coroutine transfer: RTThread.Transfer */

#include <setjmp.h>

RTThread__Transfer (from, to)
jmp_buf *from, *to;
{
  if (_setjmp(*from) == 0) _longjmp (*to, 1);
}
