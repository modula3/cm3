/* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. */
/* See file COPYRIGHT-CMASS for details. */

#include <unistd.h>

/* getpagesize() : Return the VM page size measured in bytes.
 *
 * Implemented via sysconf(2).
 */
int getpagesize()
{
  return sysconf (_SC_PAGESIZE);
}
