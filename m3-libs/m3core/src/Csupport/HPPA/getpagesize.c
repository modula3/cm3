/* Copyright 1997, Critical Mass, Inc.  All rights reserved. */

#include <unistd.h>

/* getpagesize() : Return the VM page size measured in bytes.
 *
 * Implemented via sysconf(2).
 */
int getpagesize()
{
  return sysconf (_SC_PAGESIZE);
}
