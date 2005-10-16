/* Copyright (C) 1994, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */
/*                                                             */
/* Last modified on Thu Jan 12 09:28:20 PST 1995 by kalsow     */
/* Contributed by Olaf Wagner, Jan 1995                        */

#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/mman.h>

/* off_t is an int32 in Modula-3 */
typedef long m3_off_t;

/* This function converts a Utypes.off_t into an INTEGER. Utypes.off_t
 * is defined as an int32_t, and i386 is a 32-bit platform, so we can
 * expect that INTEGER has the same size and thus is the same type as
 * Utypes.off_t.
 */
long
m3_asLong (m3_off_t val)
{
  return val;
}

void
m3_assignOffT (off_t* dest, long src)
{
  *dest = (off_t)src;
}
caddr_t m3_mmap(caddr_t addr, size_t len, int prot, int flags, int fd, long offset)
{
  off_t off = (off_t) offset;
  return mmap(addr, len, prot, flags, fd, off);
}

long m3_lseek(int fildes, long offset, int whence)
{
  off_t off = (off_t) offset;
  return (long) lseek(fildes, off, whence);
}

int m3_truncate(const char *path, long length)
{
  off_t len = (off_t) length;
  return truncate(path, len);
}

int m3_ftruncate(int fd, long length)
{
  off_t len = (off_t) length;
  return ftruncate(fd, length);
}

/* added to avoid problems with the ellipsis... */

int m3_fcntl(int fd, int cmd, int arg)
{
  return fcntl(fd, cmd, arg);
}

int m3_open(const char *path, int flags, mode_t mode)
{
  return open(path, flags, mode);
}
