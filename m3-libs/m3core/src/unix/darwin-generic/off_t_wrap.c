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

long
m3_asLong (off_t val)
{
  long v = (off_t)val;
  if (v != val) return -1;
  return v;
}

void
m3_assignOffT (off_t* dest, long src)
{
  *dest = (off_t)src;
}

void *
m3_mmap(caddr_t addr, size_t len, int prot, int flags, int fd, int offset)
{
  off_t off = (off_t)offset;
  return mmap(addr, len, prot, flags, fd, off);
}

int m3_lseek(int fildes, int offset, int whence)
{
  off_t off = (off_t)offset;
  return m3_asLong(lseek(fildes, off, whence));
}

int m3_pread(int d, void *buf, size_t nbytes, int offset)
{
  off_t off = (off_t)offset;
  return pread(d, buf, nbytes, offset);
}

int m3_profil(char *samples, size_t size, u_long offset, u_int scale)
{
  off_t off = (off_t)offset;
  return profil(samples, size, offset, scale);
}

int m3_pwrite(int d, const void *buf, size_t nbytes, int offset)
{
  off_t off = (off_t)offset;
  return pwrite(d, buf, nbytes, offset);
}

int m3_truncate(const char *path, int length)
{
  off_t len = (off_t) length;
  return truncate(path, len);
}

int m3_ftruncate(int fd, int length)
{
  off_t len = (off_t) length;
  return ftruncate(fd, length);
}
