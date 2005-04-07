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
