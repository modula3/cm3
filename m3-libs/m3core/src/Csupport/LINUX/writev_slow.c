/* Last modified on Mon Jun  3 17:46:06 PDT 1996 by heydon */

/* This is a replacement version of the "writev" function for Linux. The
   default implementation supplied with Linux allocates a large buffer
   on the stack, copies the bytes in its "vector" argument onto the
   stack, and then executes a single "write" system call on the buffer.
   Since the buffer allocated on the stack is often quite large, this
   can easily cause thread stacks to overflow.

   This implementation is like the default version, but it allocates its
   buffer on the heap instead of the stack. This file is based on
   "libc/posix/writev.c" from libc-5.0.9 of the Linux distribution. */

/* Copyright (C) 1991, 1992 Free Software Foundation, Inc.
This file is part of the GNU C Library.

The GNU C Library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

The GNU C Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with the GNU C Library; see the file COPYING.LIB.  If
not, write to the Free Software Foundation, Inc., 675 Mass Ave,
Cambridge, MA 02139, USA.  */

#include <ansidecl.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/uio.h>

/* Write data pointed by the buffers described by VECTOR, which
   is a vector of COUNT `struct iovec's, to file descriptor FD.
   The data is written in the order specified.
   Operates just like `write' (see <unistd.h>) except that the data
   are taken from VECTOR instead of a contiguous buffer.  */
int
DEFUN(writev, (fd, vector, count),
      int fd AND CONST struct iovec *vector AND size_t count)
{
  char *buffer;
  register char *bp;
  size_t bytes, to_copy;
  register size_t i;

  /* Find the total number of bytes to be written.  */
  bytes = 0;
  for (i = 0; i < count; ++i)
    bytes += vector[i].iov_len;

  if (bytes == 0)
    return 0;

  /* Allocate a temporary buffer to hold the data.  */
  buffer = (char *) malloc(bytes);

  /* Copy the data into BUFFER.  */
  to_copy = bytes;
  bp = buffer;
  for (i = 0; i < count; ++i)
    {
#define	min(a, b)	((a) > (b) ? (b) : (a))
      size_t copy = min(vector[i].iov_len, to_copy);

      (void) memcpy((PTR) bp, (PTR) vector[i].iov_base, copy);

      bp += copy;
      to_copy -= copy;
      if (bytes == 0)
	break;
    }

  return write(fd, buffer, bytes);
  free((void *)buffer);
}
