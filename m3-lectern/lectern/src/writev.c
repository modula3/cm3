/* Copyright 1995 Digital Equipment Corporation. */
/* Distributed only by permission. */
/* Last modified on Tue Aug 20 08:14:42 PDT 1996 by mcjones */

/* Patch for old Linux systems whose library simulation of the writev
   kernel call copies all the arguments onto the stack, overflowing a
   typical Modula-3 thread stack. */

#include <stddef.h>
#include <sys/uio.h>

writev(int filedes, const struct iovec *vector, size_t count)
{
  int i, k, n = 0;
  for(i = 0; i < count; i++)
  {
    k = write(filedes, vector[i].iov_base, vector[i].iov_len);
    if (k < 0) return k;
    n += k;
  }
  return n;
}
