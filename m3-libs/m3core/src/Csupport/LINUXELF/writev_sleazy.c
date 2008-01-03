/* Last modified on Mon Jun  3 17:25:35 PDT 1996 by heydon  */
/*      modified on Thu Mar  9 14:44:30 PST 1995 by mcjones */

/* This is a replacement version of the "writev" function for Linux. The
   default implementation supplied with Linux allocates a large buffer
   on the stack, copies the bytes in its "vector" argument onto the
   stack, and then executes a single "write" system call on the buffer.
   Since the buffer allocated on the stack is often quite large, this
   can easily cause thread stacks to overflow.

   This implementation does not allocate a stack buffer, but rather
   invokes the "write" system call many times, once for each of the
   "count" buffers in "vector". This works when making calls on an X
   server, but may not work when writing to file descriptors
   corresponding to low-level ``devices'' like UDP sockets where making
   multiple "write" calls violates the "writev" semantics. */

#include <stddef.h>
#include <sys/uio.h>

int writev(int filedes, const struct iovec *vector, size_t count)
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
