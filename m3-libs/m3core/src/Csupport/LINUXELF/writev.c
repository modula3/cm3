/* Last modified on Fri May 31 14:51:53 PDT 1996 by heydon  */
/*      modified on Thu Mar  9 14:44:30 PST 1995 by mcjones */

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

/**********************

From: mcjones@pa.dec.com
Date: Wed, 08 Nov 95 13:34:45 -0800
To: m3@pa.dec.com
cc: William Kalsow <kalsow@sctc.com>
Subject: Re: Question on PixmapVBT

Bill Kalsow says:

    If you write a writev() replacement that uses a heap
    allocated bufffer whenever it's passed a big chunk of data,
    your problem will be solved.

I encountered a similar problem in another application running on 
Linux, and I wrote a different writev() replacement, which converts 
a writev() call into a sequence of write() calls.  I'm told this 
isn't always equivalent, depending on the type of socket, but it 
worked for me and avoided the allocation and extra copy.  The code 
I used is attached below.

By the way, I sent messages to all the Linux-kernel discussion groups 
I could find proposing that writev should be promoted to a real kernel 
call, but I never got a single reply.

Paul McJones

*****************/
