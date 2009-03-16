/* Copyright (C) 1993, Digital Equipment Corporation                  */
/* All rights reserved.                                               */
/* See the file COPYRIGHT for a full description.                     */

/*
On some platforms, such as 32bit Linux and 32bit Solaris,
various functions are defined to have 32bit limits by default.
unless #define _FILE_OFFSET_BITS 64, which only affects C source.

Usually they are also available with the name ending in "64" as well.
    open => open64
    stat => stat64
    etc.

It might take a #define to expose those names to C.
(Just to help motivate why there are so many #defines.)

Therefore, saying, e.g.
<*EXTERNAL*> PROCEDURE ftruncate (fd: int; length: off_t): int;

is wrong, unless you constrain off_t to 32 bits, which is not good.

It would be correct to say:
<*EXTERNAL ftruncate64*> PROCEDURE ftruncate (fd: int; length: off_t): int;

However that is not portable.
So use these wrappers instead.
*/

/* This is the point. */
#define _FILE_OFFSET_BITS 64

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <assert.h>

#ifdef __cplusplus
extern "C" {
#endif

void Unix__Assertions()
{
    assert(sizeof(mode_t) <= sizeof(int));
}

/* open doesn't take any off_t parameter, but there is open64, that
#define _FILE_OFFSET_BITS 64 maps open to. */
int Unix__open(const char* path, int flags, /*mode_t*/int mode)
{
    return open(path, flags, mode);
}

/* wrapped in case passing mode_t vs. int varies */
int Unix__mkdir(const char* path, /*mode_t*/int mode)
{
    return mkdir(path, mode);
}

int Unix__ftruncate(int fd, off_t length)
{
    return ftruncate(fd, length);
}

off_t Unix__lseek(int fd, off_t offset, int whence)
{
    return lseek(fd, offset, whence);
}

#ifdef __cplusplus
} /* extern C */
#endif

