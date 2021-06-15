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

#ifndef INCLUDED_M3CORE_H
#include "m3core.h"
#endif
#ifdef _WIN32
#include <windows.h>
#endif
#undef M3MODULE /* Support concatenating multiple .c files. */
#define M3MODULE Unix

#ifdef __cplusplus
extern "C"
{
#endif

void __cdecl
Unix__Assertions(void)
{
    /* make sure things are normal */
    M3_STATIC_ASSERT(CHAR_BIT == 8);
    M3_STATIC_ASSERT(sizeof(short) == 2);
    M3_STATIC_ASSERT(sizeof(int) == 4);
    M3_STATIC_ASSERT((sizeof(long) == 4) || (sizeof(long) == 8));
    M3_STATIC_ASSERT((sizeof(void*) == 4) || (sizeof(void*) == 8));
    M3_STATIC_ASSERT((sizeof(size_t) == 4) || (sizeof(size_t) == 8));
    M3_STATIC_ASSERT(sizeof(ptrdiff_t) == sizeof(size_t));
    M3_STATIC_ASSERT(sizeof(void*) <= sizeof(WORD_T)); // C backend can have 64bit word and 32bit pointer
#if !defined(_WIN64) && !defined(__vms)
    M3_STATIC_ASSERT(sizeof(void*) == sizeof(long));
    M3_STATIC_ASSERT(sizeof(size_t) == sizeof(long));
#endif

#if defined(_MSC_VER) || defined(__DECC)
    M3_STATIC_ASSERT(sizeof(__int64) == 8);
#else
    M3_STATIC_ASSERT(sizeof(long long) == 8);
#endif

#ifndef _WIN32

/* make sure all the Modula-3 types are large enough */

#define CHECK_M3_TYPE_SIZE(x) M3_STATIC_ASSERT(sizeof(m3_##x) >= sizeof(x))
#define IS_TYPE_SIGNED(x)  (((x)-1) < (x)0)

    CHECK_M3_TYPE_SIZE(dev_t);
    CHECK_M3_TYPE_SIZE(gid_t);
    CHECK_M3_TYPE_SIZE(ino_t);
    CHECK_M3_TYPE_SIZE(mode_t);
    CHECK_M3_TYPE_SIZE(nlink_t);
    CHECK_M3_TYPE_SIZE(off_t);
    CHECK_M3_TYPE_SIZE(pid_t);
    CHECK_M3_TYPE_SIZE(pthread_t);
    CHECK_M3_TYPE_SIZE(uid_t);

    M3_STATIC_ASSERT(IS_TYPE_SIGNED(pid_t));
#endif
    Usocket__Assertions();
}

M3WRAP3_(int, open, const char*, int, m3_mode_t)
M3WRAP1_(m3_mode_t, umask, m3_mode_t)
M3WRAP2_(int, chmod, const char*, m3_mode_t)
M3WRAP2_(int, creat, const char*, m3_mode_t)
M3WRAP1_(int, dup, int)
M3WRAP1(int, system, const char*)
M3WRAP1_(int, isatty, int)
M3WRAP2(int, rename, const char*, const char*)
M3WRAP1_(int, rmdir, const char*)
M3WRAP1_(int, unlink, const char*)
M3WRAP2_(int, access, const char*, int)
M3WRAP1_(int, chdir, const char*)
M3WRAP1_(int, close, int)
M3WRAP2_(int, dup2, int, int)

#ifdef __sun
M3WRAP1(m3_off_t, tell, int)
#endif

#ifndef _WIN32
M3WRAP3(int, chown, const char*, m3_uid_t, m3_gid_t)
M3WRAP3(int, fchown, int, m3_uid_t, m3_gid_t)
M3WRAP2(int, truncate, const char*, m3_off_t)
M3WRAP2(int, ftruncate, int, m3_off_t)
M3WRAP3(INTEGER, readlink, const char*, char*, INTEGER)
M3WRAP2(int, symlink, const char*, const char*)
M3WRAP1(int, fsync, int)
M3WRAP0(int, getdtablesize)
M3WRAP0(int, getpagesize)
#if 0 /* deprecated, errors on MacOSX 10.10.4 Yosemite, worked on 10.5.8. */
M3WRAP1(void*, sbrk, INTEGER)
#endif
M3WRAP3(int, execve, const char*, char**, char**)
M3WRAP1(unsigned, sleep, unsigned)
M3WRAP3(m3_off_t, lseek, int, m3_off_t, int)
M3WRAP2(int, mkdir, const char*, m3_mode_t)
M3WRAP2(int, gethostname, char*, WORD_T)
M3WRAP2(char*, getcwd, char*, WORD_T)

#ifndef __vms

M3WRAP2(int, fchmod, int, m3_mode_t)
M3WRAP3(int, mknod, const char*, m3_mode_t, m3_dev_t)

#if 0 /* See RTProcess.Fork. */
M3_DLL_EXPORT m3_pid_t __cdecl
Unix__fork(void)
{
#ifdef __sun
    /* Prior to Solaris 2.10, fork() was fork1() or forkall() depending
     * on which library used. In Solaris 2.10, fork() is always fork1(),
     * and a separate forkall() is available. fork1()'s declaration
     * does have some #ifdef guards around it; hopefully ok.
     */
    return fork1();
#else
    return fork();
#endif
}
#endif

#endif /* vms */
#endif /* win32 */

M3_DLL_EXPORT void __cdecl
Unix__underscore_exit(int exit_code)
{
    _exit(exit_code);
}

M3_DLL_EXPORT void __cdecl
Unix__exit(int i)
{
    exit(i);
}

#ifdef _WIN32

#if 0
M3_DLL_EXPORT char* __cdecl
Unix__getcwd(char* name, WORD_T len)
{
    assert(len < INT_MAX);
    return _getcwd(name, (int)len);
}

M3_DLL_EXPORT int __cdecl
Unix__gethostname(char* name, WORD_T len)
{
    assert(len < INT_MAX);
    return gethostname(name, (int)len);
}
#endif

M3_DLL_EXPORT int __cdecl
Unix__mkdir(const char* path, m3_mode_t mode)
{
    return _mkdir(path);
}

M3_DLL_EXPORT int __cdecl
UnixC__pipe (int* files)
{
    return _pipe(files, 0, _O_BINARY);
}

#if _MSC_VER >= 1000

M3_DLL_EXPORT m3_off_t __cdecl
Unix__lseek(int fd, m3_off_t offset, int whence)
{
    return _lseeki64(fd, offset, whence);
}

M3_DLL_EXPORT m3_off_t __cdecl
Unix__tell(int fd)
{
    return _telli64(fd);
}

#endif

#else

typedef struct m3_flock_t {
/* sorted by size and then name
   This must match between Unix.i3 and UnixC.c. */
  LONGINT len;
  LONGINT start;
  INTEGER pid;
  INTEGER type;
  INTEGER whence;
} m3_flock_t;

M3_DLL_EXPORT int __cdecl
Unix__fcntl(int fd, INTEGER request, INTEGER m3_arg)
/* fcntl is actually fcntl(fd, request, ...).
 * Wrapper is needed on some systems to handle varargs.
 * See http://edoofus.blogspot.com/2008/08/interesting-bug-unbreaking-cvsupamd64.html.
 */
{
/* Errno preservation for success:
 * work around a bug in the Solaris-2 'libsocket' library 
 * which redefines 'fcntl' in such a way as to zero out 'errno' if the
 * call is successful.
 * See m3-libs/m3core/src/unix/solaris-2-x/Unix.m3.
 */
    int e = errno;
    struct flock native_lock = { 0 };
    m3_flock_t* m3_lock = { 0 };
    void* native_arg = (void*)m3_arg;
    int r = { 0 };

    memset(&native_lock, 0, sizeof(native_lock));
    if (m3_arg)
    {
      switch (request)
      {
      case F_GETLK: case F_SETLK: case F_SETLKW:
        m3_lock = (m3_flock_t*)m3_arg;
        native_lock.l_len = m3_lock->len;
        native_lock.l_pid = m3_lock->pid;
        native_lock.l_start = m3_lock->start;
        native_lock.l_type = m3_lock->type;
        native_lock.l_whence = m3_lock->whence;
        native_arg = &native_lock;
        break;
      }
    }
    r = fcntl(fd, request, native_arg);
    if (r == 0)
        errno = e;
   if (m3_lock)
   {
      m3_lock->len = native_lock.l_len;
      m3_lock->pid = native_lock.l_pid;
      m3_lock->start = native_lock.l_start;
      m3_lock->type = native_lock.l_type;
      m3_lock->whence = native_lock.l_whence;
    }
    return r;
}

M3_DLL_EXPORT int __cdecl
Unix__ioctl(int fd, INTEGER request, ADDRESS argp)
/* ioctl is varargs. See fcntl. */
{
/* Errno preservation for success:
 * Work around a bug in the Solaris-2 'libsocket' library 
 * which redefines 'ioctl' in such a way as to zero out 'errno' if the
 * call is successful.
 * See m3-libs/m3core/src/unix/solaris-2-x/Unix.m3.
 */
    int e = errno;
    int r = ioctl(fd, request, argp);
    if (r == 0)
        errno = e;
    return r;
}

#endif

#ifndef _WIN32
#undef M3MODULE /* Support concatenating multiple .c files. */
#define M3MODULE UnixC

#if 0 // This would be nice, but structural type equivalence gets
      // in the way. There are other equivalent types
      // and the names clash.
int __cdecl Unix__pipe (Unix__PipeArray* files)
{
    return pipe (files->files);
}

#else

M3WRAP1(int, pipe, int*)

#endif
#endif

#ifdef __cplusplus
} /* extern C */
#endif
