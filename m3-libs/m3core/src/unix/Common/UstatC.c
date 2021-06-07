#ifndef INCLUDED_M3CORE_H
#include "m3core.h"
#endif

#if M3_HAS_VISIBILITY
#ifdef __APPLE__
#pragma GCC visibility push(default)
#else
#pragma GCC visibility push(protected)
#endif
#endif

#ifdef __cplusplus
extern "C" {
#endif

struct m3_stat_t
{
/*
This MUST match Ustat.i3.

Sorted by size, then by name; make everything LONGINT if possible, else INTEGER;
Limit on LONGINT is compatibility with existing Modula-3 code. Blowing up the sizes
larger than necessary is a slight deoptimization for the sake of simplicity and
commonality.
    
"st_" prefix is omitted from the names in case they are macros, which does happen */

    LONGINT dev;
    LONGINT ino;
    LONGINT mtime;
    LONGINT nlink;
    LONGINT rdev;
    LONGINT size;
    INTEGER flags;
    INTEGER gid;
    INTEGER mode;
    INTEGER uid;
};

static int __cdecl m3stat_from_stat(int result, m3_stat_t* m3st, struct stat* st)
{
    assert(result == 0 || result == -1);
    if (result == 0)
    {
#ifdef __vms
        /* These are strings on VMS, though they
         * appear to be good enough for our use.
         * Calling stat(/dev/null) multiple times gives
         * back the same pointers.
         */
        m3st->dev = (LONGINT)(INTEGER)st->st_dev;
        m3st->rdev = (LONGINT)(INTEGER)st->st_rdev;
#else
        m3st->dev = st->st_dev;
        m3st->rdev = st->st_rdev;
#endif
        m3st->ino = st->st_ino;
        m3st->mtime = st->st_mtime;
        m3st->nlink = st->st_nlink;
        m3st->size = st->st_size;
        m3st->gid = st->st_gid;
        m3st->mode = st->st_mode;
        m3st->uid = st->st_uid;
#ifdef HAS_STAT_FLAGS
        m3st->flags = st->st_flags;
#else
        m3st->flags = 0;
#endif
    }
    return result;
}

M3_DLL_EXPORT
int
__cdecl
Ustat__stat(const char* path, m3_stat_t* m3st)
{
    struct stat st;
    return m3stat_from_stat(stat(path, &st), m3st, &st);
}

M3_DLL_EXPORT
int
__cdecl
Ustat__lstat(const char* path, m3_stat_t* m3st)
{
    struct stat st;
    return m3stat_from_stat(lstat(path, &st), m3st, &st);
}

M3_DLL_EXPORT
int
__cdecl
Ustat__fstat(int fd, m3_stat_t* m3st)
{
    struct stat st;
    return m3stat_from_stat(fstat(fd, &st), m3st, &st);
}

#ifdef HAS_STAT_FLAGS

M3WRAP2(int, chflags, const char*, unsigned long)
M3WRAP2(int, fchflags, int, unsigned long)

#endif

#ifdef __cplusplus
} /* extern "C" */
#endif

#if M3_HAS_VISIBILITY
#pragma GCC visibility pop
#endif
