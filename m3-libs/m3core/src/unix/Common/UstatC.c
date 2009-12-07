#include "m3unix.h"

#ifdef __cplusplus
extern "C" {
#endif

struct _m3_stat_t
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

static int m3stat_from_stat(int result, m3_stat_t* m3st, stat_t* st)
{
    assert(result == 0 || result == -1);
    if (result == 0)
    {
#if defined(__APPLE__) && defined(__arm)
        m3st->dev = st->dev;
        m3st->ino = st->ino;
        m3st->mtime = st->mtime;
        m3st->nlink = st->nlink;
        m3st->rdev = st->rdev;
        m3st->size = st->size;
        m3st->gid = st->gid;
        m3st->mode = st->mode;
        m3st->uid = st->uid;
#ifdef HAS_STAT_FLAGS
        m3st->flags = st->flags;
#else
        m3st->flags = 0;
#endif
#else
        m3st->dev = st->st_dev;
        m3st->ino = st->st_ino;
        m3st->mtime = st->st_mtime;
        m3st->nlink = st->st_nlink;
        m3st->rdev = st->st_rdev;
        m3st->size = st->st_size;
        m3st->gid = st->st_gid;
        m3st->mode = st->st_mode;
        m3st->uid = st->st_uid;
#ifdef HAS_STAT_FLAGS
        m3st->flags = st->st_flags;
#else
        m3st->flags = 0;
#endif
#endif
    }
    return result;
}

int Ustat__stat(const char* path, m3_stat_t* m3st)
{
    stat_t st;
    return m3stat_from_stat(stat(path, (struct stat*)&st), m3st, &st);
}

#ifndef _WIN32
int Ustat__lstat(const char* path, m3_stat_t* m3st)
{
    stat_t st;
    return m3stat_from_stat(lstat(path, (struct stat*)&st), m3st, &st);
}
#endif

int Ustat__fstat(int fd, m3_stat_t* m3st)
{
    stat_t st;
    return m3stat_from_stat(fstat(fd, (struct stat*)&st), m3st, &st);
}

#ifdef HAS_STAT_FLAGS

M3WRAP2(int, chflags, const char*, unsigned long)
M3WRAP2(int, fchflags, int, unsigned long)

#endif

#ifdef __cplusplus
} /* extern "C" */
#endif
