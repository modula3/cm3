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
    }
    return result;
}

int Ustat__stat(const char* path, m3_stat_t* m3st)
{
    stat_t st;
    return m3stat_from_stat(stat(path, &st), m3st, &st);
}

int Ustat__lstat(const char* path, m3_stat_t* m3st)
{
    stat_t st;
    return m3stat_from_stat(lstat(path, &st), m3st, &st);
}

int Ustat__fstat(int fd, m3_stat_t* m3st)
{
    stat_t st;
    return m3stat_from_stat(fstat(fd, &st), m3st, &st);
}

#ifdef HAS_STAT_FLAGS

int Ustat__chflags(const char* path, unsigned long flags)
{
    return chflags(path, flags);
}

int Ustat__fchflags(int fd, unsigned long flags)
{
    return fchflags(fd, flags);
}

#endif

#ifdef __cplusplus
} /* extern "C" */
#endif
