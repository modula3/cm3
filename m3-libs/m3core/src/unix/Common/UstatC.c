#define __USE_LARGEFILE64

#include <assert.h>

#ifndef _MSC_VER
#define __int64 long long
#endif
#ifdef _WIN64
typedef __int64 INTEGER;
#else
typedef long INTEGER;
#endif
typedef __int64 LONGINT;

typedef struct _m3_stat_t {
/* Sorted by size, then by name; make everything LONGINT if possible, else INTEGER;
Limit on LONGINT is compatibility with existing Modula-3 code. Blowing up the sizes
larger than necessary is a slight deoptimization for the sake of simplicity and
commonality. */
    LONGINT st_mtime;
    LONGINT st_rdev;
    LONGINT st_size;
    INTEGER st_gid;
    INTEGER st_mode;
    INTEGER st_uid;
} m3_stat_t;

void m3stat_set_mtime(m3_stat_t* m3st, LONGINT mtime) { m3st->st_mtime = mtime; }
void m3stat_set_rdev(m3_stat_t* m3st, LONGINT rdev) { m3st->st_rdev = rdev; }
void m3stat_set_size(m3_stat_t* m3st, LONGINT size) { m3st->st_size = size; }
void m3stat_set_gid(m3_stat_t* m3st, INTEGER gid) { m3st->st_gid = gid; }
void m3stat_set_mode(m3_stat_t* m3st, INTEGER mode) { m3st->st_mode = mode; }
void m3stat_set_uid(m3_stat_t* m3st, INTEGER uid) { m3st->st_uid = uid; }

/* These headers sometimes #define away our identifiers st_mtime, etc.
   That is why we have the wrapper functions. */
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

typedef struct stat stat_t;

int m3stat_from_stat(int result, m3_stat_t* m3st, stat_t* st)
{
    assert(result == 0 || result == -1);
    if (result == 0)
    {
        m3stat_set_mode(m3st, st->st_mode);
        m3stat_set_mtime(m3st, st->st_mtime);
        m3stat_set_size(m3st, st->st_size);
        m3stat_set_rdev(m3st, st->st_rdev);
        m3stat_set_uid(m3st, st->st_uid);
        m3stat_set_gid(m3st, st->st_gid);
    }
    return result;
}

int m3_stat(const char* path, m3_stat_t* m3st)
{
    stat_t st;
    return m3stat_from_stat(stat(path, &st), m3st, &st);
}

int m3_lstat(const char* path, m3_stat_t* m3st)
{
    stat_t st;
    return m3stat_from_stat(lstat(path, &st), m3st, &st);
}

int m3_fstat(int fd, m3_stat_t* m3st)
{
    stat_t st;
    return m3stat_from_stat(fstat(fd, &st), m3st, &st);
}
