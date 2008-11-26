#ifndef _MSC_VER
#define __int64 long long
#endif

typedef struct _m3_stat_t {
    __int64 st_mode;
    __int64 st_mtime;
    __int64 st_size;
} m3_stat_t;

__inline void m3stat_set_mode(m3_stat_t* m3st, __int64 mode) { m3st->st_mode = mode; }
__inline void m3stat_set_mtime(m3_stat_t* m3st, __int64 mtime) { m3st->st_mtime = mtime; }
__inline void m3stat_set_size(m3_stat_t* m3st, __int64 size) { m3st->st_size = size; }

/* These headers sometimes #define away our identifiers st_mtime, etc.
   That is why we have the wrapper functions. */
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

typedef struct stat stat_t;

int m3stat_from_stat(int result, m3_stat_t* m3st, stat_t* st)
{
    m3stat_set_mode(m3st, st->st_mode);
    m3stat_set_mtime(m3st, st->st_mtime);
    m3stat_set_size(m3st, st->st_size);
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
