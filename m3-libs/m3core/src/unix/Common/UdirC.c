#include <sys/types.h>
#include <dirent.h>
#include <assert.h>
#include <stddef.h>
#ifndef _MSC_VER
#define __int32 int
#define __int64 long long
#endif
typedef unsigned __int32 UINT32;
typedef struct dirent dirent_t;

typedef struct _m3_dirent_t {
#ifdef __OpenBSD__
    UINT32 d_ino;
#else
    __int64 d_ino;
    char pad[3]; /* not portable */
#endif
    char d_name[1];
} m3_dirent_t;

volatile m3_dirent_t* m3_readdir(DIR* dir)
{
    volatile m3_dirent_t* m3;
    volatile dirent_t* d;

    d = readdir(dir);
    if (!d)
        return 0;

    m3 = (m3_dirent_t*) (((char*)d->d_name) - offsetof(m3_dirent_t, d_name));

#ifdef __OpenBSD__
    m3->d_ino = d->d_fileno;
#else
    m3->d_ino = d->d_ino;
#endif
    return m3;
}
