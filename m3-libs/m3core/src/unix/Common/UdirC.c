#include "m3unix.h"

#include <sys/types.h>
#include <dirent.h>
#include <assert.h>
#include <stddef.h>
#if defined(__CYGWIN__)
typedef struct dirent dirent_t;
#else
typedef struct dirent64 dirent_t;
#endif

/* The simplest thing is to just always make this UINT64.
However, if the underlying platform makes it 32bits, and
d_name is 32bit aligned but not 64bit aligned, then 32bits
is preferable; adjust via #ifdef as needed. */
typedef UINT64 m3_ino_t;

typedef struct _m3_dirent_t {
    m3_ino_t d_ino;
    char pad[3]; /* not portable; adjust via #ifdef as needed */
    char d_name[1];
} m3_dirent_t;

volatile m3_dirent_t* m3_readdir(DIR* dir)
{
    volatile m3_dirent_t* m3;
    volatile dirent_t* d;

#if defined(__CYGWIN__)
    d = readdir(dir);
#else
    d = readdir64(dir);
#endif
    if (!d)
        return 0;

    m3 = (m3_dirent_t*) (((char*)d->d_name) - offsetof(m3_dirent_t, d_name));

    /* make sure there was actually room */
    assert(((char*)m3) >= ((char*)d));

    /* and that it is aligned */
    assert((((size_t)m3) & (sizeof(m3_ino_t) - 1)) == 0);

    m3->d_ino = d->d_ino;
    return m3;
}
