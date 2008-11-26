#include <sys/types.h>
#include <dirent.h>
#include <assert.h>
#include <stddef.h>

#ifndef _MSC_VER
#define __int64 long long
#endif

typedef struct dirent dirent_t;

typedef struct _m3_dirent_t {
    __int64 d_ino;
    char pad_for_align[3]; /* maybe not portable */
    char d_name[1];
} m3_dirent_t;

m3_dirent_t* m3_readdir(DIR* dir)
{
    __int64 d_ino;
    m3_dirent_t* m3;
    dirent_t* d;

    d = readdir(dir);
    if (!d)
        return 0;

    d_ino = d->d_ino;
    m3 = (m3_dirent_t*) (((char*)d->d_name) - offsetof(m3_dirent_t, d_name));
    assert((((size_t) m3) & (sizeof(__int64) - 1)) == 0);
    m3->d_ino = d_ino;
    return m3;
}

#ifdef UDIR_MAIN

#include <stdio.h>

int main()
{
    dirent_t* d = 0;
#define X(x) printf(#x " 0x%x\n", (unsigned) (size_t) x)
    X(&d->d_name); /* This should be > 8 and evenly divisible by 8, else revisit;
                    the first compromise is to go down to size_t, if 32 bit platforms only have 32 bit ino. */
    X(sizeof(d->d_ino));
    return 0;
}
#endif
