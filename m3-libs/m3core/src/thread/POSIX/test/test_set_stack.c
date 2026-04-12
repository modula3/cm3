// This is a test program for DJGPP and setting stack pointer.
// The technique is likely portable to not-NT, as long as
// alloca will subtract an arbitrary size_t/ptrdiff_t to
// to reach from current static to malloc result.
//
// This does not work for example on Linux/amd64, unless you
// say gcc -fno-stack-clash-protection, which we should be
// to use selectively.
//
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <malloc.h>
#include <sys/mman.h>

void f (char* p)
{
    char* q = (char*)alloca (1);
    printf ("sp:%p\n", q);
    printf ("1dist:%ld\n", p - q);
    printf ("2dist:%ld\n", -(p - q));
} 

int main()
{
    size_t pagesize = getpagesize ();
    size_t size = 1L << 20;
    char* p = 0;
    char* pfree = 0;
    char* full_end = 0;
    int er = 0;
    size_t dist = 0;
    size_t align = 0;
    char* volatile sp = 0;
    char* q = (char*)alloca (1);

    // malloc might not be aligned, so we want roughly two pages
    // at each end, one to align, one to protect
    size_t extra_size = size + pagesize * 4;

    printf ("pagesize:%lx\n", pagesize);
    printf ("size:%lx\n", size);
    f ((char*)&p);

    pfree = (char*)malloc (extra_size);
    full_end = pfree + extra_size;
    p = pfree;
    align = (size_t)pfree % pagesize;
    printf ("align:%p\n", (char*)align);
    printf ("malloc start:%p\n", pfree);
    printf ("malloc end:%p\n", pfree + extra_size);

    p += pagesize - align;

    printf ("malloc aligned start:%p\n", p);
    printf ("malloc aligned end:%p\n",   full_end - align);

    er = mprotect (p, pagesize, PROT_NONE);
    printf ("er:%d\n", er);
    er = mprotect (full_end - align - pagesize, pagesize, PROT_NONE);
    printf ("er:%d\n", er);

    fflush (0);

    p += pagesize;
    dist = q - (full_end - align - pagesize);
    printf ("3dist:%ld %lx\n", dist, dist);
    sp = (char*)alloca (dist);
    printf ("\nnew sp:%p\n", sp);
    f ((char*)&p);
    f (sp);
}
