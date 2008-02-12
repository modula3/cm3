#if defined(_WIN32) && !defined(__CYGWIN__)
#define pclose hide_pclose
#define popen hide_popen
#define unlink hide_unlink
#endif

#include <stdio.h>

#if defined(_WIN32) && !defined(__CYGWIN__)
#undef pclose
#undef popen
#undef unlink
#endif

FILE* Cstdio__stdout(void)
{
    return stdout;
}

FILE* Cstdio__stderr(void)
{
    return stderr;
}

FILE* Cstdio__stdin(void)
{
    return stdin;
}

#if defined(_WIN32) && !defined(__CYGWIN__)

/* remove dependence on oldnames.lib interesting? */

int __cdecl pclose(FILE * a)
{
    return _pclose(a);
}

FILE * __cdecl popen(const char * a, const char * b)
{
    return _popen(a, b);
}

int __cdecl unlink(const char * a)
{
    return _unlink(a);
}

#endif
