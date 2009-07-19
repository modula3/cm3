#define _CRT_SECURE_NO_DEPRECATE

#include <stdio.h>

#ifdef __cplusplus
extern "C" {
#endif

int Cstdio__feof(FILE* f)
{
    return feof(f);
}

int Cstdio__getc(FILE* f)
{
    return getc(f);
}

int Cstdio__ungetc(int c, FILE* f)
{
    return ungetc(c, f);
}

int Cstdio__putc(int c, FILE* f)
{
    return putc(c, f);
}

int Cstdio__fflush(FILE* f)
{
    return fflush(f);
}

FILE* Cstdio__fdopen(int fd, const char* mode)
{
#ifdef _WIN32
    return _fdopen(fd, mode);
#else
    return fdopen(fd, mode);
#endif
}

FILE* Cstdio__fopen(const char* path, const char* mode)
{
    return fopen(path, mode);
}

int Cstdio__fclose(FILE* f)
{
    return fclose(f);
}

FILE* Cstdio__get_stdin(void)
{
    return stdin;
}

FILE* Cstdio__get_stdout(void)
{
    return stdout;
}

FILE* Cstdio__get_stderr(void)
{
    return stderr;
}

#ifdef __cplusplus
}
#endif
