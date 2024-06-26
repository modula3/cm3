// Nothing in this file is used. Nothing in this file is tested.

#ifndef INCLUDED_M3CORE_H
#include "m3core.h"
#endif

#ifdef __cplusplus
extern "C" {
#endif

#undef M3MODULE /* Support concatenating multiple .c files. */
#define M3MODULE Cstdio

M3WRAP1(int, fclose, FILE*)
M3WRAP2_(FILE*, fdopen, int, const char*) /* fileno to FILE* */
M3WRAP1(int, feof, FILE*)
M3WRAP1(int, ferror, FILE*)
M3WRAP1(int, fflush, FILE*)
M3WRAP1(int, fgetc, FILE*)
M3WRAP3(char*, fgets, char*, int, FILE*)
M3WRAP1_(int, fileno, FILE*) /* FILE* to fileno */
M3WRAP2(FILE*, fopen, const char*, const char*)
M3WRAP2(int, fputc, int, FILE*)
M3WRAP2(int, fputs, const char*, FILE*)
M3WRAP4(size_t, fread, void*, size_t, size_t, FILE*)
M3WRAP3(FILE*, freopen, const char*, const char*, FILE*)
M3WRAP3(int, fseek, FILE*, long, int)
M3WRAP1(long, ftell, FILE*)
M3WRAP4(size_t, fwrite, const void*, size_t, size_t, FILE*)
M3WRAP1(int, getc, FILE*)
M3WRAP0(int, getchar)

int __cdecl Cstdio__pclose (FILE* a) /* close pipe */
{
    int r;
    Scheduler__DisableSwitching ();
#ifdef _WIN32
    r = _pclose (a);
#else
    r = pclose (a);
#endif
    Scheduler__EnableSwitching ();
    return r;
}

FILE* __cdecl Cstdio__popen (const char* a, const char* b) /* open pipe */
{
    FILE* r;
    Scheduler__DisableSwitching ();
#ifdef _WIN32
    r = _popen (a, b);
#else
    r = popen (a, b);
#endif
    Scheduler__EnableSwitching ();
    return r;
}

M3WRAP2(int, putc, int, FILE*)
M3WRAP1(int, putchar, int)
M3WRAP1(int, puts, const char*) /* put string + "\n" to stdout, atomically */
M3WRAP1(int, remove, const char*)
M3WRAP2(int, rename, const char*, const char*)
M3WRAP4(int, setvbuf, FILE*, char*, int, size_t)
M3WRAP0(FILE*, tmpfile)
M3WRAP2(int, ungetc, int, FILE*)
#ifndef __HAIKU__
M3WRAP1_(int, getw, FILE*)
M3WRAP2_(int, putw, int, FILE*)
#endif

M3WRAP1_RETURN_VOID(clearerr, FILE*)
M3WRAP1_RETURN_VOID(perror, const char*) /* print error */
M3WRAP1_RETURN_VOID(rewind, FILE*)
M3WRAP2_RETURN_VOID(setbuf, FILE*, char*)

#undef X
#undef X_
#define X(a) EXTERN_CONST int Cstdio__##a = a;
#define X_(a) EXTERN_CONST int Cstdio__##a = _##a;

X(BUFSIZ)
X(FILENAME_MAX)
X(FOPEN_MAX)
X_(IOFBF) /* setvbuf full buffered */
X_(IOLBF) /* setvbuf line buffered */
X_(IONBF) /* setvbuf not buffered */
X(L_tmpnam)
X(SEEK_CUR)
X(SEEK_END)
X(SEEK_SET)
X(TMP_MAX)
X(EOF)

#undef X
#define X(a) FILE* __cdecl Cstdio__get_##a(void) { return a; }

X(stdin)
X(stdout)
X(stderr)

#ifdef __cplusplus
}
#endif
