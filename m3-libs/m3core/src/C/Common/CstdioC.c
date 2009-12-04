#define _CRT_SECURE_NO_DEPRECATE

#include <stdio.h>

#if !defined(_MSC_VER) && !defined(__cdecl)
#define __cdecl /* nothing */
#endif

#ifdef __cplusplus
extern "C" {
#endif

#ifdef _MSC_VER
#pragma optimize("gty", on)
#endif

/* return types need to be single tokens -- else we need separate macros for void functions
   parameter types do not have this restriction */
#define return_void
#define return_int return
#define return_long return
#define return_PCSTR return
#define return_PFILE  return
#define return_PSTR return
#define return_size_t return
typedef const char* PCSTR;
typedef char* PSTR;
typedef FILE* PFILE;

#define X(ret, name, in, out)     ret __cdecl Cstdio__##name in { return_##ret name out; }
#ifdef _WIN32
#define X_(ret, name, in, out)     ret __cdecl Cstdio__##name in { return_##ret _##name out; }
#else
#define X_(ret, name, in, out)    X(ret, name, in, out)
#endif
#define X0(ret, name)             X(ret, name, (void),               ())
#define X1(ret, name, a)          X(ret, name, (a i),                (i))
#define X2(ret, name, a, b)       X(ret, name, (a i, b j),           (i, j))
#define X3(ret, name, a, b, c)    X(ret, name, (a i, b j, c k),      (i, j, k))
#define X4(ret, name, a, b, c, d) X(ret, name, (a i, b j, c k, d m), (i, j, k, m))

#define X1_(ret, name, a)        X_(ret, name, (a i),                (i))
#define X2_(ret, name, a, b)     X_(ret, name, (a i, b j),           (i, j))

X1(void, clearerr, PFILE)
X1(int, fclose, PFILE)
X2_(PFILE, fdopen, int, PCSTR) /* fileno to FILE* */
X1(int, feof, PFILE)
X1(int, ferror, PFILE)
X1(int, fflush, PFILE)
X1(int, fgetc, PFILE)
X3(PSTR, fgets, PSTR, int, PFILE)
X1_(int, fileno, PFILE) /* FILE* to fileno */
X2(PFILE, fopen, PCSTR, PCSTR)
X2(int, fputc, int, PFILE)
X2(int, fputs, PCSTR, PFILE)
X4(size_t, fread, void*, size_t, size_t, PFILE)
X3(PFILE, freopen, PCSTR, PCSTR, PFILE)
X3(int, fseek, PFILE, long, int)
X1(long, ftell, PFILE)
X4(size_t, fwrite, const void*, size_t, size_t, PFILE)
X1(int, getc, PFILE)
X0(int, getchar)
X1_(int, pclose, PFILE) /* close pipe */
X1(void, perror, PCSTR) /* print error */
X2_(PFILE, popen, PCSTR, PCSTR) /* open pipe */
X2(int, putc, int, PFILE)
X1(int, putchar, int)
X1(int, puts, PCSTR) /* put string + "\n" to stdout */
X1(int, remove, PCSTR)
X2(int, rename, PCSTR, PCSTR)
X1(void, rewind, PFILE)
X2(void, setbuf, PFILE, PSTR)
X4(int, setvbuf, PFILE, PSTR, int, size_t)
X2_(PSTR, tempnam, PCSTR, PCSTR)
X0(PFILE, tmpfile)
X1(PSTR, tmpnam, PSTR)
X2(int, ungetc, int, PFILE)
X1_(int, getw, PFILE)
X2_(int, putw, int, PFILE)

#undef X
#undef X_
#define X(a) const unsigned Cstdio__##a = a;
#define X_(a) const unsigned Cstdio__##a = _##a;

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

#undef X
#define X(a) const int Cstdio__##a = a;

X(EOF)

#undef X
#define X(a) const char Cstdio__##a[] = a;

X(P_tmpdir)

#undef X
#define X(a) PFILE __cdecl Cstdio__get_##a(void) { return a; }

X(stdin)
X(stdout)
X(stderr)

#ifdef __cplusplus
}
#endif
