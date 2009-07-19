INTERFACE Cstdio;

FROM Ctypes IMPORT int, const_char_star;

TYPE
    FILE = RECORD END;
    FILE_star = UNTRACED REF FILE;

<*EXTERNAL Cstdio__feof*> PROCEDURE feof (f: FILE_star): int;
<*EXTERNAL Cstdio__getc*> PROCEDURE getc (f: FILE_star): int;
<*EXTERNAL Cstdio__ungetc*> PROCEDURE ungetc (c: int; f: FILE_star): int;
<*EXTERNAL Cstdio__putc*> PROCEDURE putc (c: int; f: FILE_star): int;
<*EXTERNAL Cstdio__fflush*> PROCEDURE fflush (f: FILE_star): int;
<*EXTERNAL Cstdio__fdopen*> PROCEDURE fdopen (fd: int; mode: const_char_star): FILE_star;
<*EXTERNAL Cstdio__fopen*> PROCEDURE fopen (path: const_char_star; mode: const_char_star): FILE_star;
<*EXTERNAL Cstdio__fclose*> PROCEDURE fclose (f: FILE_star): int;

<*EXTERNAL Cstdio__get_stdin*> PROCEDURE get_stdin (): FILE_star;
<*EXTERNAL Cstdio__get_stdout*> PROCEDURE get_stdout (): FILE_star;
<*EXTERNAL Cstdio__get_stderr*> PROCEDURE get_stderr (): FILE_star;

END Cstdio.
