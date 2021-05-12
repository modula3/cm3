INTERFACE Cstdio;

(* Nothing in this file is used. Nothing in this file is tested. *)

FROM Ctypes IMPORT int, const_char_star, void_star;
FROM Cstddef IMPORT size_t;

TYPE
    FILE_star = UNTRACED REF RECORD END;

<*EXTERNAL Cstdio__feof*> PROCEDURE feof (f: FILE_star): int;
<*EXTERNAL Cstdio__getc*> PROCEDURE getc (f: FILE_star): int;
<*EXTERNAL Cstdio__ungetc*> PROCEDURE ungetc (c: int; f: FILE_star): int;
<*EXTERNAL Cstdio__putc*> PROCEDURE putc (c: int; f: FILE_star): int;
<*EXTERNAL Cstdio__fflush*> PROCEDURE fflush (f: FILE_star): int;
<*EXTERNAL Cstdio__fdopen*> PROCEDURE fdopen (fd: int; mode: const_char_star): FILE_star;
<*EXTERNAL Cstdio__fopen*> PROCEDURE fopen (path: const_char_star; mode: const_char_star): FILE_star;
<*EXTERNAL Cstdio__fclose*> PROCEDURE fclose (f: FILE_star): int;
<*EXTERNAL Cstdio__fread*> PROCEDURE fread (buffer: void_star;
                                            size: size_t;
                                            nitems: size_t;
                                            stream: FILE_star): size_t;

<*EXTERNAL Cstdio__get_stdin*> PROCEDURE get_stdin (): FILE_star;
<*EXTERNAL Cstdio__get_stdout*> PROCEDURE get_stdout (): FILE_star;
<*EXTERNAL Cstdio__get_stderr*> PROCEDURE get_stderr (): FILE_star;

END Cstdio.
