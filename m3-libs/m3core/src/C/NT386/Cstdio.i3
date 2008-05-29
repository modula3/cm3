(* $Id: Cstdio.i3,v 1.5 2008-05-29 12:43:18 jkrell Exp $ *)

INTERFACE Cstdio;

FROM Ctypes IMPORT const_char_star, char_star, void_star, const_void_star, char, int, long;
FROM Cstddef IMPORT size_t;

TYPE
    FILE = RECORD END;
    FILE_star = UNTRACED REF FILE;
    fpos_t = ARRAY [0..1] OF int; (* treat this as opaque; maybe not portable *)

CONST
    (* These might not be portable. *)
    SEEK_SET = 0;
    SEEK_CUR = 1;
    SEEK_END = 2;

<*EXTERNAL "Cstdio__stderr"*> PROCEDURE stderr(): FILE_star;
<*EXTERNAL "Cstdio__stdin"*> PROCEDURE stdin(): FILE_star;
<*EXTERNAL "Cstdio__stdout"*> PROCEDURE stdout(): FILE_star;
<*EXTERNAL*> PROCEDURE fclose (a: FILE_star): int;
<*EXTERNAL*> PROCEDURE feof(a: FILE_star): int;
<*EXTERNAL*> PROCEDURE ferror(a: FILE_star): int;
<*EXTERNAL*> PROCEDURE fflush(a: FILE_star): int;
<*EXTERNAL*> PROCEDURE fgetc (a: FILE_star): int;
<*EXTERNAL*> PROCEDURE fgetpos (a: FILE_star; VAR b: fpos_t): int;
<*EXTERNAL*> PROCEDURE fgets (a: char_star; b: int; c: FILE_star): char_star;
<*EXTERNAL*> PROCEDURE fopen (a: const_char_star; b: const_char_star): FILE_star;
<*EXTERNAL*> PROCEDURE fputc (a: int; b: FILE_star): int;
<*EXTERNAL*> PROCEDURE fputs (a: const_char_star; b: FILE_star): int;
<*EXTERNAL*> PROCEDURE fread (a: void_star; b: size_t; c: size_t; d: FILE_star): size_t;
<*EXTERNAL*> PROCEDURE freopen (a: const_char_star; b: const_char_star; c: FILE_star): FILE_star;
<*EXTERNAL*> PROCEDURE fseek (a: FILE_star; b: long; c: [0..2]): int; (* 0..2 maybe not portable *)
<*EXTERNAL*> PROCEDURE fsetpos (a: FILE_star; b: fpos_t): int;
<*EXTERNAL*> PROCEDURE ftell (a: FILE_star): long;
<*EXTERNAL*> PROCEDURE fwrite (a: const_void_star; b: size_t; c: size_t; d: FILE_star): size_t;
<*EXTERNAL*> PROCEDURE getc (a: const_char_star; b: FILE_star): int;
<*EXTERNAL*> PROCEDURE getchar (): int;
<*EXTERNAL*> PROCEDURE pclose(a: FILE_star): int;
<*EXTERNAL*> PROCEDURE perror (a: const_char_star);
<*EXTERNAL*> PROCEDURE popen(a: const_char_star): FILE_star;
<*EXTERNAL*> PROCEDURE putc (a: char; b: FILE_star);
<*EXTERNAL*> PROCEDURE putchar (a: char);
<*EXTERNAL*> PROCEDURE puts (a: const_char_star);
<*EXTERNAL*> PROCEDURE remove (a: const_char_star): int;
<*EXTERNAL*> PROCEDURE rename (a: const_char_star; b: const_char_star): int;
<*EXTERNAL*> PROCEDURE setbuf (a: FILE_star; b: char_star; c: int; d: size_t); (* need values for the int *)
<*EXTERNAL*> PROCEDURE setvbuf (a: FILE_star; b: char_star);
<*EXTERNAL*> PROCEDURE tmpfile (): FILE_star;
<*EXTERNAL*> PROCEDURE tmpnam (): FILE_star;
<*EXTERNAL*> PROCEDURE ungetc (a: char; b: FILE_star);
<*EXTERNAL*> PROCEDURE unlink(a: const_char_star): int;

(*
On Windows, these functions without leading underscores are deprecated,
and they exist perfectly well with leading undersores.
fileno points out that open/read/write/close on Windows ought to be exposed, if it not already.
fileno and fdopen are bridges between stdio and "unixio".

<*EXTERNAL*> PROCEDURE fcloseall(void): int;
<*EXTERNAL*> PROCEDURE fdopen(a: int;  b: const_char_star): FILE_star;
<*EXTERNAL*> PROCEDURE fgetchar(): int;
<*EXTERNAL*> PROCEDURE fileno(a: FILE_star): int;
<*EXTERNAL*> PROCEDURE flushall(): int
<*EXTERNAL*> PROCEDURE fputchar(a: int): int;
<*EXTERNAL*> PROCEDURE getw(a: FILE_star): int;
<*EXTERNAL*> PROCEDURE putw(a: int; b: FILE_star): int;
<*EXTERNAL*> PROCEDURE rmtmp(): int;
*)

END Cstdio.
