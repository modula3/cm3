(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Fri Apr  9 18:51:54 PDT 1993 by muller        *)

INTERFACE Cstdio;

FROM Ctypes IMPORT int, short, void_star, unsigned_char_star, unsigned_int;

CONST 
  NIOBRW = 8;
  IOEOF  = 8_20;

TYPE
  FILE = RECORD 
            cnt: int;
            ptr: unsigned_char_star;
            base: unsigned_char_star;
            bufsiz: int;
            flag: short;
            file: short;
            unused: ARRAY [0..1] OF int;
            lock: void_star;
            bufendp: unsigned_char_star; END;
  FILE_star = UNTRACED REF FILE;

<*EXTERNAL "_iob"*> VAR iob: ARRAY [0..NIOBRW-1] OF FILE;
<*EXTERNAL "_flsbuf"*> PROCEDURE flsbuf (c: unsigned_int; f: FILE_star): int;
<*EXTERNAL "_filbuf"*> PROCEDURE filbuf (f: FILE_star): int;

<*EXTERNAL feof*>      PROCEDURE feof (f: FILE_star): int;
<*EXTERNAL getc*>      PROCEDURE getc (f: FILE_star): int;
<*EXTERNAL ungetc*>    PROCEDURE ungetc (c: int; f: FILE_star): int;
<*EXTERNAL putc*>      PROCEDURE putc (c: int; f: FILE_star): int;
<*EXTERNAL fflush*>    PROCEDURE fflush (f: FILE_star): int;

END Cstdio.
