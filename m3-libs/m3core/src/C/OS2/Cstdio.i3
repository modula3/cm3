(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Thu May  6 09:18:19 PDT 1993 by muller        *)

INTERFACE Cstdio;

FROM Ctypes IMPORT int, char_star, unsigned_int;

CONST 
  NIOBRW = 3;
  IOEOF  = 8_20;

TYPE
  FILE = RECORD 
            flags: int;
            gptr:   char_star;
            egptr:  char_star;
            eback:  char_star;
            pbase:  char_star;
            pptr:   char_star;
            epptr:  char_star;
            base:   char_star;
            ebuf:   char_star;
            chain:  ADDRESS; END;

  FILE_star = UNTRACED REF FILE;

VAR iob: ARRAY [0..NIOBRW-1] OF FILE_star;

<*EXTERNAL "__std_filebuf_0"*> VAR stdin_file: FILE;
<*EXTERNAL "__std_filebuf_1"*> VAR stdout_file: FILE;
<*EXTERNAL "__std_filebuf_2"*> VAR stderr_file: FILE;


<*EXTERNAL "_flsbuf"*> PROCEDURE flsbuf (c: unsigned_int; f:
FILE_star): int; <*EXTERNAL "_filbuf"*> PROCEDURE filbuf (f:
FILE_star): int;

<*EXTERNAL feof*>      PROCEDURE feof (f: FILE_star): int;
<*EXTERNAL getc*>      PROCEDURE getc (f: FILE_star): int;
<*EXTERNAL ungetc*>    PROCEDURE ungetc (c: int; f: FILE_star): int;
<*EXTERNAL putc*>      PROCEDURE putc (c: int; f: FILE_star): int;
<*EXTERNAL fflush*>    PROCEDURE fflush (f: FILE_star): int;

END Cstdio.
