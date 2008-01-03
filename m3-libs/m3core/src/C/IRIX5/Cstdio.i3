(* Copyright (C) 1990, 1992, Digital Equipment Corporation.                  *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Last modified on Tue Oct  4 12:55:24 PDT 1994 by ericv                    *)
(*      modified on Fri Apr  9 16:27:42 PDT 1993 by muller                   *)

INTERFACE Cstdio;

FROM Ctypes IMPORT int, unsigned_char_star, unsigned_char;

CONST 
  NFILE = 100;
  IOEOF  = 8_20;

TYPE
  Base = [2..16];
  FILE = RECORD 
            cnt: int;
            ptr: unsigned_char_star;
            base: unsigned_char_star;
            flag: unsigned_char;
            file: unsigned_char; END;
  FILE_star = UNTRACED REF FILE;

<*EXTERNAL "_iob"*> VAR iob: ARRAY [0..NFILE - 1] OF FILE;
<*EXTERNAL "_flsbuf"*> PROCEDURE flsbuf (c: int; f: FILE_star): int;
<*EXTERNAL "_filbuf"*> PROCEDURE filbuf (f: FILE_star): int;

<*EXTERNAL "ungetc"*>  PROCEDURE ungetc (c: int; f: FILE_star): int;
<*EXTERNAL "fflush"*> PROCEDURE fflush (f: FILE_star): int;

END Cstdio.
