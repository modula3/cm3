(* Copyright (C) 1990, 1992, Digital Equipment Corporation.                  *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Last modified on Fri Apr  9 16:27:42 PDT 1993 by muller                   *)

INTERFACE Cstdio;

FROM Ctypes IMPORT int, short, char;

TYPE
  char_star = UNTRACED REF char;  (* because of the weird def in Ctypes *)

CONST 
  N_STATIC_IOBS = 3;
  IOEOF  = 8_20;

TYPE
  Base = [2..16];
  iobuf = RECORD 
            cnt: int;
            ptr: char_star;
            base: char_star;
            bufsiz: int;
            flag: short;
            file: short; END;

<*EXTERNAL "_iob"*> VAR iob: ARRAY [0..N_STATIC_IOBS - 1] OF iobuf;
<*EXTERNAL "_flsbuf"*> PROCEDURE flsbuf (c: CHAR; f: UNTRACED REF iobuf);
<*EXTERNAL "_filbuf"*> PROCEDURE filbuf (f: UNTRACED REF iobuf): CHAR;
<*EXTERNAL "ungetc"*>  PROCEDURE ungetc (c: CHAR; f: UNTRACED REF iobuf);
<*EXTERNAL "fflush"*> PROCEDURE flush (f: UNTRACED REF iobuf);

END Cstdio.
