(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Thu May 13 17:45:05 PDT 1993 by swart          *)
(*      modified on Tue Jun 25 10:43:54 1991 by muller         *)

INTERFACE NullRd;

IMPORT Rd;

TYPE
  T <: Public;
  Public = Rd.T OBJECT METHODS init (): T END;
  (* An initialized NullRd.T is an empty input stream:
        len (rd) = 0
        src (rd) = 'eof'
        seekable (rd) = TRUE
        intermittent (rd) = FALSE  *)

END NullRd.
