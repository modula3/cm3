(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Mon Feb  1 20:36:42 PST 1993 by mjordan    *)
(*      modified on Fri Oct  9 17:44:27 PDT 1992 by muller     *)
(*      modified on Sun Jul 12, 1992 by thomas@mw.lpc.ethz.ch  *)

INTERFACE Csetjmp;

FROM Ctypes IMPORT int;

TYPE
  jmp_buf = RECORD
(* This information appears both here and in m3middle/Target.m3.
   Ideally it would only occur in one place. *)
    opaque : ARRAY [0..15] OF INTEGER;
  END;

<*EXTERNAL "longjmp" *> PROCEDURE ulongjmp (VAR env: jmp_buf; val: int);

END Csetjmp.
