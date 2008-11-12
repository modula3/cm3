(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Mon Feb  1 20:36:42 PST 1993 by mjordan    *)
(*      modified on Fri Oct  9 17:44:27 PDT 1992 by muller     *)
(*      modified on Sun Jul 12, 1992 by thomas@mw.lpc.ethz.ch  *)

INTERFACE Csetjmp;		(* for NT386/WIN32 *)

FROM Ctypes IMPORT int;

TYPE jmp_buf = ARRAY [0..7] OF int;

<*EXTERNAL "setjmp" *> PROCEDURE usetjmp (VAR env: jmp_buf): int;
<*EXTERNAL "longjmp" *> PROCEDURE ulongjmp (VAR env: jmp_buf; val: int);

END Csetjmp.
