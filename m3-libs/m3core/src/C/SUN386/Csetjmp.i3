(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Fri Oct  9 17:44:27 PDT 1992 by muller                 *)
(*      modified on Sun Jul 12, 1992 by thomas@mw.lpc.ethz.ch  *)

INTERFACE Csetjmp;		(* for SUN386 *)

FROM Ctypes IMPORT int;

TYPE jmp_buf = ARRAY [0..7] OF int;

<*EXTERNAL*> PROCEDURE setjmp (VAR env: jmp_buf): int;
<*EXTERNAL*> PROCEDURE longjmp (VAR env: jmp_buf; val: int);

<*EXTERNAL "_setjmp" *>  PROCEDURE usetjmp (VAR env: jmp_buf): int;
<*EXTERNAL "_longjmp" *> PROCEDURE ulongjmp (VAR env: jmp_buf; val: int);

END Csetjmp.
