(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Thu Apr 28 13:08:02 PDT 1994 by kalsow     *)
(*      modified on Tue Feb  5 00:29:50 1991 by muller         *)

INTERFACE Csetjmp;		(* for SPARC *)

FROM Ctypes IMPORT int;

TYPE jmp_buf = ARRAY [0..9] OF int;
(* large enough for a sigjmp_buf *)

<*EXTERNAL*> PROCEDURE setjmp (VAR env: jmp_buf): int;
<*EXTERNAL*> PROCEDURE longjmp (VAR env: jmp_buf; val: int);

<*EXTERNAL "_setjmp" *>  PROCEDURE usetjmp (VAR env: jmp_buf): int;
<*EXTERNAL "_longjmp" *> PROCEDURE ulongjmp (VAR env: jmp_buf; val: int);

END Csetjmp.
