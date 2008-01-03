(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Wed Aug 10 17:12:28 PDT 1994 by kalsow     *)
(*      modified on Tue Feb  5 00:29:48 1991 by muller         *)

INTERFACE Csetjmp;		(* for SEQUENT *)

FROM Ctypes IMPORT int;

TYPE jmp_buf = ARRAY [0..83] OF int;

<*EXTERNAL*> PROCEDURE setjmp (VAR env: jmp_buf): int;
<*EXTERNAL*> PROCEDURE longjmp (VAR env: jmp_buf; val: int);

<*EXTERNAL "_setjmp" *>  PROCEDURE usetjmp (VAR env: jmp_buf): int;
<*EXTERNAL "_longjmp" *> PROCEDURE ulongjmp (VAR env: jmp_buf; val: int);

END Csetjmp.
