(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Thu Sep 23 09:12:39 PDT 1993 by kalsow     *)
(*      modified on Wed Mar 13 00:20:16 1991 by muller         *)

INTERFACE Csetjmp;		(* for ARM *)

FROM Ctypes IMPORT int;

TYPE jmp_buf = ARRAY [0..9] OF int;

<*EXTERNAL*> PROCEDURE setjmp (VAR env: jmp_buf): int;
<*EXTERNAL*> PROCEDURE longjmp (VAR env: jmp_buf; val: int);

<*EXTERNAL "_setjmp" *>  PROCEDURE usetjmp (VAR env: jmp_buf): int;
<*EXTERNAL "_longjmp" *> PROCEDURE ulongjmp (VAR env: jmp_buf; val: int);

END Csetjmp.
