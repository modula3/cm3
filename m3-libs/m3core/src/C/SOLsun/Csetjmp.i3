(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Fri Apr 29 09:55:47 PDT 1994 by kalsow     *)
(*      modified on Fri Sep 10 14:34:19 PDT 1993 by muller     *)

INTERFACE Csetjmp;		(* for SOLsun *)

FROM Ctypes IMPORT int;

TYPE jmp_buf = ARRAY [0..18] OF int;
(* large enough for a sigjmp_buf.  Apparently the BSD
   compatibility library implements setjmp with sigsetjmp... *)

<*EXTERNAL*> PROCEDURE setjmp (VAR env: jmp_buf): int;
<*EXTERNAL*> PROCEDURE longjmp (VAR env: jmp_buf; val: int);

<*EXTERNAL "setjmp" *>  PROCEDURE usetjmp (VAR env: jmp_buf): int;
<*EXTERNAL "longjmp" *> PROCEDURE ulongjmp (VAR env: jmp_buf; val: int);

END Csetjmp.
