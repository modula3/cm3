(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Wed Mar 13 00:20:16 1991 by muller         *)

INTERFACE Csetjmp;		(* for OKI *)

FROM Ctypes IMPORT int;

TYPE jmp_buf = ARRAY [0..21] OF int;

<*EXTERNAL*> PROCEDURE setjmp (VAR env: jmp_buf): int;
<*EXTERNAL*> PROCEDURE longjmp (VAR env: jmp_buf; val: int);

<*EXTERNAL "setjmp" *>  PROCEDURE usetjmp (VAR env: jmp_buf): int;
<*EXTERNAL "longjmp" *> PROCEDURE ulongjmp (VAR env: jmp_buf; val: int);

END Csetjmp.
