(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Tue Mar 17 12:14:23 PST 1992 by muller         *)

INTERFACE Csetjmp;		(* for NEXT *)

FROM Ctypes IMPORT int;

TYPE jmp_buf = ARRAY [0..39] OF int;

<*EXTERNAL*> PROCEDURE setjmp (VAR env: jmp_buf): int;
<*EXTERNAL*> PROCEDURE longjmp (VAR env: jmp_buf; val: int);

<*EXTERNAL "_setjmp" *>  PROCEDURE usetjmp (VAR env: jmp_buf): int;
<*EXTERNAL "_longjmp" *> PROCEDURE ulongjmp (VAR env: jmp_buf; val: int);

END Csetjmp.
