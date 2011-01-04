(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE Csetjmp;

(* See Common/Csetjmp.i3 for comments. *)
FROM Ctypes IMPORT int;
TYPE jmp_buf = ARRAY [0..16_FFFFFF] OF LONGREAL;
<*EXTERNAL "longjmp" *> PROCEDURE ulongjmp (VAR env: jmp_buf; val: int);

END Csetjmp.
