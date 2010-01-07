(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE Csetjmp;

FROM Ctypes IMPORT int;

(* ideal alignment is 16 bytes but 4 is ok; 8 here *)
TYPE jmp_buf = ARRAY [0..73] OF LONGINT;

<*EXTERNAL "_longjmp" *> PROCEDURE ulongjmp (VAR env: jmp_buf; val: int);

END Csetjmp.
