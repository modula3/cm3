(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE Csetjmp;

FROM Ctypes IMPORT int;

TYPE jmp_buf = ARRAY [0..18] OF int;

<*EXTERNAL "siglongjmp" *>
PROCEDURE ulongjmp (VAR env: jmp_buf; val: int);

END Csetjmp.
