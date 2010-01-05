(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE Csetjmp;

FROM Ctypes IMPORT int;

TYPE jmp_buf = ARRAY [0..10] OF int; (* actually, this is a sigjmp_buf,
                                        just in case *)

<*EXTERNAL "_longjmp" *> PROCEDURE ulongjmp (VAR env: jmp_buf; val: int);

END Csetjmp.
