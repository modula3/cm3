(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE Csetjmp;

FROM Ctypes IMPORT int;

CONST
  JBLEN = 28;

TYPE jmp_buf = ARRAY [0..JBLEN-1] OF int;

<*EXTERNAL longjmp *> PROCEDURE ulongjmp (VAR env: jmp_buf; val: int);

END Csetjmp.
