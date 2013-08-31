(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

UNSAFE INTERFACE Csetjmp;
FROM Ctypes IMPORT int;

(* jmp_buf is allocated with alloca(Csetjmp__Jumpbuf_size)
   "u" in "ulongjmp" is probably for "underscore".
   This variant of longjmp never restores the signal mask.
*)
<*EXTERNAL "Csetjmp__Jumpbuf_size" *> VAR Jumpbuf_size: INTEGER;
<*EXTERNAL "Csetjmp__ulongjmp" *> PROCEDURE ulongjmp (env: ADDRESS; val: int);

END Csetjmp.
