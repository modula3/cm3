(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

UNSAFE INTERFACE Csetjmp;

(* See Common/Csetjmp.i3 for comments. *)
FROM Ctypes IMPORT int;
<*EXTERNAL "Csetjmp__Jumpbuf_size" *> VAR Jumpbuf_size: INTEGER;
<*EXTERNAL "longjmp" *> PROCEDURE ulongjmp (env: ADDRESS; val: int);

END Csetjmp.
