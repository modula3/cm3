(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

UNSAFE INTERFACE Csetjmp;
FROM Ctypes IMPORT int;

TYPE jmp_buf = ADDRESS; (* Name for m3core.h to replace. *)

(* Modula-3 exception uses setjmp/longjmp, without signal mask save/restore
 * TODO: Use C++ exceptions or Win32 exceptions or libunwind.
 *)
<*EXTERNAL "Csetjmp__m3_longjmp" *> PROCEDURE m3_longjmp (env: jmp_buf; val: int);

END Csetjmp.
