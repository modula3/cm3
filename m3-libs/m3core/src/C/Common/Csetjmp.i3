(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

UNSAFE INTERFACE Csetjmp;
FROM Ctypes IMPORT int;

(* temporary for compat, not used; u is for underscore;
 * _longjmp does not save/restore signal mask, saving much time
 *)
<*EXTERNAL "Csetjmp__ulongjmp" *> PROCEDURE ulongjmp (env: ADDRESS; val: int);

(* Modula-3 exception uses setjmp/longjmp, without signal mask save/restore
 * TODO: Use C++ exceptions or Win32 exceptions or libunwind.
 *)
<*EXTERNAL "Csetjmp__m3_longjmp" *> PROCEDURE m3_longjmp (env: ADDRESS; val: int);

END Csetjmp.
