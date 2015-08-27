(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

UNSAFE INTERFACE Csetjmp;
FROM Ctypes IMPORT int;

(* TODO? Move this to C?

   "u" in "ulongjmp" is probably for "underscore".
   This variant of longjmp never restores the signal mask.
   Because we believe we never change it?
   And restoring it is less efficient? (Requires possible kernel
   interaction?)

   If the platform only has "regular" longjmp and no signal mask,
   e.g. Win32, then this is resolved to that.

   This function does not return in the usual sense.
   This is used to raise an exception.
   This is subject to be removed, either by using C, or "libunwind", or
   Win32 exceptions, or C++ exceptions.

*)
<*EXTERNAL "Csetjmp__ulongjmp" *> PROCEDURE ulongjmp (env: ADDRESS; val: int);

END Csetjmp.
