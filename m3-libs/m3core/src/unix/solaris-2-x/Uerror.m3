(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Sat Jun 27 15:48:33 PDT 1992 by muller         *)
(*      modified on Mon Nov 19 23:46:25 1990 by mjordan        *)

UNSAFE MODULE Uerror;

IMPORT Ctypes;

PROCEDURE GetFrom_sys_errlist(n: INTEGER): Ctypes.char_star RAISES {}=
  BEGIN
    <* ASSERT 0 <= n AND n <= Max *>
    RETURN sys_errlist[n]
  END GetFrom_sys_errlist;

BEGIN
END Uerror.

