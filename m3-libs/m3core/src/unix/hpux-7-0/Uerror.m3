(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Thu Dec  3 07:54:38 PST 1992 by mcjones    *)
(*      modified on Mon Nov 19 23:48:46 1990 by muller         *)
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

