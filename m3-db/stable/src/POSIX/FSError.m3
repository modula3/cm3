(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Wed Oct 11 15:12:48 PDT 1995 by najork                   *)
(*       Created on Wed Oct 11 15:02:48 PDT 1995 by najork                   *)

MODULE FSError;

IMPORT OSError, OSErrorPosix, Uerror;

PROCEDURE FileNotFound (err: OSError.Code): BOOLEAN =
  BEGIN
    RETURN err.head = OSErrorPosix.ErrnoAtom (Uerror.ENOENT)
  END FileNotFound;

BEGIN
END FSError.
