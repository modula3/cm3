(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

MODULE IPErrorPosix EXPORTS IPError;

IMPORT Atom, Cerrno, IP;

PROCEDURE RaiseOS (a: Atom.T) RAISES {IP.Error} =
  BEGIN
    Raise (a, Cerrno.GetErrno());
  END RaiseOS;

BEGIN
END IPErrorPosix.
