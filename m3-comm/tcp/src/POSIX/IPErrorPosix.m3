(* Copyright 1996, Critical Mass, Inc.  All rights reserved. *)

MODULE IPErrorPosix EXPORTS IPError;

IMPORT Atom, Cerrno, IP;

PROCEDURE RaiseOS (a: Atom.T) RAISES {IP.Error} =
  BEGIN
    Raise (a, Cerrno.errno);
  END RaiseOS;

BEGIN
END IPErrorPosix.
