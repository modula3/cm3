(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

INTERFACE IPError;

IMPORT Atom, AtomList, IP;

PROCEDURE Raise (a: Atom.T;  info: INTEGER := 0) RAISES {IP.Error};
(* == RAISE IP.Error ([a, info]) *)

PROCEDURE RaiseOS (a: Atom.T) RAISES {IP.Error};
(* == Raise (a, OS.GetLastError()) *)

PROCEDURE RaiseUnexpected () RAISES {IP.Error};
(* == RaiseOS (TCP.Unexpected) *)

PROCEDURE Die();
(* Crash with an unhandled exception, "IPError.FatalError" *)

VAR (*CONST*)
  Unexpected: Atom.T;
  ClosedErr: AtomList.T;

END IPError.
