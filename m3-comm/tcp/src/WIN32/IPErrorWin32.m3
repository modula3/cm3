(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

MODULE IPErrorWin32 EXPORTS IPError;

IMPORT Atom, IP, WinSock;

PROCEDURE RaiseOS (a: Atom.T) RAISES {IP.Error} =
  BEGIN
    Raise (a, WinSock.WSAGetLastError ());
  END RaiseOS;

BEGIN
END IPErrorWin32.
