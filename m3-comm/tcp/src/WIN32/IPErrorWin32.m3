(* Copyright 1996, Critical Mass, Inc.  All rights reserved. *)

MODULE IPErrorWin32 EXPORTS IPError;

IMPORT Atom, IP, WinSock;

PROCEDURE RaiseOS (a: Atom.T) RAISES {IP.Error} =
  BEGIN
    Raise (a, WinSock.WSAGetLastError ());
  END RaiseOS;

BEGIN
END IPErrorWin32.
