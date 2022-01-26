(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

UNSAFE MODULE SocketNone EXPORTS Socket;

IMPORT Atom, AtomList;
IMPORT OSError;

REVEAL
  T = Public BRANDED "Socket.T" OBJECT
  END;

PROCEDURE Create (reliable: BOOLEAN): T
  RAISES {OSError.E} =
  BEGIN
    IOError (Unexpected);
    RETURN NIL;
  END Create;

(*------------------------------------------------ internal utilities ---*)

PROCEDURE IOError (a: Atom.T) RAISES {OSError.E} =
  BEGIN
    RAISE OSError.E (AtomList.Cons (a, NIL));
  END IOError;

BEGIN
END SocketNone.
