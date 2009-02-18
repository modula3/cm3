(* Copyright (C) 1992, Digital Equipment Corporation          *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* derived from LINUXLIBC6 *)

UNSAFE MODULE RTSignal EXPORTS RTSignal, RTSignalPrivate;

IMPORT RTSignalC, RTError;

PROCEDURE InstallHandlers () =
  BEGIN
    RTSignalC.InstallHandlers();
  END InstallHandlers;

PROCEDURE RestoreHandlers () =
  BEGIN
    RTSignalC.RestoreHandlers();
  END RestoreHandlers;

PROCEDURE MsgPCSegV (pc: INTEGER) =
  BEGIN
    RTError.MsgPC (pc,
      "Segmentation violation - possible attempt to dereference NIL");
  END MsgPCSegV;

PROCEDURE MsgPCAbort (pc: INTEGER) =
  BEGIN
    RTError.MsgPC (pc, "aborted");
  END MsgPCAbort;

BEGIN
END RTSignal.
