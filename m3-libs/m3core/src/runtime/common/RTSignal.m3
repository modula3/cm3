(* Copyright (C) 1992, Digital Equipment Corporation          *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* derived from LINUXLIBC6 *)

UNSAFE MODULE RTSignal;

(* const *)
VAR Texts_t: Texts;

PROCEDURE InstallHandlers () =
  BEGIN
    RTSignalC.InstallHandlers(Texts);
  END InstallHandlers;

PROCEDURE RestoreHandlers () =
  BEGIN
    RTSignalC.RestoreHandlers();
  END RestoreHandlers;

BEGIN
END RTSignal.
