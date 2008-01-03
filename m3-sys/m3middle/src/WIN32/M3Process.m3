(* Copyright (C) 1996-2000, Critical Mass, Inc.  All rights reserved.  *)
(* See file COPYRIGHT-CMASS for details. *)

UNSAFE MODULE M3Process;

IMPORT OSError, OSErrorWin32, Process, WinBase, WinNT;

PROCEDURE Interrupt (t: Process.T)  RAISES {OSError.E} =
  BEGIN
    Abort (t); (* can't do any better yet... *)
  END Interrupt;

PROCEDURE Abort (t: Process.T)  RAISES {OSError.E} =
  VAR hProcess := LOOPHOLE (Process.GetID (t), WinNT.HANDLE);
  BEGIN
    IF WinBase.TerminateProcess (hProcess, 1) = 0 THEN
      OSErrorWin32.Raise ();
    END;
  END Abort;

BEGIN
END M3Process.

