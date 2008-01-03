(* Copyright (C) 1996-2000, Critical Mass, Inc.  All rights reserved.  *)
(* See file COPYRIGHT-CMASS for details. *)

MODULE M3Process;

IMPORT OSError, OSErrorPosix, Process, Usignal;

PROCEDURE Interrupt (t: Process.T)  RAISES {OSError.E} =
  VAR pid := Process.GetID (t);
  BEGIN
    IF Usignal.kill (pid, Usignal.SIGINT) # 0 THEN
      OSErrorPosix.Raise ();
    END;
  END Interrupt;

PROCEDURE Abort (t: Process.T)  RAISES {OSError.E} =
  VAR pid := Process.GetID (t);
  BEGIN
    IF Usignal.kill (pid, Usignal.SIGKILL) # 0 THEN
      OSErrorPosix.Raise ();
    END;
  END Abort;

BEGIN
END M3Process.

