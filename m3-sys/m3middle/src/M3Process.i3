(* Copyright (C) 1996-2000, Critical Mass, Inc.  All rights reserved.  *)
(* See file COPYRIGHT-CMASS for details. *)

INTERFACE M3Process;

IMPORT OSError, Process;

PROCEDURE Interrupt (t: Process.T)  RAISES {OSError.E};
(* Send an interrupt signal to process 't'.  *)

PROCEDURE Abort (t: Process.T)  RAISES {OSError.E};
(* Make a best effort attempt to stop process 't'.
   The implementation for Win32 here in libm3/m3middle
   is completely incorrect. *)

END M3Process.
