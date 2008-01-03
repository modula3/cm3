(* Copyright 1996-2000, Critical Mass, Inc.   All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

MODULE RTProcessWin32 EXPORTS RTProcess;
IMPORT WinBase, WinDef, Word;

VAR
  time_used : REAL := 0.0;  (* seconds *)
  last_tick : WinDef.DWORD := WinBase.GetTickCount ();

PROCEDURE TimeUsed (): REAL =
  (* NOTE: we're supposed to return the process time, not the system
     time.  But, Win95 doesn't support GetProcessTimes().... *)
  CONST
    H = FLOAT(LAST(INTEGER)) + 1.0;
    H2 = H * 0.002;
  VAR
    tick := WinBase.GetTickCount ();  (* milliseconds *)
    diff := Word.Minus (tick, last_tick);
  BEGIN
    IF (diff >= 0) THEN
      time_used := time_used + (FLOAT (diff) * 0.001);
    ELSE
      time_used := time_used + H2 + (FLOAT (diff) * 0.001);
    END;
    last_tick := tick;
    RETURN time_used;
  END TimeUsed;

BEGIN
END RTProcessWin32.
