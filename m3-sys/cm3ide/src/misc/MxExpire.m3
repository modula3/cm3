(* Copyright 1996 Critical Mass, Inc. All rights reserved.    *)

MODULE MxExpire;

IMPORT OS, Params;

CONST
  Day     = 24 * 3600;

VAR
  n_used  := 0;
  checked := FALSE;

PROCEDURE NDaysUsed (): INTEGER =
  VAR
    prog  := Params.Get (0);
    file  : TEXT;
    ftime : OS.FileTime;
  BEGIN
    IF (NOT checked) THEN
      checked := TRUE;
      file := OS.FindExecutable (prog);
      ftime := OS.LastModified (file);
      IF (ftime = OS.NO_TIME)
        THEN n_used := 0;
        ELSE n_used := ABS(OS.Now () - ftime) DIV Day;
      END;
    END;
    RETURN n_used;
  END NDaysUsed;

BEGIN
END MxExpire.


