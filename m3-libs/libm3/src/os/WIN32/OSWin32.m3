(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Thu Aug 31 14:22:15 PDT 1995 by steveg  *)

UNSAFE MODULE OSWin32;

IMPORT WinBase;

VAR
  inited := FALSE;
  win95 := FALSE;

PROCEDURE Win95(): BOOLEAN =
  BEGIN
    IF NOT inited THEN
      inited := TRUE;
      VAR
        os_version : WinBase.OSVERSIONINFO;
        b: INTEGER;
      BEGIN
        os_version.dwOSVersionInfoSize := BYTESIZE (os_version);
        b := WinBase.GetVersionEx (ADR (os_version));
        <*ASSERT b # 0*>
        win95 := os_version.dwPlatformId = WinBase.VER_PLATFORM_WIN32_WINDOWS;
      END;
    END;
    RETURN win95;
  END Win95;

BEGIN
END OSWin32.
