(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Portions Copyright 1996, Critical Mass, Inc.                *)
(*                                                             *)
(* Last modified on Thu May  2 15:18:05 PDT 1996 by heydon     *)
(*      modified on Tue Dec 20 08:43:00 PST 1994 by kalsow     *)
(*      modified on Wed Sep 22 11:52:08 PDT 1993 by mcjones    *)
(* Written by Mick Jordan, Winter-Spring 1993                  *)

UNSAFE MODULE ProcessWin32 EXPORTS Process;

IMPORT File, FileWin32, M3toC, OSError, OSErrorWin32, Pathname,
  RTProcess, Text, WinDef, WinNT, WinBase, Word;

REVEAL T = BRANDED OBJECT 
    waitOk := TRUE;
    info: WinBase.PROCESS_INFORMATION
  END;

EXCEPTION InternalError; WaitAlreadyCalled;
<* FATAL InternalError, WaitAlreadyCalled *>

PROCEDURE Create(
    cmd: Pathname.T;
    READONLY params: ARRAY  OF TEXT; 
    env: REF ARRAY OF TEXT := NIL;
    wd: Pathname.T := NIL; 
    stdin, stdout, stderr: File.T := NIL)
  : T RAISES {OSError.E} =
  VAR
    t := NEW(T);
    cmdLine := ConvertArgs(cmd, params);
    lpEnvironment: WinDef.LPVOID := NIL;
    wdAddr: WinNT.LPSTR := NIL;
    startupInfo := WinBase.STARTUPINFO{
      cb            := BYTESIZE(WinBase.STARTUPINFO),
      lpReserved    := NIL,
      lpDesktop     := NIL,
      lpTitle       := NIL,
      dwX := 0, dwY := 0, dwXSize := 0, dwYSize := 0,
      dwXCountChars := 0, dwYCountChars := 0, dwFillAttribute := 0,
      dwFlags       := WinBase.STARTF_USESTDHANDLES,
      wShowWindow   := 0,
      cbReserved2   := 0,
      lpReserved2   := NIL,
      hStdInput     := PrepHandle (stdin),
      hStdOutput    := PrepHandle (stdout),
      hStdError     := PrepHandle (stderr)};
  BEGIN
    TRY
      IF env # NIL THEN lpEnvironment := ADR(ConvertEnv(env)[0]) END;
      IF wd # NIL THEN wdAddr := M3toC.TtoS(wd) END;
      IF WinBase.CreateProcess(
    	   lpApplicationName := NIL,
    	   lpCommandLine := cmdLine,
    	   lpProcessAttributes := NIL,
    	   lpThreadAttributes := NIL,
    	   bInheritHandles := 1,
    	   dwCreationFlags := WinBase.NORMAL_PRIORITY_CLASS,
    	   lpEnvironment := lpEnvironment,
    	   lpCurrentDirectory := wdAddr,
    	   lpStartupInfo := ADR(startupInfo),
    	   lpProcessInformation := ADR(t.info)) = 0
      THEN
        OSErrorWin32.Raise()
      END
    FINALLY
      (* close the local copy of any duplicated handles *)
      CloseHandle (startupInfo.hStdInput);
      CloseHandle (startupInfo.hStdOutput);
      CloseHandle (startupInfo.hStdError)
    END;
    RETURN t
  END Create;

PROCEDURE PrepHandle (f: File.T): WinNT.HANDLE
  RAISES {OSError.E} =
  VAR h, self: WinNT.HANDLE;
  BEGIN
    IF (f = NIL) THEN RETURN NIL; END;
    self := WinBase.GetCurrentProcess ();
    IF WinBase.DuplicateHandle (self, f.handle, self, ADR (h), 0, 1,
                                WinNT.DUPLICATE_SAME_ACCESS) = 0 THEN
      OSErrorWin32.Raise();
    END;
    RETURN h;
  END PrepHandle;

PROCEDURE CloseHandle (h: WinNT.HANDLE)
  RAISES {OSError.E} =
  BEGIN
    IF (h = NIL) THEN RETURN END;
    IF WinBase.CloseHandle (h) = 0 THEN
      OSErrorWin32.Raise();
    END;
  END CloseHandle;

PROCEDURE ConvertArgs(cmd: Pathname.T; READONLY params: ARRAY OF TEXT)
  : WinNT.LPCSTR =
  VAR
    l, k := 0;
    result: REF ARRAY OF CHAR;
    cmdL := Text.Length(cmd);
  BEGIN
    INC(l, cmdL+1);
    FOR i := 0 TO NUMBER(params)-1 DO
      INC(l, Text.Length(params[i])+1)
    END;
    result := NEW(REF ARRAY OF CHAR, l+1);
    k := l; l := 0; result[l] := '\000';
    Text.SetChars(result^, cmd); 
    INC(l, cmdL); result[l] := ' '; INC(l);
    FOR i := 0 TO NUMBER(params)-1 DO
      Text.SetChars(SUBARRAY(result^, l, k-l), params[i]);
      INC(l, Text.Length(params[i]));
      result[l] := ' '; INC(l)
    END;
    RETURN ADR(result[0])
  END ConvertArgs;

PROCEDURE ConvertEnv(env: REF ARRAY OF TEXT): REF ARRAY OF CHAR =
  VAR k: CARDINAL; chars: REF ARRAY OF CHAR;
  BEGIN
    k := 0;
    FOR i := 0 TO LAST(env^) DO
      INC(k, Text.Length(env[i]) + 1) (* null-terminated string *)
    END;
    INC(k, 1); (* final null *)
    chars := NEW(REF ARRAY OF CHAR, k);
    k := 0;
    FOR i := 0 TO LAST(env^) DO
      WITH len = Text.Length(env[i]) DO
        Text.SetChars(SUBARRAY(chars^, k, len), env[i]);
        chars[k + len] := '\000';
        INC(k, len + 1)
      END
    END;
    chars[k] := '\000';
    RETURN chars
  END ConvertEnv;

PROCEDURE Wait(p: T): ExitCode =
  VAR status: WinDef.DWORD; error: WinDef.DWORD;
  BEGIN
    IF NOT p.waitOk THEN RAISE WaitAlreadyCalled END;
    p.waitOk := FALSE;
    TRY
      IF WinBase.WaitForSingleObject(p.info.hProcess, WinBase.INFINITE) #
	 WinBase.WAIT_OBJECT_0 THEN RAISE InternalError 
      END;
      IF WinBase.GetExitCodeProcess(p.info.hProcess, ADR(status)) = 0 THEN
	error := WinBase.GetLastError();
	RAISE InternalError
      END;
    FINALLY
      TRY CloseHandle(p.info.hProcess) EXCEPT ELSE END;
      TRY CloseHandle(p.info.hThread) EXCEPT ELSE END;
    END;
    RETURN Word.And(status, LAST(ExitCode))
  END Wait;

PROCEDURE Exit(n: ExitCode) =
  BEGIN
    RTProcess.Exit(n)
  END Exit;

PROCEDURE Crash(msg: TEXT) =
  BEGIN
    RTProcess.Crash(msg)
  END Crash;

PROCEDURE RegisterExitor(p: PROCEDURE()) =
  BEGIN
    RTProcess.RegisterExitor(p)
  END RegisterExitor;

PROCEDURE GetID(p: T): ID =
  BEGIN RETURN LOOPHOLE(p.info.hProcess, ID) END GetID;

PROCEDURE GetMyID(): ID =
  BEGIN RETURN LOOPHOLE(WinBase.GetCurrentProcessId(), ID) END GetMyID;

VAR
  stdin_g  := GetFileHandle(WinBase.STD_INPUT_HANDLE,  FileWin32.Read);
  stdout_g := GetFileHandle(WinBase.STD_OUTPUT_HANDLE, FileWin32.Write);
  stderr_g := GetFileHandle(WinBase.STD_ERROR_HANDLE,  FileWin32.Write);

PROCEDURE GetFileHandle(hd: WinDef.DWORD; ds: FileWin32.DirectionSet): File.T =
  VAR h := WinBase.GetStdHandle(hd);
  BEGIN
    IF (h = NIL)
      OR (h = LOOPHOLE (WinBase.INVALID_HANDLE_VALUE, WinDef.HANDLE)) THEN
      RETURN NIL;
    END;
    TRY RETURN FileWin32.New(h, ds)
    EXCEPT OSError.E => (* not available *)
    END;
    RETURN NIL;
  END GetFileHandle;

PROCEDURE GetStandardFileHandles(VAR stdin, stdout, stderr: File.T) =
  BEGIN
    stdin := stdin_g; stdout := stdout_g; stderr := stderr_g
  END GetStandardFileHandles;

PROCEDURE GetWorkingDirectory(): Pathname.T RAISES {OSError.E} =
  PROCEDURE DoIt(VAR chars: ARRAY OF CHAR): INTEGER RAISES {OSError.E} =
    BEGIN
      WITH rc = WinBase.GetCurrentDirectory(NUMBER(chars), ADR(chars[0])) DO
        IF rc = 0 THEN OSErrorWin32.Raise() END;
        RETURN rc
      END
    END DoIt;
  VAR chars: ARRAY [0..63] OF CHAR; rc := DoIt(chars);
  BEGIN
    IF rc < NUMBER(chars) THEN
      RETURN Text.FromChars(SUBARRAY(chars, 0, rc))
    END;
    WITH refChars = NEW(REF ARRAY OF CHAR, rc+1) DO
      rc := DoIt(refChars^);
      IF rc > NUMBER(refChars^) THEN RAISE InternalError END;
      RETURN Text.FromChars(SUBARRAY(refChars^, 0, rc))
    END
 END GetWorkingDirectory;

PROCEDURE SetWorkingDirectory(path: Pathname.T) RAISES {OSError.E}=
  BEGIN 
    IF WinBase.SetCurrentDirectory(M3toC.TtoS(path)) = 0 THEN
      OSErrorWin32.Raise()
    END
  END SetWorkingDirectory;

BEGIN
END ProcessWin32.
