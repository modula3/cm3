(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(* Last Modified On Fri Jun 11 14:52:25 PDT 1993 by kalsow                   *)
(*      Modified On Sat Feb  6 11:41:23 PST 1993 by mjordan                  *)
(*      Modified On Fri May 29 17:37:42 PDT 1992 by muller                   *)

UNSAFE MODULE RTPerfTool;

IMPORT RTParams, Text, WinBase, Ctypes, WinNT, M3toC (*, RTIO*);

PROCEDURE Start (param: TEXT;  VAR w: Handle): BOOLEAN =
  VAR value: TEXT;  c: Ctypes.char; r: Handle; numRead: INTEGER;
  BEGIN
    value := RTParams.Value (param);
    IF value = NIL THEN  RETURN FALSE; END;
    IF Text.Length (value) = 0 THEN  value := param;  END;
    IF NOT StartTool (value, r, w) THEN  RETURN FALSE; END;
    IF WinBase.ReadFile(r, ADR(c), 1, ADR(numRead), NIL) = 0 THEN
      RETURN FALSE;
    END;
    EVAL WinBase.CloseHandle(r);
    RETURN TRUE;
  END Start;

PROCEDURE Close (w: Handle) =
  BEGIN
    EVAL WinBase.CloseHandle(w);
  END Close;

PROCEDURE Send (w: Handle;  at: ADDRESS;  len: CARDINAL): BOOLEAN =
  VAR nWritten: INTEGER;
  BEGIN
    IF WinBase.WriteFile(w, at, len, ADR(nWritten), NIL) = 0 THEN
      RETURN FALSE;
    END;
    RETURN TRUE;
  END Send;

(*-------------------------------------------------------------- internal ---*)

CONST 
  readPort = 0;
  writePort = 1;

TYPE
  Pipe = ARRAY [0..1] OF WinNT.HANDLE;

PROCEDURE ClosePipe (READONLY p: Pipe) =
  BEGIN
    Close (p[readPort]);
    Close (p[writePort]);
  END ClosePipe;

PROCEDURE PrepHandle (old: WinNT.HANDLE): WinNT.HANDLE =
  VAR h, self: WinNT.HANDLE;
  BEGIN
    self := WinBase.GetCurrentProcess ();
    IF WinBase.DuplicateHandle (self, old, self, ADR (h), 0, 1,
                                WinNT.DUPLICATE_SAME_ACCESS) = 0 THEN
      <*ASSERT FALSE*>      
    END;
    RETURN h;
  END PrepHandle;

PROCEDURE Create(cmd: TEXT; stdin, stdout, stderr: WinNT.HANDLE): BOOLEAN=
  VAR
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
    pi: WinBase.PROCESS_INFORMATION;
  BEGIN
    IF WinBase.CreateProcess(
         lpApplicationName := NIL,
         lpCommandLine := M3toC.TtoS(cmd),
         lpProcessAttributes := NIL,
         lpThreadAttributes := NIL,
         bInheritHandles := 1,
         dwCreationFlags := WinBase.NORMAL_PRIORITY_CLASS,
         lpEnvironment := NIL,
         lpCurrentDirectory := NIL,
         lpStartupInfo := ADR(startupInfo),
         lpProcessInformation := ADR(pi)) = 0 THEN 
      Close(startupInfo.hStdInput);
      Close(startupInfo.hStdOutput);
      RETURN FALSE;
    END;

    (* Close unnecessary handles. *)
    Close(startupInfo.hStdInput);
    Close(startupInfo.hStdOutput);
    Close(startupInfo.hStdError);
    EVAL WinBase.CloseHandle(pi.hThread);
    EVAL WinBase.CloseHandle(pi.hProcess);

    RETURN TRUE
  END Create;

PROCEDURE StartTool (name: TEXT; VAR r, w: Handle): BOOLEAN =
  VAR
    toTool   : Pipe;
    fromTool : Pipe;
    errorFromTool: Pipe;
  BEGIN
    (* open a pipe to send bytes to the performance tool *)
    IF WinBase.CreatePipe(hReadPipe := ADR(toTool[readPort]),
                          hWritePipe := ADR(toTool[writePort]),
                          lpPipeAttributes := NIL,
                          nSize := 0 (* use default *)) = 0 THEN
      RETURN FALSE
    END;
 
    (* open a pipe to get bytes from the performance tool *)
    IF WinBase.CreatePipe(hReadPipe := ADR(fromTool[readPort]),
                          hWritePipe := ADR(fromTool[writePort]),
                          lpPipeAttributes := NIL,
                          nSize := 0 (* use default *)) = 0 THEN
      ClosePipe(toTool);
      RETURN FALSE
    END;
  
    (* open a pipe to get bytes from the performance tool *)
    IF WinBase.CreatePipe(hReadPipe := ADR(errorFromTool[readPort]),
                          hWritePipe := ADR(errorFromTool[writePort]),
                          lpPipeAttributes := NIL,
                          nSize := 0 (* use default *)) = 0 THEN
      ClosePipe(toTool);
      ClosePipe(fromTool);
      RETURN FALSE
    END;
  
    IF Create(name, toTool[readPort], fromTool[writePort], 
                errorFromTool[writePort]) = FALSE THEN
      ClosePipe(toTool);
      ClosePipe(fromTool);
      ClosePipe(errorFromTool);
      RETURN FALSE;
    END;

    (* Close the remote end of the pipes, since we sent them different
       handles. *)
    Close(toTool[readPort]);
    Close(fromTool[writePort]);
    Close(errorFromTool[writePort]);
    Close(errorFromTool[readPort]);

    r := fromTool [readPort];
    w := toTool [writePort];
    RETURN TRUE;
  END StartTool;

BEGIN
END RTPerfTool.

