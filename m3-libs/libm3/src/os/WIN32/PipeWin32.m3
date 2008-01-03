(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Last modified on Thu Jul  1 09:59:12 PDT 1993 by mcjones    *)

UNSAFE MODULE PipeWin32 EXPORTS Pipe;

IMPORT FileWin32, OSError, OSErrorWin32, WinBase, WinNT;

PROCEDURE Open(VAR hr, hw: T) RAISES {OSError.E} =
  VAR handleRead, handleWrite: WinNT.HANDLE;
  BEGIN
    IF WinBase.CreatePipe(
      hReadPipe := ADR(handleRead),
      hWritePipe := ADR(handleWrite),
      lpPipeAttributes := NIL,
      nSize := 0 (* use default *)) = 0 THEN OSErrorWin32.Raise()
    END;
    hr := FileWin32.New(handleRead, FileWin32.Read);
    hw := FileWin32.New(handleWrite, FileWin32.Write)
  END Open;

BEGIN
END PipeWin32.
