(* Copyright 1989 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* OSUtils.m3                                                  *)
(* Last modified on Tue May 21 13:39:59 PDT 1996 by mcjones    *)
(*      modified on Mon Jun  6 17:53:51 PDT 1994 by birrell    *)

UNSAFE MODULE OSUtilsWin32 EXPORTS OSUtils;

IMPORT File, FileWin32, FileRd, M3toC, OSError, OSErrorWin32,
       WinBase, WinNT;

(* *)
(* DupRd *)
(* *)

REVEAL DupRd = FileRd.T BRANDED OBJECT
  OVERRIDES
    init := MyFileRdInit;
  END;

PROCEDURE MyFileRdInit(rd: FileRd.T;
                       h: File.T): FileRd.T
                       RAISES { OSError.E } =
  VAR
    self := WinBase.GetCurrentProcess();
    hNew: WinNT.HANDLE;
  BEGIN
    IF WinBase.DuplicateHandle(self, h.handle, self, ADR(hNew),
                               0, 0, WinNT.DUPLICATE_SAME_ACCESS) = 0 THEN
      OSErrorWin32.Raise();
    END;
    RETURN FileRd.T.init(
      rd,
      FileWin32.New(hNew, FileWin32.Read));
  END MyFileRdInit;


(* *)
(* Fifo *)
(* *)

VAR
  pFifo: TEXT := NIL;
  hFifo: WinNT.HANDLE;

PROCEDURE CreateFifo(p: TEXT) RAISES {OSError.E} =
  CONST PipePrefix = "\\\\.\\pipe\\";
  VAR handle: WinNT.HANDLE;
  BEGIN
    <* ASSERT pFifo=NIL *>
    handle := WinBase.CreateNamedPipe(
         lpName := M3toC.TtoS(PipePrefix & p),
         dwOpenMode := WinBase.PIPE_ACCESS_INBOUND,
                       (* ***** + FILE_FLAG_OVERLAPPED ????? *)
         dwPipeMode := WinBase.PIPE_TYPE_BYTE
           + WinBase.PIPE_READMODE_BYTE + WinBase.PIPE_WAIT,
         nMaxInstances := 1,
         nOutBufferSize := 512,
         nInBufferSize := 512,
         nDefaultTimeOut := 10000, (* 10 seconds *) (* ***** *)
         lpSecurityAttributes := NIL);
    IF LOOPHOLE(handle, INTEGER) = WinBase.INVALID_HANDLE_VALUE THEN
      OSErrorWin32.Raise();
    END;
    pFifo := p;
    hFifo := handle;
  END CreateFifo;

PROCEDURE OpenFifo(): File.T RAISES {OSError.E} =
  BEGIN
    <* ASSERT pFifo#NIL *>
    IF 0 = WinBase.ConnectNamedPipe(hFifo, NIL) THEN
      OSErrorWin32.Raise();
    END;
    RETURN FileWin32.New(hFifo, FileWin32.Read);
  END OpenFifo;

 
PROCEDURE DeleteFifo() RAISES {} =
  BEGIN
    <* ASSERT pFifo#NIL *>
    pFifo := NIL;
    hFifo := NIL;
  END DeleteFifo;

BEGIN
END OSUtilsWin32.
