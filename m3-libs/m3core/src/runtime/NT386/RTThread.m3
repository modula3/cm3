(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

UNSAFE MODULE RTThread EXPORTS RTThread, RTHooks;

IMPORT WinDef, WinBase, RTError;

(*--------------------------------------------- exception handling support --*)

VAR handlersIndex: INTEGER := -1;

PROCEDURE GetCurrentHandlers(): ADDRESS=
  BEGIN
    IF (handlersIndex < 0) THEN Init (); END;
    RETURN LOOPHOLE (WinBase.TlsGetValue(handlersIndex), ADDRESS);
  END GetCurrentHandlers;

PROCEDURE SetCurrentHandlers(h: ADDRESS)=
  BEGIN
    IF (handlersIndex < 0) THEN Init (); END;
    EVAL WinBase.TlsSetValue(handlersIndex, LOOPHOLE (h, WinDef.DWORD));
  END SetCurrentHandlers;

(*RTHooks.PushEFrame*)
PROCEDURE PushEFrame (frame: ADDRESS) =
  TYPE Frame = UNTRACED REF RECORD next: ADDRESS END;
  VAR f := LOOPHOLE (frame, Frame);
  BEGIN
    IF (handlersIndex < 0) THEN Init (); END;
    f.next := LOOPHOLE (WinBase.TlsGetValue(handlersIndex), ADDRESS);
    EVAL WinBase.TlsSetValue(handlersIndex, LOOPHOLE (f, WinDef.DWORD));
  END PushEFrame;

(*RTHooks.PopEFrame*)
PROCEDURE PopEFrame (frame: ADDRESS) =
  BEGIN
    IF (handlersIndex < 0) THEN Init (); END;
    EVAL WinBase.TlsSetValue(handlersIndex, LOOPHOLE (frame, WinDef.DWORD));
  END PopEFrame;

PROCEDURE Init () =
  BEGIN
    handlersIndex := WinBase.TlsAlloc();
    IF handlersIndex < 0 THEN
      RTError.Msg ("RTThread.m3", 42,
                   "Win32 failure: unable to get thread-local-storage index");
    END;
  END Init;

BEGIN
END RTThread.
