(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

INTERFACE RTThread;

(*---------------------------------------------------- exception delivery ---*)

PROCEDURE GetCurrentHandlers(): ADDRESS;
(* == RETURN WinBase.TlsGetValue(handlersIndex) *)

PROCEDURE SetCurrentHandlers(h: ADDRESS);
(* == WinBase.TlsSetValue(handlersIndex, h) *)

END RTThread.
