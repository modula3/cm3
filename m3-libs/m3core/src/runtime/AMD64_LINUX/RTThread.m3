(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Mon Nov 21 11:28:44 PST 1994 by kalsow     *)
(*      modified on Tue May  4 18:49:28 PDT 1993 by muller     *)

UNSAFE MODULE RTThread EXPORTS RTThread, RTHooks;

IMPORT Usignal;

PROCEDURE SP (READONLY s: State): ADDRESS =
  BEGIN
    (* no user thread support *)
    RETURN NIL;
  END SP;

(*--------------------------------------------------------- thread stacks ---*)

PROCEDURE NewStack (size: INTEGER;  VAR(*OUT*)s: Stack) =
  BEGIN
    (* no user thread support *)
  END NewStack;

PROCEDURE DisposeStack (VAR s: Stack) =
  BEGIN
    (* no user thread support *)
  END DisposeStack;

PROCEDURE FlushStackCache () =
  BEGIN
    (* no user thread support *)
  END FlushStackCache;

(*-------------------------------------------------- modifying the models ---*)

PROCEDURE UpdateStateForNewSP (VAR s: State; offset: INTEGER) =
  BEGIN
    (* no user thread support *)
  END UpdateStateForNewSP;

PROCEDURE UpdateFrameForNewSP (<*UNUSED*> a: ADDRESS;
                               <*UNUSED*> offset: INTEGER) =
  BEGIN
  END UpdateFrameForNewSP;

(*------------------------------------ manipulating the SIGVTALRM handler ---*)

PROCEDURE setup_sigvtalrm (handler: Usignal.SignalHandler) =
  BEGIN
    (* no user thread support *)
  END setup_sigvtalrm;

PROCEDURE allow_sigvtalrm () =
  BEGIN
    (* no user thread support *)
  END allow_sigvtalrm;

PROCEDURE disallow_sigvtalrm () =
  BEGIN
    (* no user thread support *)
  END disallow_sigvtalrm;

BEGIN
END RTThread.
