(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Tue Aug  6 11:13:33 PDT 1996 by najork                   *)
(*       Created on Mon Jan 16 10:06:59 PST 1995 by najork                   *)


INTERFACE WinTrestle;

IMPORT ProperSplit, Trestle, TrestleClass, VBT, WinDef;

TYPE
  T <: Trestle.T;

REVEAL
  TrestleClass.RootVBT <: ProperSplit.T;

(* A Child object serves as the mediator between a top-level VBT.T and
   a Trestle.T.  Calling "trsl.beChild(v)" creates a "Child" object which
   contains Windows-specific data for the top-level VBT.T (such as its 
   window handle). The "Child" object is stored in "v.upRef". *)

TYPE 
  Child <: ProperSplit.Child;   (* created by WinTrestle.BeChild *)

PROCEDURE WindowHandle (v: VBT.T): WinDef.HWND;
(* Return the window handle associated with a VBT (or NIL) *)

PROCEDURE Init();

END WinTrestle.
