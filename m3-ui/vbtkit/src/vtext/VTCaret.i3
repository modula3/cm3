(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified On Tue Jun 16 13:12:45 PDT 1992 by muller *)
(*      modified On Tue Oct 15 16:48:55 PDT 1991 by meehan *)
(*      modified On Fri Sep 11 21:15:11 1987 by jdd *)

(* This module contains caret support for VTs. *)

INTERFACE VTCaret;

IMPORT Rd, Thread, VTDef;

TYPE
  T = VTDef.T;
  I = VTDef.I;
  OnOffState = VTDef.OnOffState;
  View = VTDef.View;

PROCEDURE Init (vt: T) RAISES {};
PROCEDURE InitInView (view: View)
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted};
(* Initialize the caret for a VT. *)

PROCEDURE Switch (vt: T; state: OnOffState)
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted};
PROCEDURE Move (vt: T; place: CARDINAL)
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted};
(* The exported operations, as described in VText.def *)

PROCEDURE Deactivate (view: View) RAISES {};
PROCEDURE Reactivate (view: View)
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted};
(* Deactivate temporarily removes the display of the caret; it will be
   reactivated by Reactivate. Multiple deactivations require multiple
   reactivations. *)

PROCEDURE Close (vt: T) RAISES {};
(* Close a VT's caret. *)

END VTCaret.
