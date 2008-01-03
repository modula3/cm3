(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified On Tue Jun 16 13:12:38 PDT 1992 by muller *)
(*      modified On Wed Aug 21 18:19:41 PDT 1991 by meehan *)
(*      Modified On Fri Sep 11 21:17:00 1987 by jdd *)

(* This module maintains the "real" screen structures. *)

INTERFACE VTReal;

IMPORT Rd, Rect, Thread;
IMPORT VTDef;

TYPE
  T = VTDef.T;
  View = VTDef.View;
  I = VTDef.I;


PROCEDURE Change (vt: T; begin, oEnd, nEnd: I) RAISES {};
PROCEDURE SetStart (view: View; at: I; turned: BOOLEAN) RAISES {};

PROCEDURE Update (vt: T) RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted};

PROCEDURE UpdateView (view: View) RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted};

PROCEDURE Init (view: View) RAISES {};
PROCEDURE Bad (view: View; READONLY bad: Rect.T) RAISES {};

PROCEDURE Resize (view: View; n: CARDINAL) RAISES {};

END VTReal.
