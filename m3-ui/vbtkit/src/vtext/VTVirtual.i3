(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified On Tue Jun 16 13:12:34 PDT 1992 by muller *)
(*      modified On Mon Oct 14 17:45:16 PDT 1991 by meehan *)
(*      Modified On Mon Sep 14 22:51:21 1987 by jdd *)

(* This module maintains the "virtual" screen structures. *)

INTERFACE VTVirtual;

IMPORT Rd, Thread;
IMPORT VTDef;

TYPE
  T = VTDef.T;
  View = VTDef.View;
  I = VTDef.I;


PROCEDURE Change (vt: T; begin, oEnd, nEnd: I) RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted};

PROCEDURE SetStart (view : View;
                    from : I;
                    n    : CARDINAL := 0;
                    force: BOOLEAN  := FALSE)
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted};


PROCEDURE Update (vt: T) RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted};

PROCEDURE UpdateView (view: View) RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted};

PROCEDURE Init (view: View; start: I) RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted};

PROCEDURE Bad (view: View) RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted};

PROCEDURE Resize (view: View; n: CARDINAL) RAISES {};

END VTVirtual.
