(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified On Tue Jun 16 13:12:39 PDT 1992 by muller *)
(*      modified On Fri Mar 20 10:27:21 PST 1992 by jdd *)
(* modified On Sun Nov 24 17:56:03 PST 1991 by meehan *)

(* This file includes internal utility operations for VT *)

INTERFACE VTRd;

IMPORT Rd, Thread;
IMPORT VTDef;

TYPE
  T = VTDef.T;
  I = VTDef.I;

PROCEDURE InitReaderIx (vt: T; index: I)
  RAISES {Rd.Failure, Thread.Alerted};

PROCEDURE Rev (vt: T) RAISES {Rd.Failure, Thread.Alerted};

END VTRd.

