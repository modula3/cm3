(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified On Sun Mar 21 16:29:21 PST 1993 by meehan *)
(*      modified On Tue Jun 16 13:12:40 PDT 1992 by muller *)
(*      Modified On Fri Sep 11 21:16:26 1987 by jdd *)

(* This file includes the VText "Pounce" operation. *)

INTERFACE VTPounce;

IMPORT Point, Rd, Rect, Thread;
IMPORT VTDef;

TYPE
  View = VTDef.View;
  Pixels = VTDef.Pixels;
  I = VTDef.I;
  LineNo = VTDef.LineNo;
  SelectionMode = VTDef.SelectionMode;
  WhichEnd = VTDef.WhichEnd;



PROCEDURE Locate (             view  : View;
                               p     : Point.T;
                  VAR (* OUT*) iL, iR: I;
                  VAR (* OUT*) lineNo: LineNo;
                  VAR (* OUT*) c     : CHAR     )
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted};


PROCEDURE Extend (               view  : View;
                  VAR (* INOUT*) iL, iR: I;
                                 lineNo: LineNo;
                                 c     : CHAR;
                                 mode  : SelectionMode)
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted};


PROCEDURE Encage (             view: View;
                               p   : Point.T;
                               iL  : I;
                  VAR (* OUT*) iM  : I;
                               iR  : I;
                  VAR (* OUT*) cage: Rect.T   ): WhichEnd
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted};

END VTPounce.

