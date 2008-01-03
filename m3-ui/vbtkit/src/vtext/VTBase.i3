(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified On Sun Mar 21 16:29:23 PST 1993 by meehan *)
(*      modified On Tue Jun 16 13:12:46 PDT 1992 by muller *)
(*      modified On Fri Mar 20 10:06:06 PST 1992 by jdd    *)


(* This module contains VTView operations that investigate the buffer's
   representation on the screen. *)

INTERFACE VTBase;

IMPORT Point, Rd, Thread;
IMPORT VTDef;

TYPE
  View = VTDef.View;
  Pixels = VTDef.Pixels;
  I = VTDef.I;
  LineNo = VTDef.LineNo;
  VirtualStart = VTDef.VirtualStart;


PROCEDURE ComputeLine (             view  : View;
                                    avail : Pixels;
                                    from  : I;
                       VAR (* OUT*) max   : I;
                       VAR (* OUT*) turned: BOOLEAN;
                       VAR (* OUT*) width : Pixels    ): I
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted};

(* Computes the characteristics of a screen line starting at "from";
   returns the index after the end. ("From" is believed to be at the
   beginning of a screen line.) "Avail" is the available width in pixels.

   "Max" is set to the index after the last character examined to make the
   decision (the first was "from"). "Turned" is set to whether the end of
   the screen line is turned (if the screen line does not end in a new-line
   and is not at the end of the buffer). "Width" is set to the width in
   pixels needed to display the text. *)


PROCEDURE Up (             view : View;
                           avail: Pixels;
                           place: I;
                           n    : CARDINAL;
              VAR (* OUT*) start: VirtualStart)
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted};

(* Computes the beginning of the screen line "n" lines above the screen
   line that "place" is on. ("Place" is not believed to be at the beginning
   of a screen line.) "Avail" is the available width in pixels. "Min" and
   "max" are set to a half-open interval that includes a set of buffer
   positions that imply this result.

   "Turned" is set to whether the beginning of that screen line is turned
   (if the screen line is not preceded by a new-line and is not at the
   beginning of the buffer). If fewer than "n" screen lines exist, the
   beginning of the buffer is returned. The array "line" is filled in with
   the virtual line info for the lines up to and including "place"; "lines"
   <= "n". *)


PROCEDURE Down (view: View; from: I; n: CARDINAL): I
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted};

(* Computes the beginning of the screen line "n" lines below the screen
   line that "from" is on. ("From" is believed to be at the beginning of a
   screen line.) If fewer than "n" screen lines exist, the end of the
   buffer is returned. *)

PROCEDURE UnsafeLocateLine (view: View; place: I): INTEGER RAISES {};

(* Computes the screen line number in the view that "place" is on (i.e., if
   there were a caret at "place", the screen line on which it would
   appear). (Place is not believed to be at the beginning of a screen
   line.) If "place" is above the view, -1 is returned; if "place" is below
   the view, -2 is returned. UnsafeLocateLine can be called only when the
   "virtual" structure is not dirty, hence its name. *)


PROCEDURE UnsafeLocatePoint (             view : View;
                                          place: I;
                             VAR (* OUT*) p    : Point.T;
                                          off  : CARDINAL  := 1)
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted};

(* Computes the position in the view that "place" appears (i.e., if there
   were a caret at "place", the screen line on which it would appear).
   (Place is not believed to be at the beginning of a screen line.) The
   coordinate is in absolute screen coordinates, and refers to the
   northwest corner of the character at "place". If "place" is above the
   view, p.v is set to -1 and p.h is arbitrary; if "place" is below the
   view, p.v is set to -2 and p.h is arbitrary. UnsafeLocatePoint can be
   called only when the "virtual" structure is not dirty, hence its name.
   If "off" is zero, UnsafeLocatePoint returns the right edge of the
   character instead of the left. *)
   
END VTBase.

