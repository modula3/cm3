(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman and Stephen Harrison                                    *)
(* Last modified on Tue Jun 16 01:57:00 1992 by steveg   *)

INTERFACE MGRd;

IMPORT
  PaintOp, Rd, TextPort, VTDef;

TYPE
  T <: Rd.T;

TYPE
  Style = RECORD
            intervalStyle: VTDef.IntervalStyle;
            bg, fg       : PaintOp.T;
          END;

CONST
  PastStyle = Style{intervalStyle := VTDef.IntervalStyle.GrayStyle, bg :=
                    PaintOp.Bg, fg := PaintOp.Fg};

  FutureStyle = Style{intervalStyle := VTDef.IntervalStyle.NoStyle, bg :=
                      PaintOp.Bg, fg := PaintOp.Fg};


PROCEDURE FromTextPort (         tp     : TextPort.T;
                        READONLY present: Style;
                        READONLY past                  := PastStyle;
                        READONLY future                := FutureStyle): T;
(* Returns a reader on "tp".  Read actions on the reader are reflected in
   the text port.

   The postion of the textport preceding the current position of the reader
   will be displayed according to "past".  The character after the current
   position will be displayed in "present" and the remaining character in
   "future".

   Editing the textport while reading will have unpredictable results *)

END MGRd.
