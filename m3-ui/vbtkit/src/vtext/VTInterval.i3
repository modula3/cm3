(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified On Mon Dec 21 18:27:25 PST 1992 by meehan                   *)
(*      modified On Tue Jun 16 13:12:43 PDT 1992 by muller                   *)
(*      modified On Tue Dec 18 09:15:58 1990 by jdd                          *)

INTERFACE VTInterval;

IMPORT VTDef, VText;

TYPE
  T = VTDef.T;
  ColorScheme = VTDef.ColorScheme;
  I = VTDef.I;
  Index = VTDef.Index;
  Interval = VText.Interval;
  IntervalOptions = VTDef.IntervalOptions;
  IntervalStyle = VTDef.IntervalStyle;
  OnOffState = VTDef.OnOffState;
  Tint = VTDef.Tint;
  View = VTDef.View;

  Private = VTDef.PublicInterval OBJECT
              l, r: I;
              next: Interval := NIL
            END;

REVEAL Interval <: Private;

PROCEDURE New (vt: T; iL, iR: Index; READONLY options: IntervalOptions):
  Interval;

PROCEDURE ExplodeInterval (READONLY     interval      : Interval;
                           VAR (* OUT*) indexL, indexR: Index;
                           VAR (* OUT*) options       : IntervalOptions;
                           VAR (* OUT*) state         : OnOffState       );

PROCEDURE MakeOptions (style                  : IntervalStyle;
                       whiteBlack, whiteStroke: ColorScheme;
                       leading                : Tint           ):
  IntervalOptions;

PROCEDURE Switch (interval: Interval; state: OnOffState) RAISES {VTDef.Error};

PROCEDURE Move (interval: Interval; iL, iR: Index) RAISES {VTDef.Error};

PROCEDURE ChangeOptions (interval: Interval; READONLY options: IntervalOptions)
  RAISES {VTDef.Error};

PROCEDURE Delete (interval: Interval) RAISES {VTDef.Error};

PROCEDURE Close (interval: Interval);

PROCEDURE Fix (vt: T);

PROCEDURE CurrentOptions (view: View; at: I; VAR (*OUT*) from, to: I):
  IntervalOptions;

END VTInterval.
