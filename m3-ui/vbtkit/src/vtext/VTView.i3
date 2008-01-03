(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified On Tue Jun 16 20:53:18 PDT 1992 by muller                   *)
(*      modified On Mon Apr 27  1:27:09 PDT 1992 by meehan                   *)
(*      modified On Wed Mar 25 23:01:11 1992 by steveg                       *)
(*      modified On Tue Dec 18 15:46:10 1990 by jdd                          *)

INTERFACE VTView;

IMPORT Font, Rd, Rect, Thread, VBT;
IMPORT VTDef;

TYPE
  T = VTDef.T;
  ColorScheme = VTDef.ColorScheme;
  I = VTDef.I;
  ErrorCode = VTDef.ErrorCode;
  IntervalStylePrecedence = VTDef.IntervalStylePrecedence;
  Tint = VTDef.Tint;
  VFont = VTDef.VFont;
  VOptions = VTDef.VOptions;
  View = VTDef.View;
  Points = VTDef.Points;

EXCEPTION Error(ErrorCode) (* ! = VTDef.Error !*);

PROCEDURE New (         vt      : T;
                        vbt     : VBT.T;
               READONLY full    : Rect.T;
               READONLY vOptions: VOptions;
                        start   : I         ): View
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted};

PROCEDURE MakeVFont (         font     : Font.T;
                     READONLY printable: SET OF CHAR;
                              whiteTabs: BOOLEAN      ): VFont
  RAISES {VTDef.Error};

PROCEDURE MakeVOptions (vFont: VFont;
                        leftMargin, rightMargin, turnMargin, topMargin,
                          leading: Points;
                        whiteBlack, whiteStroke: ColorScheme;
                        leftOffset             : Points;
                        wrap                   : BOOLEAN;
                        eob                    : BOOLEAN;
                        intervalStylePrecedence: IntervalStylePrecedence):
  VOptions RAISES {};

PROCEDURE Move (view: View; READONLY full, saved: Rect.T; scroll: BOOLEAN)
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted};

PROCEDURE Rescreen (view: View; READONLY cd: VBT.RescreenRec) RAISES {};

PROCEDURE Close (t: View) RAISES {};

PROCEDURE SetPixelOptions (VAR vOptions: VOptions; vbt: VBT.T);

END VTView.
