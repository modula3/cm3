(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified On Sun Mar 21 16:29:24 PST 1993 by meehan *)
(*      modified On Tue Jun 16 13:12:44 PDT 1992 by muller *)
(*      modified On Wed Mar 25 21:46:11 1992 by steveg *)
(*      modified On Fri Mar 20 10:12:44 PST 1992 by jdd    *)

(* Modified On Mon Sep 25 15:09:34 PDT 1989 by brooks *)

INTERFACE VTDef;

IMPORT Font, MText, MTextRd, PaintOp, Rect, ScrnFont, Thread, VBT;

TYPE
  T = REF RECORD
            mutex    : MUTEX;
            closed   : BOOLEAN;
            mtext    : MText.T;
            length   : CARDINAL;
            intervals: Interval;
            markers  : Marker;
            caret: RECORD
                     index  : I;
                     state  : OnOffState;
                     mutex  : MUTEX;
                     black  : BOOLEAN;
                     blinker: Thread.T;
                   END;
            rd, rrd: MTextRd.T := NIL;
            rdDirty: BOOLEAN;
            views  : View;
          END;

  Interval <: PublicInterval;
  PublicInterval = OBJECT
              METHODS
                left  (): INTEGER;
                right (): INTEGER;
                getOptions (): IntervalOptions
              END;

  View = REF RECORD
               vt         : T;
               vbt        : VBT.T;
               vScreenFont: VScreenFont;
               rect: RECORD full, clip, text, textClip, bad: Rect.T;  END;
               vOptions   : VOptions;
               lineSpacing: Pixels;     (* leading plus character height *)
               lineWidth  : Pixels;     (* available for displaying text *)
               nLines     : INTEGER;   (* number of lines displayed *)
               virtual, newVirtual: Virtual;
               real               : Real;
               caret: RECORD
                        rect             : Rect.T;
                        lineNo           : LineNo;
                        deactivationCount: CARDINAL;
                        black            : BOOLEAN;
                      END;
               next, previous: View;
             END;

TYPE
  IntervalStylePrecedence =
    REF ARRAY [IntervalStyle.HighlightStyle .. IntervalStyle.BoxStyle],
          [IntervalStyle.HighlightStyle .. IntervalStyle.BoxStyle] OF
          BOOLEAN;

  Marker = REF RECORD
                 vt     : T;
                 index  : INTEGER;
                 options: MarkerOptions;
                 state  : OnOffState;
                 next   : Marker;
               END;

  Index = CARDINAL;
  I = INTEGER;
  (* This used to be [(FIRST (Index) - 1) ..  (LAST (Index) + 1)], but now
     that Index = CARDINAL, that won't work.  jdd suggested making this
     INTEGER and see what happens.  So far, so good. *)

  Points = REAL;
  Pixels = INTEGER;
  LineNo = INTEGER;

  IntervalStyle = {NoStyle,     (* ordinary text appearance *)
                   HighlightStyle, (* just applies style colors as
                                      specified *)
                   InverseStyle, (* applies style colors with fg/bg
                                    inverted *)
                   GrayStyle,   (* grayed-out text *)
                   UnderlineStyle, (* 2-pixel thick underline *)
                   ThinUnderlineStyle, (* 1-pixel thick underline *)
                   GrayUnderlineStyle,
                   (* 1-pixel thick gray-textured underline *)
                   BoxStyle,    (* box around the text *)
                   SlugStyle,   (* solid black - illegible *)
                   OverlapStyle}; (* stippled over *)
  OnOffState = {Off, On};
  SelectionMode = {CharSelection, WordSelection, LineSelection,
                   ParagraphSelection, AllSelection};
  TriState = {True, False, Unknown};

  Block = RECORD
            old, new: LineNo;
            length  : CARDINAL;
          END;
  BlockArray = REF ARRAY OF RECORD block: Block END;
  Blocks = RECORD
             n    : CARDINAL;
             block: BlockArray
           END;

  RealLine = RECORD
               valid        : BOOLEAN;
               from, to     : I;
               width        : Pixels;
               turned       : ARRAY [0 .. 1] OF TriState;
               allWhiteBelow: BOOLEAN;
             END;
  RealLines = REF ARRAY OF RECORD realLine: RealLine;  END;
  RealStart = RECORD
                at    : I;
                turned: BOOLEAN;
              END;
  Real = RECORD
           start                 : RealStart;
           dirty                 : BOOLEAN;
           firstDirty, firstAfter: LineNo;
           lines                 : CARDINAL;
           line                  : RealLines;
           blocks                : Blocks;
         END;

  VirtualLine = RECORD
                  valid        : BOOLEAN;
                  from, to, max: I;
                  turned       : BOOLEAN;
                  width        : Pixels;
                END;
  VirtualLines = REF ARRAY OF RECORD virtualLine: VirtualLine;  END;
  VirtualStart = RECORD
                   at, min, max: I;
                   turned      : BOOLEAN;
                 END;
  Virtual = RECORD
              start                 : VirtualStart;
              dirty, bodyDirty      : BOOLEAN;
              firstDirty, firstAfter: LineNo;
              lines                 : CARDINAL;
              line                  : VirtualLines;
            END;

  WordCode = {Special, WhiteSpace, AlphaNumeric};

  VFont = REF RECORD
                handedOut: CARDINAL;
                vFont: RECORD
                         font     : Font.T;
                         printable: SET OF CHAR;
                         whiteTabs: BOOLEAN;
                       END;
              END;

  VScreenFont = REF RECORD
                      vScreenFont: RECORD
                                     vFont      : VFont;
                                     metrics    : ScrnFont.Metrics;
                                     box        : Rect.T;
                                     width      : ARRAY CHAR OF Pixels;
                                     defined    : SET OF CHAR;
                                     paintOpaque: BOOLEAN;
                                   END;
                    END;

  Tint = PaintOp.T;
  ColorScheme = PaintOp.ColorScheme;

  VOptions = RECORD
               vFontxxx      : VFont;
               leftMarginPts : Points;  (* left margin *)
               rightMarginPts: Points;  (* right margin *)
               leftOffsetPts : Points;  (* negative indentation for text *)
               topMarginPts  : Points;  (* top margin of every region *)
               leadingPts    : Points;  (* how many pixels between lines *)
               turnMarginPts: Points;  (* margin for displaying turning
                                          indicator *)
               (* For grubby historical reasons, we keep integer
                  equivalents of these quantities. *)
               leftMargin : Pixels;
               rightMargin: Pixels;
               leftOffset : Pixels;
               topMargin  : Pixels;
               leading    : Pixels;
               turnMargin : Pixels;

               whiteBlack, whiteStroke: ColorScheme;
               wrap: BOOLEAN;   (* perform line-wrapping? *)
               eob : BOOLEAN;   (* display visible end-of-buffer? *)
               intervalStylePrecedence: IntervalStylePrecedence;
               (* if non-NIL, defines style precedence *)
             END;

  IntervalOptions = RECORD
                      style                  : IntervalStyle;
                      whiteBlack, whiteStroke: ColorScheme;
                      leading                : Tint;
                    END;

  MarkerOptions = RECORD
                    whichEnd   : WhichEnd;
                    top, bottom: BOOLEAN;
                    stroke     : Tint;
                  END;
  WhichEnd = {Left, Right};


TYPE
  ErrorCode = {IsNil, IllegalIndex, IllegalRegion, IllegalCoord,
                IllegalDividers, IllegalFont, Closed};

(* 
   IsNil is raised if the vtext passed to a procedure is NIL.
   IllegalIndex indicates a range error (e.g., begin > end).
   IllegalRegion indicates a bad region number or too many regions.
   IllegalCoord indicates a problem with coordinates, such as trying to
       create a region smaller than the minimum height.
   IllegalDividers indicates not enough dividers on a call to Rearrange.
   IllegalFont means an illegal font was used.
   Closed is raised if the vtext has been closed.
*)

EXCEPTION Error(ErrorCode);

CONST
  ErrorCodeTexts =               (* Be sure to keep these in sync! *)
  ARRAY ErrorCode OF
    TEXT {
    "The vtext passed to a procedure was NIL.",
    "Range error, e.g., begin > end.",
    "Bad region number, or too many regions.",
    "Problem with coordinates, e.g., creating a region smaller than the minimum height.",
    "Not enough dividers on a call to Rearrange.",
    "An illegal font was used.", "The vtext has been closed."};

END VTDef.
