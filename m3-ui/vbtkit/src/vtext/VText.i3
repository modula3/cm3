(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified On Sun Mar 21 16:29:14 PST 1993 by meehan                *)
(*      modified On Tue Jun 16 13:12:33 PDT 1992 by muller                *)
(*      modified On Wed Mar 25 23:07:48 1992 by steveg                    *)
(*      modified On Fri Mar 20 10:05:27 PST 1992 by jdd                   *)


(* A Visible Text or VText combines an MText and a rectangular screen
   area for displaying it. VText provides a mechanism to maintain the
   correspondence between the MText and the screen, as is needed in
   implementing a display-oriented text editor.

   A VText is not an opaque REF. This allows the VText client to read
   certain parts of it. However, the client must call VText procedures
   to change a VText's state; it must not modify the VText
   representation directly.

   The VText client can call VText to divide the rectangular screen area
   into several regions, each independently positioned to view the same
   or different portions of the underlying MText. Regions are dynamic;
   they can be created and destroyed at will, and the amount of screen
   devoted to each region may be changed. The size of the entire VText
   screen area may also be changed.

   The display will appear as follows: text will be displayed in
   horizontal lines in the specified font. Each line of text is preceded
   by leading pixel rows of blank space. Line turning is performed as
   described in the Ivy spec. As many lines are displayed as possible;
   if the end of the text is reached, or less than a line's height of
   space remains at the bottom of the vbt, or the maximum number of
   lines are being displayed, then the remaining space at the bottom of
   the window is blank. The first character of the text is never
   displayed below the first line; if the text is nonempty, at least one
   character of it is always displayed (the last character is never
   "displayed above the first line").

   The state of a VText includes a Caret (an Index and an on/off state
   bit), zero or more Intervals (a pair of Indexes, an on/off state bit,
   and a style). and zero or more Markers (a Index, an on/off state bit,
   and a style). The Caret is designed to show a text editor's type-in
   point; Intervals are designed to provide feedback for text selections
   and items in forms. If the Caret is on, a blinking Caret appears at
   the position of the Caret's Index. If an Interval is on, all
   characters between the two indexes are displayed with the Interval's
   style. If a Marker is on, that character position displayed in the
   Marker's style.

   Finally, a VText procedure maps from a screen location to the
   position in the MText of the character displayed there. This is
   designed to support mouse input for making text selections.

   (Unlike earlier versions of VText, all coordinates are screen
   coordinates.)

   Screen updating is decoupled from changes to the VText. The only
   VText operations that change the screen are Update and
   ConcurrentUpdate (which completely update the display), and
   SplitRegion, MergeRegion, and Move (which may draw some portion of
   the display if called with scroll = TRUE). A blinking caret also
   updates concurrently. However, all operations that return information
   act as though the screen were constantly kept up-to-date.

   The ConcurrentUpdate operation may modify the screen asynchronously.
   To wait until an operation has finished, use Quiesce. While an
   asynchronous updating operation is in progress, it is not safe to use
   the VText or the MText.

   VTexts are not necessarily monitored. Higher-level synchronization
   must be used to avoid consistency problems if several threads must
   access the same VText, or the MText on which it is based. *)

INTERFACE VText;

IMPORT Font, MText, Point, Rd, Rect, Thread, VBT, VTDef, VTextDef;

TYPE
  T = VTextDef.T;
  ColorScheme = VTDef.ColorScheme;
  ErrorCode = VTDef.ErrorCode;
  Index = VTDef.Index;
  Interval = VTDef.Interval;
  IntervalOptions = VTDef.IntervalOptions;
  IntervalStyle = VTDef.IntervalStyle;
  IntervalStylePrecedence = VTDef.IntervalStylePrecedence;
  Marker = VTDef.Marker;
  MarkerOptions = VTDef.MarkerOptions;
  OnOffState = VTDef.OnOffState;
  Pixels = VTDef.Pixels;         (* INTEGER *)
  Points = VTDef.Points;         (* REAL *)
  Region = VTextDef.Region;
  SelectionMode = VTDef.SelectionMode;
  Tint = VTDef.Tint;
  VFont = VTDef.VFont;
  VOptions = VTDef.VOptions;
  WhichEnd = VTDef.WhichEnd;


EXCEPTION
  Error (ErrorCode);
  
(************************************************************************)
(*				 Creation				*)
(************************************************************************)


PROCEDURE New (         mtext   : MText.T;
                        vbt     : VBT.T;
               READONLY rect    : Rect.T;
               READONLY vOptions: VOptions ): T
  RAISES {VTDef.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted};
(* Creates a new VText, with mtext as its initial contents, to be displayed
   in the VBT vbt in the rectangle rect. The caller passes control of this
   rectangle to the VText package, and must not paint into it until the
   VText is Closed. The VText returned by New has regionMax = 0, caretState
   = Off, an empty set of Intervals, and an empty set of Markers. New does
   not paint the display. *)


PROCEDURE ExplodeVText (READONLY     vtext   : T;
                        VAR (* OUT*) mtext   : MText.T;
                        VAR (* OUT*) vbt     : VBT.T;
                        VAR (* OUT*) rect    : Rect.T;
                        VAR (* OUT*) vOptions: VOptions ) RAISES {};
(* Given a VText, return its components *)


PROCEDURE MakeVFont (         font     : Font.T;
                     READONLY printable: SET OF CHAR;
                              whiteTabs: BOOLEAN      ): VFont
  RAISES {VTDef.Error};
(* Creates a VFont, which is a massaged version of a Font.T. If a character
   exists in the font and is in "printable" and is not a tab or new-line,
   it will be displayed from the font. If '\t' is not in printable, tabs
   will display as \011. If '\t' is in printable, then tabs will display as
   white-space to the next tab-stop (multiple of 8 space-widths) if
   whiteTabs is true, or as a special glyph of that width if whiteTabs is
   false. *)


PROCEDURE ExplodeVFont (READONLY     vFont    : VFont;
                        VAR (* OUT*) font     : Font.T;
                        VAR (* OUT*) printable: SET OF CHAR;
                        VAR (* OUT*) whiteTabs: BOOLEAN      ) RAISES {};
(* Given a VFont, returns its components *)


PROCEDURE MakeVOptions (vFont: VFont;
                        leftMargin, rightMargin, turnMargin, topMargin,
                          leading: Points;
                        whiteBlack, whiteStroke: ColorScheme;
                        leftOffset             : Points;
                        wrap: BOOLEAN;  (* do line-wrapping *)
                        eob: BOOLEAN (* display a visible end-of-buffer *);
                        intervalStylePrecedence: IntervalStylePrecedence := NIL):
  VOptions RAISES {};
(* Creates a VOptions, which encapsulates some VText options *)
(* If intervalStylePrecedence is non-NIL and intervalStylePrecedence[i,j]
   is TRUE, then an overlap of style i and style j is painted as i;
   otherwise, it is painted as OverlapStyle. Regardless, NoStyle has lower
   precedence than any style, and SlugStyle and OverlapStyle have higher
   precedence. Margin, leading and offset values given in points.*)


PROCEDURE ExplodeVOptions (READONLY     vOptions: VOptions;
                           VAR (* OUT*) vFont   : VFont;
                           VAR (* OUT*) leftMargin, rightMargin, turnMargin,
                                        topMargin, leading: Points;
                           VAR (* OUT*) whiteBlack, whiteStroke: ColorScheme;
                           VAR (* OUT*) leftOffset: Points;
                           VAR (* OUT*) wrap: BOOLEAN;
                           VAR (* OUT*) eob: BOOLEAN;
                           VAR (* OUT*) intervalStylePrecedence:
                                            IntervalStylePrecedence)
  RAISES {};
(* Given a VOptions, return its components *)


PROCEDURE ChangeVOptions (vtext: T; READONLY vOptions: VOptions)
  RAISES {VTDef.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted};
(* Changes the VOptions for a VText *)


PROCEDURE Close (vtext: T) RAISES {VTDef.Error};
(* Destroy a VText. This vtext will not paint in its vbt any more. The
   display of the text is not erased. *)
   
(************************************************************************)
(*				 Regions				*)
(************************************************************************)

PROCEDURE SplitRegion (vtext : T;
                       r     : Region;
                       v     : Pixels;
                       scroll: BOOLEAN  := TRUE)
  RAISES {VTDef.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted};
(* Splits region r to create two Regions. Other Regions are unaffected,
   except that region numbers are reassigned to preserve the property that
   they increase from the top of the screen towards the bottom. The Region
   being split will be divided by a horizontal black line, one pixel high,
   v pixels below top of the existing Region. If scroll is TRUE, screen
   bits may be moved to reduce the cost of later Updates.

   SplitRegion raises Error if a new Region cannot be created (because
   vtext.regionMax = LAST(Region)). *)


PROCEDURE MergeRegion (vtext: T; i, j: Region; scroll: BOOLEAN := TRUE)
  RAISES {VTDef.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted};
(* Destroys Region i and assigns its screen area to Region j, which must be
   adjacent to i (j = i+1 or j = i-1). Other Regions are unaffected, except
   that region numbers are reassigned to preserve the property that they
   increase from the top of the screen towards the bottom. The top line of
   region j remains the same.

   If scroll is TRUE, screen bits may be moved to reduce the cost of later
   Updates.

   MergeRegion raises Error if i and j are not adjacent. *)

(************************************************************************)
(* Moving *)
(************************************************************************)


PROCEDURE Move (         vtext             : T;
                READONLY newRect, savedRect: Rect.T;
                READONLY dividers          : ARRAY OF Pixels;
                         scroll            : BOOLEAN          := TRUE)
  RAISES {VTDef.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted};
(* Moves the display rectangle or the region dividers within it, or both.
   The dividers array tells the new v coordinate (absolute) of the divider
   below each region except the last. If scroll is TRUE, screen bits may be
   moved to reduce the cost of later Updates. Move raises Error if
   NUMBER(dividers) < vtext^.regionMax. *)


PROCEDURE Rescreen (vtext: T; READONLY cd: VBT.RescreenRec) RAISES {};
(* Passes a Rescreen event to a VText.  VBT.mu must be locked, but not
   exclusively for this activation.  *)

(************************************************************************)
(*				 Drawing				*)
(************************************************************************)

PROCEDURE Update (vtext: T)
  RAISES {VTDef.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted};
(* Paints the display to make it consistent with the state of vtext *)

PROCEDURE ConcurrentUpdate (vtext: T)
  RAISES {VTDef.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted};
(* Like Update, but may return before finished *)

PROCEDURE Quiesce (vtext: T) RAISES {VTDef.Error};
(* Returns only after any outstanding ConcurrentUpdate has completed *)

PROCEDURE Bad (vtext: T; READONLY bad: Rect.T) RAISES {VTDef.Error};
(* Specifies that a given rectangle is bad, and should be completely
   redrawn at the next Update *)

(************************************************************************)
(*			        Editing   				*)
(************************************************************************)

PROCEDURE Replace (vtext: T; begin, end: Index; newText: TEXT)
  RAISES {VTDef.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted};
(* Replaces the text between "begin" and "end" with "newText".  If "begin" and
   "end" are at the same place, this is a pure insertion; if "newText" is
   empty, this is a pure deletion. *)

PROCEDURE ReplaceChars (         vtext     : T;
                                 begin, end: Index;
                        READONLY str       : ARRAY OF CHAR)
  RAISES {VTDef.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted};
(* " = Replace (vtext, begin, end, Text.FromChars (str)) " *)

PROCEDURE ReplaceFile (vtext     : T;
                       begin, end: Index;
                       file      : Rd.T;
                       start     : Index   := 0;
                       numChars  : Index   := LAST(Index))
  RAISES {VTDef.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted};
(* Inserts a file into the text at h. The whole file might not be read in
   at once; some reading may be delayed. *)

PROCEDURE Invalidate (vtext: T; begin, oldEnd, newEnd: Index)
  RAISES {VTDef.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted};
(* An excessively low-level interface; tells VText that the underlying
   MText has has the interval [begin,oldEnd) replaced by [begin,newEnd). *)

(************************************************************************)
(* The Caret *)
(************************************************************************)


PROCEDURE SwitchCaret (vtext: T; state: OnOffState)
  RAISES {VTDef.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted};
(* Sets vtext's caretState := state *)

PROCEDURE MoveCaret (vtext: T; place: Index)
  RAISES {VTDef.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted};
(* Moves caret to place *)

PROCEDURE CaretIndex (vtext: T): Index RAISES {VTDef.Error};
(* Returns the caret's index *)

(************************************************************************)
(* Intervals *)
(************************************************************************)

(* Clients should beware of creating a needlessly large number of Intervals
   - they may make redisplaying slower. If two Intervals with state = On
   and different styles overlap, the resulting highlight will be displayed
   in OverlapStyle. The NoHighlight and OverlapStyle styles should not be
   used by client programs. *)


PROCEDURE CreateInterval (         vtext         : T;
                                   indexL, indexR: Index;
                          READONLY options       : IntervalOptions):
  Interval RAISES {VTDef.Error};
(* Creates an Interval with indexL = begin, indexR = end, style = style,
   and state = Off *)


PROCEDURE ExplodeInterval (READONLY     interval      : Interval;
                           VAR (* OUT*) indexL, indexR: Index;
                           VAR (* OUT*) options: IntervalOptions;
                           VAR (* OUT*) state: OnOffState) RAISES {};
(* Explodes an Interval *)


PROCEDURE MakeIntervalOptions (style                  : IntervalStyle;
                               whiteBlack, whiteStroke: ColorScheme;
                               leading                : Tint           ):
  IntervalOptions RAISES {};
(* Makes a IntervalOptions *)


PROCEDURE ExplodeIntervalOptions (READONLY intervalOptions: IntervalOptions;
                                  VAR (* OUT*) style: IntervalStyle;
                                  VAR (* OUT*) whiteBlack, whiteStroke:
                                                   ColorScheme;
                                  VAR (* OUT*) leading: Tint) RAISES {};
(* Explodes a IntervalOptions *)


PROCEDURE SwitchInterval (interval: Interval; state: OnOffState)
  RAISES {VTDef.Error};
(* Sets interval's state := state *)


PROCEDURE MoveInterval (interval: Interval; indexL, indexR: Index)
  RAISES {VTDef.Error};
(* Moves the interval *)


PROCEDURE ChangeIntervalOptions (         interval: Interval;
                                 READONLY options : IntervalOptions)
  RAISES {VTDef.Error};
(* Re-sets interval's options *)


PROCEDURE DeleteInterval (interval: Interval) RAISES {VTDef.Error};
(* Sets interval's state = Off and then deletes interval from the set of
   intervals associated with the VText *)

(************************************************************************)
(*			     Markers					*)
(************************************************************************)

PROCEDURE CreateMarker (vtext: T; at: Index; options: MarkerOptions):
  Marker RAISES {VTDef.Error};
(* Creates an Marker at the character position with state = Off *)


PROCEDURE ExplodeMarker (READONLY      marker: Marker;
                         VAR (* OUT *) at    : Index;
                         VAR (* OUT *) options: MarkerOptions;
                         VAR (* OUT*) state: OnOffState) RAISES {};
(* Explodes a Marker *)


PROCEDURE MakeMarkerOptions (whichEnd   : WhichEnd;
                             top, bottom: BOOLEAN;
                             stroke     : Tint      ): MarkerOptions
  RAISES {};
(* Makes a MarkerOptions *)


PROCEDURE ExplodeMarkerOptions (READONLY     markerOptions: MarkerOptions;
                                VAR (* OUT*) whichEnd     : WhichEnd;
                                VAR (* OUT*) top, bottom  : BOOLEAN;
                                VAR (* OUT*) stroke       : Tint           )
  RAISES {};
(* Explodes a MarkerOptions *)

PROCEDURE SwitchMarker (marker: Marker; state: OnOffState)
  RAISES {VTDef.Error};
(* Sets marker's state := state *)

PROCEDURE ChangeMarkerOptions (marker: Marker; options: MarkerOptions)
  RAISES {VTDef.Error};
(* Re-sets marker's options *)


PROCEDURE MoveMarker (marker: Marker; place: Index) RAISES {VTDef.Error};
(* Moves the marker to the character position *)

PROCEDURE DeleteMarker (marker: Marker) RAISES {VTDef.Error};
(* Sets marker's state = Off and then deletes marker from the set of
   markers associated with the VText *)

(************************************************************************)
(*		                Scrolling				*)
(************************************************************************)

PROCEDURE Scroll (vtext: T; r: Region; displacement: INTEGER)
  RAISES {VTDef.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted};
(* Scrolls the display within region r. The displacement parameter encodes
   both the direction and distance to scroll. If displacement > 0 the
   region scrolls displacement lines toward-end-of-file; if displacement <
   0 the region scrolls -displacement lines toward-start-of-file; if
   displacement = 0 nothing happens. This procedure enforces the
   constraints on text display as described under New. After calling
   Scroll, the client can call StartIndex to see precisely what
   happened. *)


PROCEDURE SetStart (vtext  : T;
                    r      : Region;
                    place  : Index;
                    upLines: CARDINAL := 0;
                    force  : BOOLEAN  := FALSE)
  RAISES {VTDef.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted};
(* Scrolls the display within region r. The place parameter indicates a
   character position that should be displayed in the line upLines from the
   top of the display, if possible, after scrolling. If place does not
   correspond to the start of a line, according to the line-breaking
   algorithm, then place will not be displayed at the start of a line. If
   force is TRUE AND upLines is zero, display is constrained to occur at
   the given place; otherwise, this procedure enforces the constraints on
   text display as described under New; after calling SetStart, the client
   can call StartIndex to see precisely what happened. *)

(************************************************************************)
(* Line-Breaking *)
(************************************************************************)

PROCEDURE LinesBetween (vtext     : T;
                        begin, end: Index;
                        max       : CARDINAL;
                        avail     : Pixels      := UseCurrentWidth; ):
  INTEGER RAISES {VTDef.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted};

(* Returns the number of lines that would appear between the two indexes if
   they were both visible on the screen. "Avail" is the available screen
   width; if avail=UseCurrentWidth (below), the current screen width is
   used. Returns 0 if they would be on the same line, 1 if they would be on
   adjacent lines, and "max" if they would be more than "max" lines apart.
   Returns -1 if the second would be before the first. *)

CONST UseCurrentWidth = -1;


PROCEDURE ComputeLine (              vtext : T;
                                     from  : Index;
                       VAR (* OUT *) max   : Index;
                       VAR (* OUT *) turned: BOOLEAN;
                       VAR (* OUT *) width : Pixels    ): Index
  RAISES {VTDef.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted};
(* Computes the characteristics of a screen line starting at "from";
   returns the index after the end. ("From" is trusted to be at the
   beginning of a screen line.) ComputeLine pretends that an end-of-file
   character exists following the characters in the mtext, so the input and
   the output index may exceed the length of the mtext by 1. "Max" is set
   to the index after the last character examined to make the decision (the
   first was "from"). "Turned" is set to whether the end of the screen line
   is turned (if the screen line does not end in a new-line and is not at
   the end of the buffer). "Width" is set to the width in pixels needed to
   display the text. *)


PROCEDURE UpLines (vtext: T; place: Index; n: CARDINAL; r: Region := 0):
  Index
  RAISES {VTDef.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted};
(* Find the beginning of the nth line above place. If n=0, find the
   beginning of the line containing place. *)

(************************************************************************)
(* Locations *)
(************************************************************************)

PROCEDURE StartIndex (vtext: T; r: Region): Index RAISES {VTDef.Error};
(* Returns the start of the region, as an index *)

PROCEDURE LineIndex (vtext: T; r: Region; n: CARDINAL): Index
  RAISES {VTDef.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted};
(* Returns the index of the position displayed at the beginning of line n.
   N >= 0. If n >= the number of lines in the region, the index of the
   first character following the region is returned. *)

PROCEDURE CharsInRegion (vtext: T; r: Region): CARDINAL
  RAISES {VTDef.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted};

PROCEDURE Locate (              vtext: T;
                                r    : Region;
                                place: Index;
                  VAR (* OUT *) h, v : INTEGER )
  RAISES {VTDef.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted};
(* Sets h and v to the position specified by place, which will be on a
   baseline. If place is not visible, v will be set to a negative
   number. *)

PROCEDURE InRegion (vtext: T; r: Region; place: Index): BOOLEAN
  RAISES {VTDef.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted};
(* Returns TRUE iff the position specified by place is visible in the
   region. *)

PROCEDURE WhichLine (vtext: T; r: Region; v: Pixels): CARDINAL
  RAISES {VTDef.Error};
(* Returns the line number of the line in region r which contains v. V is
   relative to the top of region r. It does not matter whether there is
   actually any text at that line; the result of WhichLine depends only on
   v, the height of r, and the font being used by vtext. *)

PROCEDURE Pounce (              vtext                 : T;
                                r                     : Region;
                                p                     : Point.T;
                                mode                  : SelectionMode;
                  VAR (* OUT *) indexL, indexM, indexR: Index;
                  VAR (* OUT *) cage                  : Rect.T         ):
  WhichEnd
  RAISES {VTDef.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted};
(* Maps a screen location to the run of characters "pointed to" by (h, v).
   Pounce sets indexL and indexR to the left and right character
   boundaries, in units of mode, of the smallest run of characters
   bracketing the given location. The "unit" of each SelectionMode is
   described in the Ivy spec. The result of Pounce indicates which end is
   "closer" to the location. The distance metric is: if both are on the
   same line, h-distance in pixels, with ties returning Left; if both are
   on two different lines, v-distance in lines, with ties returning Left.
   "indexM" is set to a pleasant insertion point near to that end. A cage
   is returned, but in the screen co-ordinate system. *)

PROCEDURE PounceLocate (              vtext         : T;
                                      r             : Region;
                                      p             : Point.T;
                        VAR (* OUT *) indexL, indexR: Index;
                        VAR (* OUT *) lineNumber    : CARDINAL;
                        VAR (* OUT *) c             : CHAR      )
  RAISES {VTDef.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted};
(* Performs the Locate part of Pounce. The input point "p" is in screen
   coordinates. The out indexes "indexL" and "indexR" will be at most one
   character apart; they define the position pointed to. The output
   "lineNumber" is the screen line number pointed to in the view. The
   output "c" is the character pointed to, if indexL < indexR. *)

PROCEDURE PounceExtend (                vtext         : T;
                                        r             : Region;
                        VAR (* INOUT *) indexL, indexR: Index;
                                        lineNumber    : CARDINAL;
                                        c             : CHAR;
                                        mode          : SelectionMode)
  RAISES {VTDef.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted};
(* Performs the Extend part of Pounce. The input values "indexL", "indexR",
   "lineNumber", and "c" are as returned by PounceLocate. The output values
   "indexL" and "indexR" are the new bounds for the selection as dictated
   by "mode". *)

PROCEDURE PounceEncage (             vtext : T;
                                     r     : Region;
                                     p     : Point.T;
                                     indexL: Index;
                        VAR (* OUT*) indexM: Index;
                                     indexR: Index;
                        VAR (* OUT*) cage  : Rect.T   ): WhichEnd
  RAISES {VTDef.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted};
(* Performs the Encage part of Pounce. The input point "p" is the input to
   PounceLocate. The inputs "indexL" and "indexR" are the outputs of
   PounceLocate or PounceExtend. The output "indexM" is a reasonable
   insertion point for the selection; the output "cage" is the cage of
   values "p" for which the Pounce functions will return the same
   values. *)

END VText.

