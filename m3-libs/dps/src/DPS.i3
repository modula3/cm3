(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Last modified on Fri Jan 14 12:18:19 PST 1994 by kalsow                   *)
(*      modified on Mon Feb 10 17:52:31 PST 1992 by muller                   *)
(*      modified on Mon Feb 10 16:54:50 PST 1992 by ayers                    *)

INTERFACE DPS;

IMPORT Fifo, Wr;

TYPE Button = { Left, Middle, Right };

TYPE Modifier = { Shift, Lock, Ctrl, Option };
TYPE Modifiers = SET OF Modifier;

TYPE ClickType = { FirstDown, OthersDown, Dragging, LastUp };

(* TYPE XEvent = { XExpose, XButtonPress, XButtonRelease }; *)

CONST FullWidth = 1024; FullHeight = 1024;

CONST StandardFontPoints = 20.0;
(* It boggles the mind, but "16.0", above gives a PostScript. *)
(* complaint about the BuittonDLEDrawRoundedPath macro. "14.0" is OK. *)

EXCEPTION BadPostScript (TEXT);

TYPE Place = RECORD x, y: REAL; END;
CONST ZeroPlace = Place {0.0, 0.0};

TYPE Box = RECORD low, high: Place; END;
CONST ZeroBox = Box { Place{0.0,0.0}, Place{0.0,0.0} };
CONST EverywhereBox = Box { Place{-999999.0,-999999.0}, Place{999999.0,999999.0} };

TYPE FixedPoint = { nw, n, ne, w, c, e, sw, s, se }; 

TYPE MouseEvent = RECORD 
  whatChanged: Button; 
  place: Place; 
  modifiers: Modifiers; 
  clickType: ClickType; 
  END;

TYPE KeyEvent = RECORD 
  key: INTEGER;  (* Raw. See CharFromKey. *)
  modifiers: Modifiers; 
  clickType: ClickType; 
  END;

TYPE T = OBJECT 
  (* Change with caution. Congruent definition in ccDisplayPS.c! *)
  dpy: INTEGER := 0; win: INTEGER := 0; fd: INTEGER := 0; 
  ctx: INTEGER := 0; gc: INTEGER := 0; cursor: INTEGER := 0;
  xWidth: INTEGER; xHeight: INTEGER;
  ctm: ARRAY [0..5] OF REAL; (* Current transform via GetTransformWrap. *)
  invctm: ARRAY [0..5] OF REAL; (* Current transform via GetTransformWrap. *)
  xoffset: INTEGER; yoffset: INTEGER; (* Current via GetTransformWrap. *)
  planes: INTEGER := 255;
  desiredWidth: REAL := 0.0; desiredHeight: REAL := 0.0;
  yTranslationNeeded: REAL := 0.0;
  foundationList: REF ARRAY OF TEXT := NIL;
  backgroundTransformation: TEXT := "";
  backgroundTransformationMaintainsSimilarity: BOOLEAN := TRUE;
  backgroundTransformationScaler: REAL := StandardFontPoints; 
  (* ^^ Stepped by 1.0. *)
  currentTransformation: TEXT := "";
  specialWriter: Wr.T := NIL;
  dirtyFifo: Fifo.T;
  alwaysNervous: BOOLEAN := FALSE;
 METHODS
  Create ( width, height: INTEGER := 600; 
   color: BOOLEAN := TRUE; over: T := NIL ) := Create;
  Paint (box: Box; only: REFANY); (* Only via clean-dirty-stuff thread. *)
  Dirty (box: Box; only: REFANY) := Dirty;  
  Mouse (event: MouseEvent): BOOLEAN;
  Char (char: CHAR): BOOLEAN;
  Key (event: KeyEvent);
  Send ( text: TEXT; regardlessOfCircumstance: BOOLEAN := FALSE;
   alreadyLocked: BOOLEAN := FALSE ) RAISES {BadPostScript}:= Send;
  SendClientTransformation (text: TEXT) := SendClientTransformation;
  SendFoundation (text: TEXT) := SendFoundation;
  SendSpecialFoundation (text: TEXT) := SendSpecialFoundation;
  UnsendFoundation (text: TEXT) := UnsendFoundation;
  Flush () := Flush;
  KillInputFocus () := KillInputFocus;
  END;

PROCEDURE GSaveAndClip (box: Box): TEXT;
PROCEDURE GSaveAndClipIf (box, containee: Box): TEXT;
PROCEDURE GRestore (): TEXT;

PROCEDURE NewPathBox (box: Box): TEXT;
PROCEDURE BoxCoordsAsText (box: Box): TEXT;

PROCEDURE BoxAlter (b: Box; fp: FixedPoint; w, h: REAL := -1.0): Box;

PROCEDURE BoxesIntersect (b1, b2: Box): BOOLEAN; 
PROCEDURE ContainerContainee (b1, b2: Box): BOOLEAN; 
PROCEDURE PlaceIsInBox (p: Place; b: Box): BOOLEAN; 
PROCEDURE EdgedBoxClipAndPaint (box: Box; hue: REAL := -1.0): TEXT; (* Hue < 0.0 => Grays. *)

PROCEDURE EscapeText (text: TEXT): TEXT; (* Fixes parens etc. for show. *)

PROCEDURE IsAccent (char: CHAR): BOOLEAN;
PROCEDURE ContainsAccent (text: TEXT): BOOLEAN;

PROCEDURE ShowItAccentedPostScript (text: TEXT): TEXT;

PROCEDURE PreferredFontName (): TEXT;
PROCEDURE SetPreferredFontName (name: TEXT);

PROCEDURE MeasureText ( text: TEXT; window: T; fontName: TEXT;
 accentsHaveWidth: BOOLEAN := FALSE ): REF ARRAY OF REAL; 
(* Returns array of character widths. *)
PROCEDURE MeasureChar ( char: CHAR; window: T; fontName: TEXT;
 accentsHaveWidth: BOOLEAN := FALSE ): REAL;
PROCEDURE TextWidth ( text: TEXT; window: T; fontName: TEXT;
 accentsHaveWidth: BOOLEAN := FALSE ): REAL; 

PROCEDURE BoxUnion (b1, b2: Box): Box; 

PROCEDURE CharFromKey (key: INTEGER; modifiers: Modifiers): CHAR; 

PROCEDURE Create (t: T; width, height: INTEGER := 600; color: BOOLEAN := TRUE; over: T := NIL);

PROCEDURE Dirty (t: T; box: Box; only: REFANY);

PROCEDURE Send ( t: T; text: TEXT;
 regardlessOfCircumstance: BOOLEAN := FALSE;
 alreadyLocked: BOOLEAN := FALSE ) RAISES {BadPostScript};

PROCEDURE SendNervously ( t: T; text: TEXT;
 regardlessOfCircumstance: BOOLEAN := FALSE;
 alreadyLocked: BOOLEAN := FALSE ) RAISES {BadPostScript};

PROCEDURE AcquireDPSMutex (); (* Used by wrap procedures. *)
PROCEDURE ReleaseDPSMutex ();

PROCEDURE SendClientTransformation (t: T; text: TEXT);
PROCEDURE SendFoundation (t: T; text: TEXT);
PROCEDURE SendSpecialFoundation (t: T; text: TEXT);
PROCEDURE UnsendFoundation (t: T; text: TEXT);

PROCEDURE Flush (t: T);
(* Any thread that paints and can block should call Flush before blocking. *)

PROCEDURE KillInputFocus (t: T); (* Any existing focus is lost. *)
(* And nothing remembered: can't get focus back "at the same place." *)

PROCEDURE PostscriptToWriter (t: T; wr: Wr.T);
PROCEDURE PostscriptToText (t: T): TEXT;

PROCEDURE PlaceToStderr (pre: TEXT; place: Place);
PROCEDURE BoxToStderr (pre: TEXT; box: Box);

  END DPS.



