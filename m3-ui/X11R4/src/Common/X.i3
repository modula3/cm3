(* Copyright (C) 1989, 1990 Digital Equipment Corporation       *)
(* All rights reserved.                                         *)
(* See the file COPYRIGHT for a full description.               *)

(* File: X.i3                                                   *)
(* Last modified on Fri May 20 19:28:17 PDT 1994 by msm         *)
(*      modified on Wed Nov 24 15:22:31 PST 1993 by steveg      *)
(*      modified on Wed Aug 25 09:38:13 PDT 1993 by kalsow      *)
(*      modified on Tue May 11 09:21:55 PDT 1993 by muller      *)
(*      modified on Fri May  7 15:55:30 PDT 1993 by mjordan     *)
(*      modified on Mon Apr  8 22:18:20 PDT 1991 by gnelson     *)
(*      modified on Thu Mar  8 16:44:51 1990 by jerome          *)
(*      modified on Sun Feb 18 14:00:55 1990 by harrison        *)


UNSAFE INTERFACE X;

(*==============================================================*)
(*  The X11 R4 Interface for Modula 3                           *)
(*                                                              *)
(*  contains:   /usr/include/X11/X.h                            *)
(*              /usr/include/X11/Xlib.h                         *)
(*              /usr/include/X11/Xutil.h                        *)
(*==============================================================*)

(***********************************************************
Copyright 1987 by Digital Equipment Corporation, Maynard, Massachusetts,
and the Massachusetts Institute of Technology, Cambridge, Massachusetts.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its
documentation for any purpose and without fee is hereby granted,
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in
supporting documentation, and that the names of Digital or MIT not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************)

FROM Ctypes IMPORT char, char_star, char_star_star, char_star_star_star,
                   const_char_star, const_unsigned_char_star,
                   int, int_star, long, long_star, short,
                   unsigned_char, unsigned_char_star, unsigned_char_star_star,
                   unsigned_int, unsigned_int_star, unsigned_long,
                   unsigned_long_star, unsigned_short, void_star;
IMPORT Word, XMachine;

EXCEPTION Error;
(* If a Modula-3 client (like Trestle) registers an error handler with
   X, then the handler may raise this exception.  Most X calls
   can then result in Error being raised *)


(***************************************************************

                /usr/include/X11/X.h

 ***************************************************************)

(* Definitions for the X window system likely to be used by applications *)

CONST
  X_PROTOCOL            =  11;  (* current protocol version *)
  X_PROTOCOL_REVISION   =  0;   (* current minor version *)

(* Resources *)

TYPE
  Int = int; Short = short; Long = long; Char = char;

  Argv = UNTRACED REF ARRAY [0..255] OF char_star;

  Enumeration   =  Int;

  XID           =  XMachine.XID;

  Window        =  XID;
  WindowStar    =  UNTRACED REF Window;
  WindowStarStar    =  UNTRACED REF WindowStar;

  Drawable      =  XID;
  Font          =  XID;
  Pixmap        =  XID;
  PixmapStar    =  UNTRACED REF Pixmap;
  Cursor        =  XID;
  Colormap      =  XID;
  ColormapStar  =  UNTRACED REF Colormap;
  GContext      =  XID;
  KeySym        =  XMachine.KeySym;
  KeySymStar    =  UNTRACED REF KeySym;

  Mask          =  unsigned_long;

  Atom          =  unsigned_long;
  AtomStar      =  UNTRACED REF Atom;
  AtomStarStar  =  UNTRACED REF AtomStar;

  VisualID      =  unsigned_long;

  Time          =  XMachine.Time;
  TimeStar      =  UNTRACED REF Time;

  KeyCode       =  unsigned_char;
  KeyCodeStar   =  UNTRACED REF KeyCode;
  KeyCodeStarStar   =  UNTRACED REF KeyCodeStar;

TYPE
  RetIntProc        =  PROCEDURE (): Int;
  Retchar_starProc  =  PROCEDURE (): char_star;


(*****************************************************************
 * RESERVED RESOURCE AND CONSTANT DEFINITIONS
 *****************************************************************)

CONST
  None  =                       0;      (* universal null resource or null atom *)
  ParentRelative  =             1;      (* background pixmap in CreateWindow
                                           and ChangeWindowAttributes *)
  CopyFromParent  =             0;      (* border pixmap in CreateWindow
                                           and ChangeWindowAttributes
                                           special VisualID and special window
                                           class passed to CreateWindow *)
  PointerWindow  =              0;      (* destination window in SendEvent *)
  InputFocus  =                 1;      (* destination window in SendEvent *)
  PointerRoot  =                1;      (* focus window in SetInputFocus *)
  AnyPropertyType  =            0;      (* special Atom, passed to GetProperty *)
  AnyKey  =                     0;      (* special Key Code, passed to GrabKey *)
  AnyButton  =                  0;      (* special Button Code, passed to GrabButton *)
  AllTemporary  =               0;      (* special Resource ID passed to KillClient *)
  CurrentTime  =                0;      (* special Time *)
  NoSymbol  =                   0;      (* special KeySym *)

(*****************************************************************
 * EVENT DEFINITIONS
 *****************************************************************)

(* Input Event Masks. Used as event-mask window attribute and as arguments
   to Grab requests.  Not to be confused with event names.  *)

  NoEventMask =                 0 ;
  KeyPressMask =                Word.Shift (1, 0)  ;
  KeyReleaseMask =              Word.Shift (1, 1)  ;
  ButtonPressMask =             Word.Shift (1, 2)  ;
  ButtonReleaseMask =           Word.Shift (1, 3)  ;
  EnterWindowMask =             Word.Shift (1, 4)  ;
  LeaveWindowMask =             Word.Shift (1, 5)  ;
  PointerMotionMask =           Word.Shift (1, 6)  ;
  PointerMotionHintMask =       Word.Shift (1, 7)  ;
  Button1MotionMask =           Word.Shift (1, 8)  ;
  Button2MotionMask =           Word.Shift (1, 9)  ;
  Button3MotionMask =           Word.Shift (1, 10) ;
  Button4MotionMask =           Word.Shift (1, 11) ;
  Button5MotionMask =           Word.Shift (1, 12) ;
  ButtonMotionMask =            Word.Shift (1, 13) ;
  KeymapStateMask =             Word.Shift (1, 14) ;
  ExposureMask =                Word.Shift (1, 15) ;
  VisibilityChangeMask =        Word.Shift (1, 16) ;
  StructureNotifyMask =         Word.Shift (1, 17) ;
  ResizeRedirectMask =          Word.Shift (1, 18) ;
  SubstructureNotifyMask =      Word.Shift (1, 19) ;
  SubstructureRedirectMask =    Word.Shift (1, 20) ;
  FocusChangeMask =             Word.Shift (1, 21) ;
  PropertyChangeMask =          Word.Shift (1, 22) ;
  ColormapChangeMask =          Word.Shift (1, 23) ;
  OwnerGrabButtonMask =         Word.Shift (1, 24) ;

(* Event names.  Used in "type" field in XEvent structures.  Not to be
confused with event masks above.  They start from 2 because 0 and 1
are reserved in the protocol for errors and replies. *)

  KeyPress =                    2;
  KeyRelease =                  3;
  ButtonPress =                 4;
  ButtonRelease =               5;
  MotionNotify =                6;
  EnterNotify =                 7;
  LeaveNotify =                 8;
  FocusIn =                     9;
  FocusOut =                    10;
  KeymapNotify =                11;
  Expose =                      12;
  GraphicsExpose =              13;
  NoExpose =                    14;
  VisibilityNotify =            15;
  CreateNotify =                16;
  DestroyNotify =               17;
  UnmapNotify =                 18;
  MapNotify =                   19;
  MapRequest =                  20;
  ReparentNotify =              21;
  ConfigureNotify =             22;
  ConfigureRequest =            23;
  GravityNotify =               24;
  ResizeRequest =               25;
  CirculateNotify =             26;
  CirculateRequest =            27;
  PropertyNotify =              28;
  SelectionClear =              29;
  SelectionRequest =            30;
  SelectionNotify =             31;
  ColormapNotify =              32;
  ClientMessage =               33;
  MappingNotify =               34;
  LASTEvent =                   35;     (* must be bigger than any event # *)


(* Key masks. Used as modifiers to GrabButton and GrabKey, results of QueryPointer,
   state in various key-, mouse-, and button-related events. *)

  ShiftMask =                   Word.Shift (1, 0);
  LockMask =                    Word.Shift (1, 1);
  ControlMask =                 Word.Shift (1, 2);
  Mod1Mask =                    Word.Shift (1, 3);
  Mod2Mask =                    Word.Shift (1, 4);
  Mod3Mask =                    Word.Shift (1, 5);
  Mod4Mask =                    Word.Shift (1, 6);
  Mod5Mask =                    Word.Shift (1, 7);

(* modifier names.  Used to build a SetModifierMapping request or
   to read a GetModifierMapping request.  These correspond to the
   masks defined above. *)

  ShiftMapIndex =               0;
  LockMapIndex =                1;
  ControlMapIndex =             2;
  Mod1MapIndex =                3;
  Mod2MapIndex =                4;
  Mod3MapIndex =                5;
  Mod4MapIndex =                6;
  Mod5MapIndex =                7;


(* button masks.  Used in same manner as Key masks above. Not to be confused
   with button names below. *)

  Button1Mask =                 Word.Shift (1, 8);
  Button2Mask =                 Word.Shift (1, 9);
  Button3Mask =                 Word.Shift (1, 10);
  Button4Mask =                 Word.Shift (1, 11);
  Button5Mask =                 Word.Shift (1, 12);

  AnyModifier =                 Word.Shift (1, 15)  (* used in GrabButton, GrabKey *);


(* button names. Used as arguments to GrabButton and as detail in ButtonPress
   and ButtonRelease events.  Not to be confused with button masks above.
   Note that 0 is already defined above as "AnyButton".  *)

  Button1 =                     1;
  Button2 =                     2;
  Button3 =                     3;
  Button4 =                     4;
  Button5 =                     5;

(* Notify modes *)

  NotifyNormal =                0;
  NotifyGrab =                  1;
  NotifyUngrab =                2;
  NotifyWhileGrabbed =          3;

  NotifyHint =                  1;      (* for MotionNotify events *)

(* Notify detail *)

  NotifyAncestor =              0;
  NotifyVirtual =               1;
  NotifyInferior =              2;
  NotifyNonlinear =             3;
  NotifyNonlinearVirtual =      4;
  NotifyPointer =               5;
  NotifyPointerRoot =           6;
  NotifyDetailNone =            7;

(* Visibility notify *)

  VisibilityUnobscured =        0;
  VisibilityPartiallyObscured = 1;
  VisibilityFullyObscured =     2;

(* Circulation request *)

  PlaceOnTop =                  0;
  PlaceOnBottom =               1;

(* protocol families *)

  FamilyInternet =              0;
  FamilyDECnet =                1;
  FamilyChaos =                 2;
  FamilyGeneric =               128;

(* Property notification *)

  PropertyNewValue =            0;
  PropertyDelete =              1;

(* Color Map notification *)

  ColormapUninstalled =         0;
  ColormapInstalled =           1;

(* GrabPointer, GrabButton, GrabKeyboard, GrabKey Modes *)

  GrabModeSync =                0;
  GrabModeAsync =               1;

(* GrabPointer, GrabKeyboard reply status *)

  GrabSuccess =                 0;
  AlreadyGrabbed =              1;
  GrabInvalidTime =             2;
  GrabNotViewable =             3;
  GrabFrozen =                  4;

(* AllowEvents modes *)

  AsyncPointer =                0;
  SyncPointer =                 1;
  ReplayPointer =               2;
  AsyncKeyboard =               3;
  SyncKeyboard =                4;
  ReplayKeyboard =              5;
  AsyncBoth =                   6;
  SyncBoth =                    7;

(* Used in SetInputFocus, GetInputFocus *)

  RevertToNone =                None;
  RevertToPointerRoot =         PointerRoot;
  RevertToParent =              2;

(*****************************************************************
 * ERROR CODES
 *****************************************************************)

  Success =                     0;      (* everything's okay *)
  BadRequest =                  1;      (* bad request code *)
  BadValue =                    2;      (* int parameter out of range *)
  BadWindow =                   3;      (* parameter not a Window *)
  BadPixmap =                   4;      (* parameter not a Pixmap *)
  BadAtom =                     5;      (* parameter not an Atom *)
  BadCursor =                   6;      (* parameter not a Cursor *)
  BadFont =                     7;      (* parameter not a Font *)
  BadMatch =                    8;      (* parameter mismatch *)
  BadDrawable =                 9;      (* parameter not a Pixmap or Window *)
  BadAccess =                   10;     (* depending on context:
                                         - key/button already grabbed
                                         - attempt to free an illegal
                                           cmap entry
                                        - attempt to store into a read-only
                                           color map entry.
                                        - attempt to modify the access control
                                           list from other than the local host.
                                        *)
  BadAlloc =                    11;     (* insufficient resources *)
  BadColor =                    12;     (* no such colormap *)
  BadGC =                       13;     (* parameter not a GC *)
  BadIDChoice =                 14;     (* choice not in range or already used *)
  BadName =                     15;     (* font or color name doesn't exist *)
  BadLength =                   16;     (* Request length incorrect *)
  BadImplementation =           17;     (* server is defective *)

  FirstExtensionError =         128;
  LastExtensionError =          255;

(*****************************************************************
 * WINDOW DEFINITIONS
 *****************************************************************)

(* Window classes used by CreateWindow *)
(* Note that CopyFromParent is already defined as 0 above *)

  InputOutput =                 1;
  InputOnly =                   2;

(* Window attributes for CreateWindow and ChangeWindowAttributes *)

  CWBackPixmap =                Word.Shift (1, 0);
  CWBackPixel =                 Word.Shift (1, 1);
  CWBorderPixmap =              Word.Shift (1, 2);
  CWBorderPixel =               Word.Shift (1, 3);
  CWBitGravity =                Word.Shift (1, 4);
  CWWinGravity =                Word.Shift (1, 5);
  CWBackingStore =              Word.Shift (1, 6);
  CWBackingPlanes =             Word.Shift (1, 7);
  CWBackingPixel =              Word.Shift (1, 8);
  CWOverrideRedirect =          Word.Shift (1, 9);
  CWSaveUnder =                 Word.Shift (1, 10);
  CWEventMask =                 Word.Shift (1, 11);
  CWDontPropagate =             Word.Shift (1, 12);
  CWColormap =                  Word.Shift (1, 13);
  CWCursor =                    Word.Shift (1, 14);

(* ConfigureWindow structure *)

  CWX =                         Word.Shift (1, 0);
  CWY =                         Word.Shift (1, 1);
  CWWidth =                     Word.Shift (1, 2);
  CWHeight =                    Word.Shift (1, 3);
  CWBorderWidth =               Word.Shift (1, 4);
  CWSibling =                   Word.Shift (1, 5);
  CWStackMode =                 Word.Shift (1, 6);


(* Bit Gravity *)

  ForgetGravity =               0;
  NorthWestGravity =            1;
  NorthGravity =                2;
  NorthEastGravity =            3;
  WestGravity =                 4;
  CenterGravity =               5;
  EastGravity =                 6;
  SouthWestGravity =            7;
  SouthGravity =                8;
  SouthEastGravity =            9;
  StaticGravity =               10;

(* Window gravity + bit gravity above *)

  UnmapGravity =                0;

(* Used in CreateWindow for backing-store hint *)

  NotUseful =                   0;
  WhenMapped =                  1;
  Always =                      2;

(* Used in GetWindowAttributes reply *)

  IsUnmapped =                  0;
  IsUnviewable =                1;
  IsViewable =                  2;

(* Used in ChangeSaveSet *)

  SetModeInsert =               0;
  SetModeDelete =               1;

(* Used in ChangeCloseDownMode *)

  DestroyAll =                  0;
  RetainPermanent =             1;
  RetainTemporary =             2;

(* Window stacking method (in configureWindow) *)

  Above =                       0;
  Below =                       1;
  TopIf =                       2;
  BottomIf =                    3;
  Opposite =                    4;

(* Circulation direction *)

  RaiseLowest =                 0;
  LowerHighest =                1;

(* Property modes *)

  PropModeReplace =             0;
  PropModePrepend =             1;
  PropModeAppend =              2;

(*****************************************************************
 * GRAPHICS DEFINITIONS
 *****************************************************************)

(* graphics functions, as in GC.alu *)

  GXclear =                     16_0;           (* 0 *)
  GXand =                       16_1;           (* src AND dst *)
  GXandReverse =                16_2;           (* src AND NOT dst *)
  GXcopy =                      16_3;           (* src *)
  GXandInverted =               16_4;           (* NOT src AND dst *)
  GXnoop =                      16_5;           (* dst *)
  GXxor =                       16_6;           (* src XOR dst *)
  GXor =                        16_7;           (* src OR dst *)
  GXnor =                       16_8;           (* NOT src AND NOT dst *)
  GXequiv =                     16_9;           (* NOT src XOR dst *)
  GXinvert =                    16_a;           (* NOT dst *)
  GXorReverse =                 16_b;           (* src OR NOT dst *)
  GXcopyInverted =              16_c;           (* NOT src *)
  GXorInverted =                16_d;           (* NOT src OR dst *)
  GXnand =                      16_e;           (* NOT src OR NOT dst *)
  GXset =                       16_f;           (* 1 *)

(* LineStyle *)

  LineSolid =                   0;
  LineOnOffDash =               1;
  LineDoubleDash =              2;

(* capStyle *)

  CapNotLast =                  0;
  CapButt =                     1;
  CapRound =                    2;
  CapProjecting =               3;

(* joinStyle *)

  JoinMiter =                   0;
  JoinRound =                   1;
  JoinBevel =                   2;

(* fillStyle *)

  FillSolid =                   0;
  FillTiled =                   1;
  FillStippled =                2;
  FillOpaqueStippled =          3;

(* fillRule *)

  EvenOddRule =                 0;
  WindingRule =                 1;

(* subwindow mode *)

  ClipByChildren =              0;
  IncludeInferiors =            1;

(* SetClipRectangles ordering *)

  Unsorted =                    0;
  YSorted =                     1;
  YXSorted =                    2;
  YXBanded =                    3;

(* CoordinateMode for drawing routines *)

  CoordModeOrigin =             0;      (* relative to the origin *)
  CoordModePrevious =           1;      (* relative to previous point *)

(* Polygon shapes *)

  Complex =                     0;      (* paths may intersect *)
  Nonconvex =                   1;      (* no paths intersect, but not convex *)
  Convex =                      2;      (* wholly convex *)

(* Arc modes for PolyFillArc *)

  ArcChord =                    0;      (* join endpoints of arc *)
  ArcPieSlice =                 1;      (* join endpoints to center of arc *)

(* GC components: masks used in CreateGC, CopyGC, ChangeGC, OR'ed into
   GC.stateChanges *)

  GCFunction =                  Word.Shift (1, 0);
  GCPlaneMask =                 Word.Shift (1, 1);
  GCForeground =                Word.Shift (1, 2);
  GCBackground =                Word.Shift (1, 3);
  GCLineWidth =                 Word.Shift (1, 4);
  GCLineStyle =                 Word.Shift (1, 5);
  GCCapStyle =                  Word.Shift (1, 6);
  GCJoinStyle =                 Word.Shift (1, 7);
  GCFillStyle =                 Word.Shift (1, 8);
  GCFillRule =                  Word.Shift (1, 9);
  GCTile =                      Word.Shift (1, 10);
  GCStipple =                   Word.Shift (1, 11);
  GCTileStipXOrigin =           Word.Shift (1, 12);
  GCTileStipYOrigin =           Word.Shift (1, 13);
  GCFont =                      Word.Shift (1, 14);
  GCSubwindowMode =             Word.Shift (1, 15);
  GCGraphicsExposures =         Word.Shift (1, 16);
  GCClipXOrigin =               Word.Shift (1, 17);
  GCClipYOrigin =               Word.Shift (1, 18);
  GCClipMask =                  Word.Shift (1, 19);
  GCDashOffset =                Word.Shift (1, 20);
  GCDashList =                  Word.Shift (1, 21);
  GCArcMode =                   Word.Shift (1, 22);

  GCLastBit =                   22;

(*****************************************************************
 * FONTS
 *****************************************************************)

(* used in QueryFont -- draw direction *)

  FontLeftToRight =             0;
  FontRightToLeft =             1;

  FontChange =                  255;

(*****************************************************************
 *  IMAGING
 *****************************************************************)

(* ImageFormat -- PutImage, GetImage *)

  XYBitmap =                    0;      (* depth 1, XYFormat *)
  XYPixmap =                    1;      (* depth == drawable depth *)
  ZPixmap =                     2;      (* depth == drawable depth *)

(*****************************************************************
 *  COLOR MAP STUFF
 *****************************************************************)

(* For CreateColormap *)

  AllocNone =                   0;      (* create map with no entries *)
  AllocAll =                    1;      (* allocate entire map writeable *)


(* Flags used in StoreNamedColor, StoreColors *)

  DoRed =                       Word.Shift (1, 0);
  DoGreen =                     Word.Shift (1, 1);
  DoBlue =                      Word.Shift (1, 2);

(*****************************************************************
 * CURSOR STUFF
 *****************************************************************)

(* QueryBestSize Class *)

  CursorShape =                 0;      (* largest size that can be displayed *)
  TileShape =                   1;      (* size tiled fastest *)
  StippleShape =                2;      (* size stippled fastest *)

(*****************************************************************
 * KEYBOARD/POINTER STUFF
 *****************************************************************)

  AutoRepeatModeOff =           0;
  AutoRepeatModeOn =            1;
  AutoRepeatModeDefault =       2;

  LedModeOff =                  0;
  LedModeOn =                   1;

(* masks for ChangeKeyboardControl *)

  KBKeyClickPercent =           Word.Shift (1, 0);
  KBBellPercent =               Word.Shift (1, 1);
  KBBellPitch =                 Word.Shift (1, 2);
  KBBellDuration =              Word.Shift (1, 3);
  KBLed =                       Word.Shift (1, 4);
  KBLedMode =                   Word.Shift (1, 5);
  KBKey =                       Word.Shift (1, 6);
  KBAutoRepeatMode =            Word.Shift (1, 7);

  MappingSuccess =              0;
  MappingBusy =                 1;
  MappingFailed =               2;

  MappingModifier =             0;
  MappingKeyboard =             1;
  MappingPointer =              2;

(*****************************************************************
 * SCREEN SAVER STUFF
 *****************************************************************)

  DontPreferBlanking =          0;
  PreferBlanking =              1;
  DefaultBlanking =             2;

  DisableScreenSaver =          0;
  DisableScreenInterval =       0;

  DontAllowExposures =          0;
  AllowExposures =              1;
  DefaultExposures =            2;

(* for ForceScreenSaver *)

  ScreenSaverReset =            0;
  ScreenSaverActive =           1;

(*****************************************************************
 * HOSTS AND CONNECTIONS
 *****************************************************************)

(* for ChangeHosts *)

  HostInsert =                  0;
  HostDelete =                  1;

(* for ChangeAccessControl *)

  EnableAccess =                1;
  DisableAccess =               0;

(* Display classes  used in opening the connection
 * Note that the statically allocated ones are even numbered and the
 * dynamically changeable ones are odd numbered *)

  StaticGray =                  0;
  GrayScale =                   1;
  StaticColor =                 2;
  PseudoColor =                 3;
  TrueColor =                   4;
  DirectColor =                 5;


(* Byte order  used in imageByteOrder and bitmapBitOrder *)

  LSBFirst =                    0;
  MSBFirst =                    1;

(* For users of XMultiplexInput.  This routine is VMS only *)

  XMINoBlock =                  1;
  XMINewInput =                 2;


(***************************************************************

                /usr/include/X11/Xlib.h

 ***************************************************************)

(* $XConsortium: Xlib.h,v 11.179 89/12/12 13:57:19 jim Exp $ *)
(*
 * Copyright 1985, 1986, 1987 by the Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising
 * or publicity pertaining to distribution of the software without specific,
 * written prior permission. M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * The X Window System is a Trademark of MIT.
 *
 *)


(*
 *      Xlib.h - Header definition and support file for the C subroutine
 *      interface library (Xlib) to the X Window System Protocol (V11).
 *      Structures and symbols starting with "_" are private to the library.
 *)

TYPE
  Bool      = Int;
  BoolStar  = UNTRACED REF Bool;
  Status    = Int;

CONST
  True      = 1;
  False     = 0;

CONST
  QueuedAlready        = 0;
  QueuedAfterReading   = 1;
  QueuedAfterFlush     = 2;


(* ?!?!?!?!
#define ConnectionNumber(dpy)   ((dpy)->fd)
#define RootWindow(dpy, scr)    (((dpy)->screens[(scr)]).root)
#define DefaultScreen(dpy)      ((dpy)->default_screen)
#define DefaultRootWindow(dpy)  (((dpy)->screens[(dpy)->default_screen]).root)
#define DefaultVisual(dpy, scr) (((dpy)->screens[(scr)]).root_visual)
#define DefaultGC(dpy, scr)     (((dpy)->screens[(scr)]).default_gc)
#define BlackPixel(dpy, scr)    (((dpy)->screens[(scr)]).black_pixel)
#define WhitePixel(dpy, scr)    (((dpy)->screens[(scr)]).white_pixel)
#define AllPlanes               (~0)
#define QLength(dpy)            ((dpy)->qlen)
#define DisplayWidth(dpy, scr)  (((dpy)->screens[(scr)]).width)
#define DisplayHeight(dpy, scr) (((dpy)->screens[(scr)]).height)
#define DisplayWidthMM(dpy, scr)(((dpy)->screens[(scr)]).mwidth)
#define DisplayHeightMM(dpy, scr)(((dpy)->screens[(scr)]).mheight)
#define DisplayPlanes(dpy, scr) (((dpy)->screens[(scr)]).root_depth)
#define DisplayCells(dpy, scr)  (DefaultVisual((dpy), (scr))->map_entries)
#define ScreenCount(dpy)        ((dpy)->nscreens)
#define ServerVendor(dpy)       ((dpy)->vendor)
#define ProtocolVersion(dpy)    ((dpy)->proto_major_version)
#define ProtocolRevision(dpy)   ((dpy)->proto_minor_version)
#define VendorRelease(dpy)      ((dpy)->release)
#define DisplayString(dpy)      ((dpy)->display_name)
#define DefaultDepth(dpy, scr)  (((dpy)->screens[(scr)]).root_depth)
#define DefaultColormap(dpy, scr)(((dpy)->screens[(scr)]).cmap)
#define BitmapUnit(dpy)         ((dpy)->bitmap_unit)
#define BitmapBitOrder(dpy)     ((dpy)->bitmap_bit_order)
#define BitmapPad(dpy)          ((dpy)->bitmap_pad)
#define ImageByteOrder(dpy)     ((dpy)->byte_order)
#define NextRequest(dpy)        ((dpy)->request + 1)
#define LastKnownRequestProcessed(dpy)  ((dpy)->last_request_read)

(* macros for screen oriented applications (toolkit) *)
#define ScreenOfDisplay(dpy, scr)(&((dpy)->screens[(scr)]))
#define DefaultScreenOfDisplay(dpy) (&((dpy)->screens[(dpy)->default_screen]))
#define DisplayOfScreen(s)      ((s)->display)
#define RootWindowOfScreen(s)   ((s)->root)
#define BlackPixelOfScreen(s)   ((s)->black_pixel)
#define WhitePixelOfScreen(s)   ((s)->white_pixel)
#define DefaultColormapOfScreen(s)((s)->cmap)
#define DefaultDepthOfScreen(s) ((s)->root_depth)
#define DefaultGCOfScreen(s)    ((s)->default_gc)
#define DefaultVisualOfScreen(s)((s)->root_visual)
#define WidthOfScreen(s)        ((s)->width)
#define HeightOfScreen(s)       ((s)->height)
#define WidthMMOfScreen(s)      ((s)->mwidth)
#define HeightMMOfScreen(s)     ((s)->mheight)
#define PlanesOfScreen(s)       ((s)->root_depth)
#define CellsOfScreen(s)        (DefaultVisualOfScreen((s))->map_entries)
#define MinCmapsOfScreen(s)     ((s)->min_maps)
#define MaxCmapsOfScreen(s)     ((s)->max_maps)
#define DoesSaveUnders(s)       ((s)->save_unders)
#define DoesBackingStore(s)     ((s)->backing_store)
#define EventMaskOfScreen(s)    ((s)->root_input_mask)
?!?!?! *)

(*
 * Extensions need a way to hang private data on some structures.
 *)

TYPE
  XExtData = RECORD
        number: Int;                (* number returned by XRegisterExtension *)
        next: XExtDataStar;         (* next item on list of data for structure *)
        free_private: RetIntProc;   (* called to free private storage *)
        private_data: char_star;    (* data private to this extension. *)
  END;
  XExtDataStar     =  UNTRACED REF XExtData;
  XExtDataStarStar =  UNTRACED REF XExtDataStar;

(*
 * This file contains structures used by the extension mechanism.
 *)

TYPE
  XExtCodes = RECORD
        extension: Int;           (* extension number *)
        major_opcode: Int;        (* major op-code assigned by server *)
        first_event: Int;         (* first event number for the extension *)
        first_error: Int;         (* first error number for the extension *)
  END;
  XExtCodesStar  =  UNTRACED REF XExtCodes;

(*
 * This structure is private to the library.
 *)

TYPE
  XExtension = RECORD             (* private to extension mechanism *)
        next: XExtensionStar;     (* next in list *)
        codes: XExtCodes;         (* public information, all extension told *)
        create_GC: RetIntProc;    (* routine to call when GC created *)
        copy_GC: RetIntProc;      (* routine to call when GC copied *)
        flush_GC: RetIntProc;     (* routine to call when GC flushed *)
        free_GC: RetIntProc;      (* routine to call when GC freed *)
        create_Font: RetIntProc;  (* routine to call when Font created *)
        free_Font: RetIntProc;    (* routine to call when Font freed *)
        close_display: RetIntProc;(* routine to call when connection closed *)
        error: RetIntProc;        (* who to call when an error occurs *)
        error_string: Retchar_starProc;  (* routine to supply error string *)
        name: char_star;           (* name of this extension *)
  END;
  XExtensionStar = UNTRACED REF XExtension;

(*
 * Data structure for retrieving info about pixmap formats.
 *)

TYPE
  XPixmapFormatValues = RECORD
    depth: Int;
    bits_per_pixel: Int;
    scanline_pad: Int;
  END;
  XPixmapFormatValuesStar  =  UNTRACED REF XPixmapFormatValues;


(*
 * Data structure for setting graphics context.
 *)
TYPE
  XGCValues = RECORD
        function: Int;           (* logical operation *)
        plane_mask: unsigned_long;(* plane mask *)
        foreground: unsigned_long;(* foreground pixel *)
        background: unsigned_long;(* background pixel *)
        line_width: Int;         (* line width *)
        line_style: Int;         (* LineSolid, LineOnOffDash, LineDoubleDash *)
        cap_style: Int;          (* CapNotLast, CapButt,
                                   CapRound, CapProjecting *)
        join_style: Int;         (* JoinMiter, JoinRound, JoinBevel *)
        fill_style: Int;         (* FillSolid, FillTiled,
                                   FillStippled, FillOpaeueStippled *)
        fill_rule: Int;          (* EvenOddRule, WindingRule *)
        arc_mode: Int;           (* ArcChord, ArcPieSlice *)
        tile: Pixmap;            (* tile pixmap for tiling operations *)
        stipple: Pixmap;         (* stipple 1 plane pixmap for stipping *)
        ts_x_origin: Int;        (* offset for tile or stipple operations *)
        ts_y_origin: Int;
        font: Font;              (* default text font for text operations *)
        subwindow_mode: Int;     (* ClipByChildren, IncludeInferiors *)
        graphics_exposures: Bool;(* boolean, should exposures be generated *)
        clip_x_origin: Int;      (* origin for clipping *)
        clip_y_origin: Int;
        clip_mask: Pixmap;       (* bitmap clipping; other calls for rects *)
        dash_offset: Int;        (* patterned/dashed line information *)
        dashes: Char;
  END;

TYPE
  XGCValuesStar  =  UNTRACED REF XGCValues;

(*
 * Graphics context.  All Xlib routines deal in this rather than
 * in raw protocol GContext ID's.  This is so that the library can keep
 * a "shadow" set of values, and thus avoid passing values over the
 * wire which are not in fact changing.
 *
 * Graphics context.  The contents of this structure are implementation
 * dependent.  A GC should be treated as opaque by application code.
 *)
TYPE
  XPrivateGC = RECORD
    ext_data: XExtDataStar;      (* hook for extension to hang data *)
    gid: GContext;               (* protocol ID for graphics context *)

    (* The rest is private and not in current headers. *)

    private_rects: Bool;          (* boolean: TRUE if clipmask is list of rectangles *)
    private_dashes: Bool;             (* boolean: TRUE if dash-list is really a list *)
    private_dirty: unsigned_long;         (* cache dirty bits *)
    private_values: XGCValues;           (* shadow structure of values *)
  END;
  GC = UNTRACED BRANDED REF ADDRESS;

(*
 * Visual structure; contains information about colormapping possible.
 *)

TYPE
  Visual = RECORD
        ext_data: XExtDataStar;  (* hook for extension to hang data *)
        visualid: VisualID;      (* visual id of this visual *)
        class: Int;              (* class of screen (monochrome, etc.) *)
        red_mask, green_mask, blue_mask: unsigned_long;  (* mask values *)
        bits_per_rgb: Int;       (* log base 2 of distinct color values *)
        map_entries: Int;        (* color map entries *)
  END;
  VisualStar   =  UNTRACED REF Visual;


(*
 * Depth structure; contains information for each possible depth.
 *)

TYPE
  Depth = RECORD
        depth: Int;              (* this depth (Z) of the depth *)
        nvisuals: Int;           (* number of Visual types at this depth *)
        visuals: VisualStar;     (* list of visuals possible at this depth *)
  END;
  DepthStar    =  UNTRACED REF Depth;


(*
 * Information about the screen.
 *)

TYPE Screen = RECORD
        ext_data: XExtDataStar;  (* hook for extension to hang data *)
        display: DisplayStar;    (* back pointer to display structure *)
        root: Window;            (* Root window id. *)
        width, height: Int;      (* width and height of screen *)
        mwidth, mheight: Int;    (* width and height of  in millimeters *)
        ndepths: Int;            (* number of depths possible *)
        depths: DepthStar;       (* list of allowable depths on the screen *)
        root_depth: Int;         (* bits per pixel *)
        root_visual: VisualStar; (* root visual *)
        default_gc: GC;          (* GC for the root root visual *)
        cmap: Colormap;          (* default color map *)
        white_pixel: unsigned_long;
        black_pixel: unsigned_long; (* White and Black pixel values *)
        max_maps, min_maps: Int; (* max and min color maps *)
        backing_store: Int;      (* Never, WhenMapped, Always *)
        save_unders: Bool;
        root_input_mask: Long;   (* initial root input mask *)
  END;
  ScreenStar    =  UNTRACED REF Screen;

(*
 * Format structure; describes ZFormat data the screen will understand.
 *)

TYPE
  ScreenFormat = RECORD
        ext_data: XExtDataStar;  (* hook for extension to hang data *)
        depth: Int;              (* depth of this image format *)
        bits_per_pixel: Int;     (* bits/pixel at this depth *)
        scanline_pad: Int;       (* scanline must padded to this multiple *)
  END;
  ScreenFormatStar     =  UNTRACED REF ScreenFormat;


(*
 * Data structure for setting window attributes.
 *)

TYPE
  XSetWindowAttributes = RECORD
    background_pixmap: Pixmap;   (* background or None or ParentRelative *)
    background_pixel: unsigned_long;     (* background pixel *)
    border_pixmap: Pixmap;       (* border of the window *)
    border_pixel: unsigned_long;  (* border pixel value *)
    bit_gravity: Int;            (* one of bit gravity values *)
    win_gravity: Int;            (* one of the window gravity values *)
    backing_store: Int;          (* NotUseful, WhenMapped, Always *)
    backing_planes: unsigned_long;(* planes to be preseved if possible *)
    backing_pixel: unsigned_long; (* value to use in restoring planes *)
    save_under: Bool;            (* should bits under be saved? (popups) *)
    event_mask: Long;            (* set of events that should be saved *)
    do_not_propagate_mask: Long; (* set of events that should not propagate *)
    override_redirect: Bool;     (* boolean value for override-redirect *)
    colormap: Colormap;          (* color map to be associated with window *)
    cursor: Cursor;              (* cursor to be displayed (or None) *)
  END;

  XWindowAttributes = RECORD
    x, y: Int;                   (* location of window *)
    width, height: Int;          (* width and height of window *)
    border_width: Int;           (* border width of window *)
    depth: Int;                  (* depth of window *)
    visual: VisualStar;          (* the associated visual structure *)
    root: Window;                (* root of screen containing window *)
    class: Int;                  (* InputOutput, InputOnly*)
    bit_gravity: Int;            (* one of bit gravity values *)
    win_gravity: Int;            (* one of the window gravity values *)
    backing_store: Int;          (* NotUseful, WhenMapped, Always *)
    backing_planes: unsigned_long;(* planes to be preserved if possible *)
    backing_pixel: unsigned_long; (* value to be used when restoring planes *)
    save_under: Bool;            (* boolean, should bits under be saved? *)
    colormap: Colormap;          (* color map to be associated with window *)
    map_installed: Bool;         (* boolean, is color map currently installed*)
    map_state: Int;              (* IsUnmapped, IsUnviewable, IsViewable *)
    all_event_masks: Long;       (* set of events all people have interest in*)
    your_event_mask: Long;       (* my event mask *)
    do_not_propagate_mask: Long; (* set of events that should not propagate *)
    override_redirect: Bool;     (* boolean value for override-redirect *)
    screen: ScreenStar;          (* back pointer to correct screen *)
  END;

TYPE
  XSetWindowAttributesStar    =  UNTRACED REF XSetWindowAttributes;
  XWindowAttributesStar       =  UNTRACED REF XWindowAttributes;

(*
 * Data structure for host setting; getting routines.
 *
 *)

TYPE
  XHostAddress = RECORD
        family: Int;             (* for example AF_DNET *)
        length: Int;             (* length of address, in bytes *)
        address: char_star;       (* pointer to where to find the bytes *)
  END;
  XHostAddressStar   =  UNTRACED REF XHostAddress;

(*
 * Data structure for "image" data, used by image manipulation routines.
 *)

TYPE
  CreateImageProc = PROCEDURE (): XImageStar;
  DestroyImageProc = PROCEDURE (i: XImageStar): Int;
  GetPixelProc = PROCEDURE (i: XImageStar; x, y: Int): unsigned_long;
  PutPixelProc = PROCEDURE (i: XImageStar; x, y: Int; p: unsigned_long): Int;
  SubImageProc = PROCEDURE (i: XImageStar;
                            x, y: Int; w, h: unsigned_int): XImageStar;
  AddPixelProc = PROCEDURE(i: XImageStar): Int;

  XImage = RECORD
    width, height: Int;          (* size of image *)
    xoffset: Int;                (* number of pixels offset in X direction *)
    format: Int;                 (* XYBitmap, XYPixmap, ZPixmap *)
    data: char_star;              (* pointer to image data *)
    byte_order: Int;             (* data byte order, LSBFirst, MSBFirst *)
    bitmap_unit: Int;            (* quant. of scanline 8, 16, 32 *)
    bitmap_bit_order: Int;       (* LSBFirst, MSBFirst *)
    bitmap_pad: Int;             (* 8, 16, 32 either XY or ZPixmap *)
    depth: Int;                  (* depth of image *)
    bytes_per_line: Int;         (* accelarator to next line *)
    bits_per_pixel: Int;         (* bits per pixel (ZPixmap) *)
    red_mask: unsigned_long;      (* bits in z arrangment *)
    green_mask: unsigned_long;
    blue_mask: unsigned_long;
    obdata: char_star;            (* hook for the object routines to hang on *)
    f: RECORD                    (* image manipulation routines *)
        create_image: CreateImageProc;
        destroy_image: DestroyImageProc;
        get_pixel: GetPixelProc;
        put_pixel: PutPixelProc;
        sub_image: SubImageProc;
        add_pixel: AddPixelProc;
    END;
  END;
  XImageStar  =  UNTRACED REF XImage;

(*
 * Data structure for XReconfigureWindow
 *)

TYPE
  XWindowChanges = RECORD
    x, y: Int;
    width, height: Int;
    border_width: Int;
    sibling: Window;
    stack_mode: Int;
  END;
  XWindowChangesStar  =  UNTRACED REF XWindowChanges;

(*
 * Data structure used by color operations
 *)

TYPE
  XColor = RECORD
    pixel: unsigned_long;
    red, green, blue: unsigned_short;
    flags: Char;               (* do_red, do_green, do_blue *)
    pad: Char;
  END;
  XColorStar = UNTRACED REF XColor;

(*
 * Data structures for graphics operations.  On most machines, these are
 * congruent with the wire protocol structures, so reformatting the data
 * can be avoided on these architectures.
 *)

TYPE

  XSegment = RECORD
    x1, y1, x2, y2: Short;
  END;
  XSegmentStar =  UNTRACED REF XSegment;

  XPoint = RECORD
    x, y: Short;
  END;
  XPoint_star = UNTRACED REF XPoint;

  XRectangle = RECORD
    x, y: Short;
    width, height: unsigned_short;
  END;
  XRectangleStar = UNTRACED REF XRectangle;

  XArc = RECORD
    x, y: Short;
    width, height: unsigned_short;
    angle1, angle2: Short;
  END;
  XArcStar = UNTRACED REF XArc;


(* Data structure for XChangeKeyboardControl *)

TYPE
  XKeyboardControl = RECORD
        key_click_percent: Int;
        bell_percent: Int;
        bell_pitch: Int;
        bell_duration: Int;
        led: Int;
        led_mode: Int;
        key: Int;
        auto_repeat_mode: Int;   (* On, Off, Default *)
  END;
  XKeyboardControlStar = UNTRACED REF XKeyboardControl;

(* Data structure for XGetKeyboardControl *)

TYPE
  XKeyboardState = RECORD
        key_click_percent: Int;
        bell_percent: Int;
        bell_pitch, bell_duration: unsigned_int;
        led_mask: unsigned_long;
        global_auto_repeat: Int;
        auto_repeats: ARRAY [0 .. 31] OF Char;
  END;
  XKeyboardStateStar  =  UNTRACED REF XKeyboardState;

(* Data structure for XGetMotionEvents.  *)

TYPE
  XTimeCoord = RECORD
        time: Time;
        x, y: Short;
  END;
  XTimeCoordStar  = UNTRACED REF XTimeCoord;

(* Data structure for X{Set,Get}ModifierMapping *)

TYPE
  XModifierKeymap = RECORD
        max_keypermod: Int;      (* The server's max # of keys per modifier *)
        modifiermap: KeyCodeStar;(* An 8 by max_keypermod array of modifiers *)
  END;
  XModifierKeymapStar  =  UNTRACED REF XModifierKeymap;

(*
 * internal atoms used for ICCCM things; not to be used by client
 *)

TYPE
  DisplayAtoms = RECORD
    text: Atom;
    wm_state: Atom;
    wm_protocols: Atom;
    wm_save_yourself: Atom;
    wm_change_state: Atom;
    wm_colormap_windows: Atom;
    (* add new atoms to end of list *)
  END;
  DisplayAtomsStar = UNTRACED REF DisplayAtoms;

(*
 * Display datatype maintaining display specific data.
 *
 * The contents of this structure are implementation dependent.
 * A Display should be treated as opaque by application code.
 *)

TYPE
  XPrivateDisplay = RECORD
        ext_data: XExtDataStar;  (* hook for extension to hang data *)
        private1: DisplayStar;
        fd: Int;                 (* Network socket. *)
        private2: Int;
        proto_major_version: Int;(* maj. version of server's X protocol *)
        proto_minor_version: Int;(* minor version of servers X protocol *)
        vendor: char_star;        (* vendor of the server hardware *)
        private3: Long;
        private4: Long;
        private5: Long;
        private6: Int;
        resource_alloc: PROCEDURE(): XID; (* allocator function *)
        byte_order: Int;         (* screen byte order, LSBFirst, MSBFirst *)
        bitmap_unit: Int;        (* padding and data requirements *)
        bitmap_pad: Int;         (* padding requirements on bitmaps *)
        bitmap_bit_order: Int;   (* LeastSignificant or MostSignificant *)
        nformats: Int;           (* number of pixmap formats in list *)
        pixmap_format: ScreenFormatStar;    (* pixmap format list *)
        private8: Int;
        release: Int;            (* release of the server *)
        private9, private10: XQEventStar;
        qlen: Int;               (* Length of input event queue *)
        last_request_read: unsigned_long; (* seq number of last event read *)
        request: unsigned_long;   (* sequence number of last request. *)
        private11: char_star;
        private12: char_star;
        private13: char_star;
        private14: char_star;
        max_request_size: unsigned_int; (* max number 32 bit words in request*)
        db: ADDRESS;             (*?!? wrong ?!?*)
        private15: XSynchronize;
        display_name: char_star; (* "host:display" string used on this connect*)
        default_screen: Int;     (* default screen for operations *)
        nscreens: Int;           (* number of screens on this server*)
        screens: ScreenStar;     (* pointer to list of screens *)
        motion_buffer: unsigned_long;    (* size of motion buffer *)
        private16: Window;
        min_keycode: Int;        (* minimum defined keycode *)
        max_keycode: Int;        (* maximum defined keycode *)
        private17: KeySymStar;
        private18: XModifierKeymapStar;
        private19: Int;
        xdefaults: char_star;     (* contents of defaults from server *)

        (* the rest is all private *)

        private_scratch_buffer: char_star;(* place to hang scratch buffer *)
        private_scratch_length: unsigned_long;   (* length of scratch buffer *)
        private_ext_number: Int;         (* extension number on this display *)
        private_ext_procs: XExtensionStar; (* extensions initialized on this display *)
        (*
         * the following can be fixed size, as the protocol defines how
         * much address space is available.
         * While this could be done using the extension vector, there
         * may be MANY events processed, so a search through the extension
         * list to find the right procedure for each event might be
         * expensive if many extensions are being used.
         *)
        private_event_vec: ARRAY [0 .. 127] OF PROCEDURE(): Bool;
        private_wire_vec: ARRAY [0 .. 127] OF PROCEDURE(): Status;
        private_lock_meaning: KeySym;       (* for XLookupString *)
        private_key_bindings: ADDRESS;      (* for XLookupString *) (*?!? wrong ?!?*)
        private_cursor_font: Font;          (* for XCreateFontCursor *)
        (*
         * ICCCM information, version 1
         *)
        private_atoms: DisplayAtomsStar;
        private_reconfigure_wm_window: RECORD (* for XReconfigureWMWindow *)
            private_sequence_number: Long;
            private_old_handler: PROCEDURE(): Int;
            private_succeeded: Bool;
        END;
        (*
         * additional connection info
         *)
        private_flags: unsigned_long;       (* internal connection flags *)
        private_mode_switch: unsigned_int;  (* keyboard group modifiers *)
  END;
  DisplayStar = UNTRACED BRANDED REF ADDRESS;


(*
 * A "XEvent" structure always  has type as the first entry.  This
 * uniquely identifies what  kind of event it is.  The second entry
 * is always a pointer to the display the event was read from.
 * The third entry is always a window of one type or another,
 * carefully selected to be useful to toolkit dispatchers.  (Except
 * for keymap events, which have no window.) You
 * must not change the order of the three elements or toolkits will
 * break! The pointer to the generic event must be cast before use to
 * access any other information in the structure.
 *)

(*
 * Definitions of specific events.
 *)
TYPE
  XKeyEvent = RECORD
     type: Int;             (* of event *)
     serial: unsigned_long; (* # of last request processed by server *)
     send_event: Bool;      (* true if this came from a SendEvent request *)
     display: DisplayStar;  (* Display the event was read from *)
     window: Window;        (* "event" window it is reported relative to *)
     root: Window;          (* root window that the event occured on *)
     subwindow: Window;     (* child window *)
     time: Time;            (* milliseconds *)
     x, y: Int;             (* pointer x, y coordinates in event window *)
     x_root, y_root: Int;   (* coordinates relative to root *)
     state: unsigned_int;   (* key or button mask *)
     keycode: unsigned_int; (* detail *)
     same_screen: Bool      (* same screen flag *)
  END;

  XKeyEventStar         = UNTRACED REF XKeyEvent;
  XKeyPressedEvent      = XKeyEvent;
  XKeyPressedEventStar  = UNTRACED REF XKeyPressedEvent;
  XKeyReleasedEvent     = XKeyEvent;
  XKeyReleasedEventStar = UNTRACED REF XKeyReleasedEvent;

TYPE
  XButtonEvent = RECORD
     type: Int;             (* of event *)
     serial: unsigned_long; (* # of last request processed by server *)
     send_event: Bool;      (* true if this came from a SendEvent request *)
     display: DisplayStar;  (* Display the event was read from *)
     window: Window;        (* "event" window it is reported relative to *)
     root: Window;          (* root window that the event occured on *)
     subwindow: Window;     (* child window *)
     time: Time;            (* milliseconds *)
     x, y: Int;             (* poInter x, y coordinates in event window *)
     x_root, y_root: Int;   (* coordinates relative to root *)
     state: unsigned_int;   (* key or button mask *)
     button: unsigned_int;  (* detail *)
     same_screen: Bool      (* same screen flag *)
  END;

  XButtonEventStar          = UNTRACED REF  XButtonEvent;
  XButtonPressedEvent       = XButtonEvent;
  XButtonPressedEventStar   = UNTRACED REF  XButtonPressedEvent;
  XButtonReleasedEvent      = XButtonEvent;
  XButtonReleasedEventStar  = UNTRACED REF  XButtonReleasedEvent;

TYPE
  XMotionEvent = RECORD
     type: Int;             (* of event *)
     serial: unsigned_long; (* # of last request processed by server *)
     send_event: Bool;      (* true if this came from a SendEvent request *)
     display: DisplayStar;  (* Display the event was read from *)
     window: Window;        (* "event" window reported relative to *)
     root: Window;          (* root window that the event occured on *)
     subwindow: Window;     (* child window *)
     time: Time;            (* milliseconds *)
     x, y: Int;             (* pointer x, y coordinates in event window *)
     x_root, y_root: Int;   (* coordinates relative to root *)
     state: unsigned_int;   (* key or button mask *)
     is_hint: Char;         (* detail *)
     same_screen: Bool      (* same screen flag *)
  END;

  XMotionEventStar          = UNTRACED REF  XMotionEvent;
  XPointerMovedEvent        = XMotionEvent;
  XPointerMovedEventStar    = UNTRACED REF  XPointerMovedEvent;

TYPE
  XCrossingEvent = RECORD
     type: Int;                (* of event *)
     serial: unsigned_long;     (* # of last request processed by server *)
     send_event: Bool;         (* true if this came from a SendEvent request *)
     display: DisplayStar;     (* Display the event was read from *)
     window: Window;           (* "event" window reported relative to *)
     root: Window;             (* root window that the event occured on *)
     subwindow: Window;        (* child window *)
     time: Time;               (* milliseconds *)
     x, y: Int;                (* pointer x, y coordinates in event window *)
     x_root, y_root: Int;      (* coordinates relative to root *)
     mode: Int;                (* NotifyNormal, NotifyGrab, NotifyUngrab *)
     detail: Int;
    (*
     * NotifyAncestor, NotifyVirtual, NotifyInferior,
     * NotifyNonLinear,NotifyNonLinearVirtual
     *)
     same_screen: Bool;        (* same screen flag *)
     focus: Bool;              (* boolean focus *)
     state: unsigned_int        (* key or button mask *)
              END;

  XCrossingEventStar        = UNTRACED REF   XCrossingEvent;
  XEnterWindowEvent         = XCrossingEvent;
  XEnterWindowEventStar     = UNTRACED REF XEnterWindowEvent;
  XLeaveWindowEvent         = XCrossingEvent;
  XLeaveWindowEventStar     = UNTRACED REF XLeaveWindowEvent;

TYPE
  XFocusChangeEvent = RECORD
     type: Int;                (* FocusIn or FocusOut *)
     serial: unsigned_long;     (* # of last request processed by server *)
     send_event: Bool;         (* true if this came from a SendEvent request *)
     display: DisplayStar;     (* Display the event was read from *)
     window: Window;           (* window of event *)
     mode: Int;                (* NotifyNormal, NotifyGrab, NotifyUngrab *)
     detail: Int
    (*
     * NotifyAncestor, NotifyVirtual, NotifyInferior,
     * NotifyNonLinear,NotifyNonLinearVirtual, NotifyPointer,
     * NotifyPointerRoot, NotifyDetailNone
     *)
  END;

  XFocusChangeEventStar     = UNTRACED REF   XFocusChangeEvent;
  XFocusInEvent             = XFocusChangeEvent;
  XFocusInEventStar         = UNTRACED REF XFocusInEvent;
  XFocusOutEvent            = XFocusChangeEvent;
  XFocusOutEventStar        = UNTRACED REF XFocusOutEvent;

(* generated on EnterWindow and FocusIn  when KeyMapState selected *)

TYPE
  XKeymapEvent = RECORD
     type: Int;
     serial: unsigned_long;     (* # of last request processed by server *)
     send_event: Bool;         (* true if this came from a SendEvent request *)
     display: DisplayStar;     (* Display the event was read from *)
     window: Window;
     key_vector: ARRAY [0..31] OF Char
              END;

  XKeymapEventStar  = UNTRACED REF   XKeymapEvent;

TYPE
  XExposeEvent = RECORD
     type: Int;
     serial: unsigned_long;     (* # of last request processed by server *)
     send_event: Bool;         (* true if this came from a SendEvent request *)
     display: DisplayStar;     (* Display the event was read from *)
     window: Window;
     x, y: Int;
     width, height: Int;
     count: Int                (* if non-zero, at least this many more *)
  END;

  XExposeEventStar      = UNTRACED REF   XExposeEvent;

TYPE
  XGraphicsExposeEvent = RECORD
     type: Int;
     serial: unsigned_long;     (* # of last request processed by server *)
     send_event: Bool;         (* true if this came from a SendEvent request *)
     display: DisplayStar;     (* Display the event was read from *)
     drawable: Drawable;
     x, y: Int;
     width, height: Int;
     count: Int;               (* if non-zero, at least this many more *)
     major_code: Int;          (* core is CopyArea or CopyPlane *)
     minor_code: Int           (* not defined in the core *)
              END;

  XGraphicsExposeEventStar  = UNTRACED REF   XGraphicsExposeEvent;

TYPE
  XNoExposeEvent = RECORD
     type: Int;
     serial: unsigned_long;     (* # of last request processed by server *)
     send_event: Bool;         (* true if this came from a SendEvent request *)
     display: DisplayStar;     (* Display the event was read from *)
     drawable: Drawable;
     major_code: Int;          (* core is CopyArea or CopyPlane *)
     minor_code: Int           (* not defined in the core *)
              END;

  XNoExposeEventStar    = UNTRACED REF   XNoExposeEvent;

TYPE
  XVisibilityEvent = RECORD
     type: Int;
     serial: unsigned_long;     (* # of last request processed by server *)
     send_event: Bool;         (* true if this came from a SendEvent request *)
     display: DisplayStar;     (* Display the event was read from *)
     window: Window;
     state: Int                (* Visibility state *)
  END;

  XVisibilityEventStar  = UNTRACED REF   XVisibilityEvent;

TYPE
  XCreateWindowEvent = RECORD
     type: Int;
     serial: unsigned_long;     (* # of last request processed by server *)
     send_event: Bool;         (* true if this came from a SendEvent request *)
     display: DisplayStar;     (* Display the event was read from *)
     parent: Window;           (* parent of the window *)
     window: Window;           (* window id of window created *)
     x, y: Int;                (* window location *)
     width, height: Int;       (* size of window *)
     border_width: Int;        (* border width *)
     override_redirect: Bool   (* creation should be overridden *)
  END;

  XCreateWindowEventStar    = UNTRACED REF   XCreateWindowEvent;

TYPE
  XDestroyWindowEvent = RECORD
     type: Int;
     serial: unsigned_long; (* # of last request processed by server *)
     send_event: Bool;      (* true if this came from a SendEvent request *)
     display: DisplayStar;  (* Display the event was read from *)
     event: Window;
     window: Window
  END;

  XDestroyWindowEventStar   = UNTRACED REF   XDestroyWindowEvent;

TYPE
  XUnmapEvent = RECORD
     type: Int;
     serial: unsigned_long;     (* # of last request processed by server *)
     send_event: Bool;         (* true if this came from a SendEvent request *)
     display: DisplayStar;     (* Display the event was read from *)
     event: Window;
     window: Window;
     from_configure: Bool
  END;

  XUnmapEventStar       = UNTRACED REF   XUnmapEvent;

TYPE
  XMapEvent = RECORD
     type: Int;
     serial: unsigned_long;     (* # of last request processed by server *)
     send_event: Bool;         (* true if this came from a SendEvent request *)
     display: DisplayStar;     (* Display the event was read from *)
     event: Window;
     window: Window;
     override_redirect: Bool (* boolean, is override set... *)
  END;

  XMapEventStar = UNTRACED REF   XMapEvent;

TYPE
  XMapRequestEvent = RECORD
     type: Int;
     serial: unsigned_long;     (* # of last request processed by server *)
     send_event: Bool;         (* true if this came from a SendEvent request *)
     display: DisplayStar;     (* Display the event was read from *)
     parent: Window;
     window: Window
              END;

  XMapRequestEventStar      = UNTRACED REF   XMapRequestEvent;

TYPE
  XReparentEvent = RECORD
     type: Int;
     serial: unsigned_long;     (* # of last request processed by server *)
     send_event: Bool;         (* true if this came from a SendEvent request *)
     display: DisplayStar;     (* Display the event was read from *)
     event: Window;
     window: Window;
     parent: Window;
     x, y: Int;
     override_redirect: Bool
  END;

  XReparentEventStar    = UNTRACED REF   XReparentEvent;

TYPE
  XConfigureEvent = RECORD
     type: Int;
     serial: unsigned_long;     (* # of last request processed by server *)
     send_event: Bool;         (* true if this came from a SendEvent request *)
     display: DisplayStar;     (* Display the event was read from *)
     event: Window;
     window: Window;
     x, y: Int;
     width, height: Int;
     border_width: Int;
     above: Window;
     override_redirect: Bool
              END;

  XConfigureEventStar       = UNTRACED REF   XConfigureEvent;

TYPE
  XGravityEvent = RECORD
     type: Int;
     serial: unsigned_long;     (* # of last request processed by server *)
     send_event: Bool;         (* true if this came from a SendEvent request *)
     display: DisplayStar;     (* Display the event was read from *)
     event: Window;
     window: Window;
     x, y: Int
  END;

  XGravityEventStar     = UNTRACED REF   XGravityEvent;

TYPE
  XResizeRequestEvent = RECORD
     type: Int;
     serial: unsigned_long;     (* # of last request processed by server *)
     send_event: Bool;         (* true if this came from a SendEvent request *)
     display: DisplayStar;     (* Display the event was read from *)
     window: Window;
     width, height: Int
              END;

  XResizeRequestEventStar   = UNTRACED REF   XResizeRequestEvent;

TYPE
  XConfigureRequestEvent = RECORD
     type: Int;
     serial: unsigned_long;     (* # of last request processed by server *)
     send_event: Bool;         (* true if this came from a SendEvent request *)
     display: DisplayStar;     (* Display the event was read from *)
     parent: Window;
     window: Window;
     x, y: Int;
     width, height: Int;
     border_width: Int;
     above: Window;
     detail: Int;              (* Above, Below, TopIf, BottomIf, Opposite *)
     value_mask: unsigned_long
  END;

  XConfigureRequestEventStar    = UNTRACED REF   XConfigureRequestEvent;

TYPE
  XCirculateEvent = RECORD
     type: Int;
     serial: unsigned_long; (* # of last request processed by server *)
     send_event: Bool;      (* true if this came from a SendEvent request *)
     display: DisplayStar;  (* Display the event was read from *)
     event: Window;
     window: Window;
     place: Int             (* PlaceOnTop, PlaceOnBottom *)
  END;

  XCirculateEventStar       = UNTRACED REF   XCirculateEvent;

TYPE
  XCirculateRequestEvent = RECORD
     type: Int;
     serial: unsigned_long;     (* # of last request processed by server *)
     send_event: Bool;         (* true if this came from a SendEvent request *)
     display: DisplayStar;     (* Display the event was read from *)
     parent: Window;
     window: Window;
     place: Int                (* PlaceOnTop, PlaceOnBottom *)
  END;

  XCirculateRequestEventStar    = UNTRACED REF   XCirculateRequestEvent;

TYPE
  XPropertyEvent = RECORD
     type: Int;
     serial: unsigned_long; (* # of last request processed by server *)
     send_event: Bool;      (* true if this came from a SendEvent request *)
     display: DisplayStar;  (* Display the event was read from *)
     window: Window;
     atom: Atom;
     time: Time;
     state: Int             (* NewValue, Deleted *)
  END;

  XPropertyEventStar        = UNTRACED REF   XPropertyEvent;

TYPE
  XSelectionClearEvent = RECORD
     type: Int;
     serial: unsigned_long; (* # of last request processed by server *)
     send_event: Bool;      (* true if this came from a SendEvent request *)
     display: DisplayStar;  (* Display the event was read from *)
     window: Window;
     selection: Atom;
     time: Time
  END;

  XSelectionClearEventStar  = UNTRACED REF   XSelectionClearEvent;

TYPE
  XSelectionRequestEvent = RECORD
     type: Int;
     serial: unsigned_long;     (* # of last request processed by server *)
     send_event: Bool;         (* true if this came from a SendEvent request *)
     display: DisplayStar;     (* Display the event was read from *)
     owner: Window;
     requestor: Window;
     selection: Atom;
     target: Atom;
     property: Atom;
     time: Time
  END;

  XSelectionRequestEventStar = UNTRACED REF XSelectionRequestEvent;

TYPE
  XSelectionEvent = RECORD
     type: Int;
     serial: unsigned_long; (* # of last request processed by server *)
     send_event: Bool;      (* true if this came from a SendEvent request *)
     display: DisplayStar;  (* Display the event was read from *)
     requestor: Window;
     selection: Atom;
     target: Atom;
     property: Atom;        (* ATOM or None *)
     time: Time
  END;

  XSelectionEventStar		= UNTRACED REF   XSelectionEvent;

TYPE
  XColormapEvent = RECORD
     type: Int;
     serial: unsigned_long;     (* # of last request processed by server *)
     send_event: Bool;         (* true if this came from a SendEvent request *)
     display: DisplayStar;     (* Display the event was read from *)
     window: Window;
     colormap: Colormap;       (* COLORMAP or None *)
     new: Bool;
     state: Int                (* ColormapInstalled, ColormapUninstalled *)
              END;

  XColormapEventStar		= UNTRACED REF   XColormapEvent;

TYPE
  XClientMessageEvent = RECORD
     type: int;
     serial: unsigned_long;    (* # of last request processed by server *)
     send_event: Bool;         (* true if this came from a SendEvent request *)
     display: DisplayStar;     (* Display the event was read from *)
     window: Window;
     message_type: Atom;
     format: int;
     alignment: ARRAY [0..-1] OF long;
     data: ARRAY [0..MAX (20 * BYTESIZE (char),
                          MAX (10 * BYTESIZE (short),
                               5 * BYTESIZE (long))) - 1] OF char; END;

  XClientMessageEvent_b = RECORD
     type: int;
     serial: unsigned_long;    (* # of last request processed by server *)
     send_event: Bool;         (* true if this came from a SendEvent request *)
     display: DisplayStar;     (* Display the event was read from *)
     window: Window;
     message_type: Atom;
     format: int;
     alignment: ARRAY [0..-1] OF long;
     data: ARRAY [0.. 19] OF char; END;

  XClientMessageEvent_s = RECORD
     type: int;
     serial: unsigned_long;    (* # of last request processed by server *)
     send_event: Bool;         (* true if this came from a SendEvent request *)
     display: DisplayStar;     (* Display the event was read from *)
     window: Window;
     message_type: Atom;
     format: int;
     alignment: ARRAY [0..-1] OF long;
     data: ARRAY [0..9] OF short; END;

  XClientMessageEvent_l = RECORD
     type: int;
     serial: unsigned_long;    (* # of last request processed by server *)
     send_event: Bool;         (* true if this came from a SendEvent request *)
     display: DisplayStar;     (* Display the event was read from *)
     window: Window;
     message_type: Atom;
     format: int;
     data: ARRAY [0..4] OF long; END;

  XClientMessageEventStar	= UNTRACED REF XClientMessageEvent;
  XClientMessageEvent_b_star	= UNTRACED REF XClientMessageEvent_b;
  XClientMessageEvent_s_star	= UNTRACED REF XClientMessageEvent_s;
  XClientMessageEvent_l_star	= UNTRACED REF XClientMessageEvent_l;

TYPE
  XMappingEvent = RECORD
     type: Int;
     serial: unsigned_long;     (* # of last request processed by server *)
     send_event: Bool;         (* true if this came from a SendEvent request *)
     display: DisplayStar;     (* Display the event was read from *)
     window: Window;           (* unused *)
     request: Int;             (* one of MappingModifier, MappingKeyboard,
                                  MappingPointer *)
     first_keycode: Int;       (* first keycode *)
     count: Int                (* defines range of change w. first_keycode*)
              END;

  XMappingEventStar		= UNTRACED REF   XMappingEvent;

TYPE
  XErrorEvent = RECORD
     type: Int;
     display: DisplayStar;      (* Display the event was read from *)
     resourceid: XID;           (* resource id *)
     serial: unsigned_long;      (* serial number of failed request *)
     error_code: unsigned_char;  (* error code of failed request *)
     request_code: unsigned_char;(* Major op-code of failed request *)
     minor_code: unsigned_char   (* Minor op-code of failed request *)
                 END;

   XErrorEventStar =  UNTRACED REF XErrorEvent;

TYPE
  XAnyEvent = RECORD
     type: Int;
     serial: unsigned_long; (* # of last request processed by server *)
     send_event: Bool;      (* true if this came from a SendEvent request *)
     display: DisplayStar;  (* Display the event was read from *)
     window: Window	(* window on which event was requested in event mask *)
              END;

  XAnyEventStar			= UNTRACED REF   XAnyEvent;


(*
 * This type is defined so Xlib can always use the same sized
 * event structure internally, to avoid memory fragmentation.
 *)

CONST XEventSize =
  MAX (BYTESIZE (Int),
  MAX (BYTESIZE (XAnyEvent),
  MAX (BYTESIZE (XKeyEvent),
  MAX (BYTESIZE (XButtonEvent),
  MAX (BYTESIZE (XMotionEvent),
  MAX (BYTESIZE (XCrossingEvent),
  MAX (BYTESIZE (XFocusChangeEvent),
  MAX (BYTESIZE (XExposeEvent),
  MAX (BYTESIZE (XGraphicsExposeEvent),
  MAX (BYTESIZE (XNoExposeEvent),
  MAX (BYTESIZE (XVisibilityEvent),
  MAX (BYTESIZE (XCreateWindowEvent),
  MAX (BYTESIZE (XDestroyWindowEvent),
  MAX (BYTESIZE (XUnmapEvent),
  MAX (BYTESIZE (XMapEvent),
  MAX (BYTESIZE (XMapRequestEvent),
  MAX (BYTESIZE (XReparentEvent),
  MAX (BYTESIZE (XConfigureEvent),
  MAX (BYTESIZE (XGravityEvent),
  MAX (BYTESIZE (XResizeRequestEvent),
  MAX (BYTESIZE (XConfigureRequestEvent),
  MAX (BYTESIZE (XCirculateEvent),
  MAX (BYTESIZE (XCirculateRequestEvent),
  MAX (BYTESIZE (XPropertyEvent),
  MAX (BYTESIZE (XSelectionClearEvent),
  MAX (BYTESIZE (XSelectionRequestEvent),
  MAX (BYTESIZE (XSelectionEvent),
  MAX (BYTESIZE (XColormapEvent),
  MAX (BYTESIZE (XClientMessageEvent),
  MAX (BYTESIZE (XMappingEvent),
  MAX (BYTESIZE (XErrorEvent),
  MAX (BYTESIZE (XKeymapEvent),
       BYTESIZE (ARRAY [1..24] OF Long)))))))))))))))))))))))))))))))));

TYPE
  XEvent = ARRAY [0 .. XEventSize - 1] OF Char;
  XEventStar  =  UNTRACED REF XEvent;


(*
 * _QEvent datatype for use in input queueing.
 *)

TYPE
  XQEvent = RECORD
    next: XQEventStar;
    event: XEvent
  END;
  XQEventStar = UNTRACED REF XQEvent;

(* ?!?!?!
#define XAllocID(dpy) (( *(dpy)->resource_alloc)((dpy)))
?!?!?! *)

(*
 * per character font metric information.
 *)

TYPE
  XCharStruct = RECORD
    lbearing: Short;            (* origin to left edge of raster *)
    rbearing: Short;            (* origin to right edge of raster *)
    width: Short;               (* advance to next char's origin *)
    ascent: Short;              (* baseline to top edge of raster *)
    descent: Short;             (* baseline to bottom edge of raster *)
    attributes: unsigned_short  (* per char flags (not predefined) *)
  END;
  XCharStructStar  =  UNTRACED REF XCharStruct;

(*
 * To allow arbitrary information with fonts, there are additional properties
 * returned.
 *)

TYPE
  XFontProp = RECORD
    name: Atom;
    card32: unsigned_long;
  END;
  XFontPropStar = UNTRACED REF XFontProp;

  XFontStruct = RECORD
    ext_data: XExtDataStar;      (* hook for extension to hang data *)
    fid: Font;            (* Font id for this font *)
    direction: unsigned_int;      (* hint about direction the font is painted *)
    min_char_or_byte2: unsigned_int;(* first character *)
    max_char_or_byte2: unsigned_int;(* last character *)
    min_byte1: unsigned_int;      (* first row that exists *)
    max_byte1: unsigned_int;      (* last row that exists *)
    all_chars_exist: Bool;(* flag if all characters have non-zero size*)
    default_char: unsigned_int;   (* char to print for undefined character *)
    n_properties: Int;   (* how many properties there are *)
    properties: XFontPropStar;    (* pointer to array of additional properties*)
    min_bounds: XCharStruct;     (* minimum bounds over all existing char *)
    max_bounds: XCharStruct;     (* maximum bounds over all existing char *)
    per_char: XCharStructStar;      (* first_char to last_char information *)
    ascent: Int;         (* log. extent above baseline for spacing *)
    descent: Int;        (* log. descent below baseline for spacing *)
  END;
  XFontStructStar      =  UNTRACED REF XFontStruct;
  XFontStructStarStar  =  UNTRACED REF XFontStructStar;

(*
 * PolyText routines take these as arguments.
 *)

TYPE
  XTextItem = RECORD
    chars: char_star;                (* pointer to string *)
    nchars: Int;                 (* number of characters *)
    delta: Int;                  (* delta between strings *)
    font: Font;                  (* font to print it in, None don't change *)
  END;

  XChar2b = RECORD
    byte1: unsigned_char;
    byte2: unsigned_char;
  END;

  XTextItem16 = RECORD
    chars: XChar2bStar;             (* two byte characters *)
    nchars: Int;                 (* number of characters *)
    delta: Int;                  (* delta between strings *)
    font: Font;                  (* font to print it in, None don't change *)
  END;

TYPE
  XTextItemStar    =  UNTRACED REF XTextItem;
  XChar2bStar      =  UNTRACED REF XChar2b;
  XTextItem16Star  =  UNTRACED REF XTextItem16;

(* ?!?!?!
typedef union { DisplayStar display;
                GC gc;
                VisualStar visual;
                ScreenStar screen;
                ScreenFormatStar pixmap_format;
                XFontStructStar font; } XEDataObject;
?!?!?! *)

TYPE
  XEDataObject  =  ADDRESS;


<*EXTERNAL*> PROCEDURE XLoadQueryFont(
        display:          DisplayStar;
        name:             const_char_star
): XFontStructStar RAISES {Error};

<*EXTERNAL*> PROCEDURE XQueryFont(
        display:          DisplayStar;
        font:             XID
): XFontStructStar RAISES {Error};

<*EXTERNAL*> PROCEDURE XGetMotionEvents(
        display:          DisplayStar;
        w:                Window;
        start:            Time;
        stop:             Time;
        nevents:          int_star
): XTimeCoordStar RAISES {Error};

<*EXTERNAL*> PROCEDURE XDeleteModifiermapEntry(
        modmap:           XModifierKeymapStar;
        keycode:          unsigned_int
): XModifierKeymapStar RAISES {Error};

<*EXTERNAL*> PROCEDURE XGetModifierMapping(
        display:          DisplayStar
): XModifierKeymapStar RAISES {Error};

<*EXTERNAL*> PROCEDURE XInsertModifiermapEntry(
        modmap:           XModifierKeymapStar;
        keycode:          KeyCode
): XModifierKeymapStar RAISES {Error};

<*EXTERNAL*> PROCEDURE XNewModifiermap(
        max:              Int
): XModifierKeymapStar RAISES {Error};

<*EXTERNAL*> PROCEDURE XCreateImage(
        display:          DisplayStar;
        visual:           VisualStar;
        depth:            unsigned_int;
        format:           Int;
        offset:           Int;
        data:             char_star;
        width:            unsigned_int;
        height:           unsigned_int;
        bitmap:           Int;
        bytes:            Int
): XImageStar RAISES {Error};

<*EXTERNAL*> PROCEDURE XGetImage(
        display:          DisplayStar;
        d:                Drawable;
        x:                Int;
        y:                Int;
        width:            unsigned_int;
        height:           unsigned_int;
        plane:            unsigned_long;
        format:           Int
): XImageStar RAISES {Error};

<*EXTERNAL*> PROCEDURE XGetSubImage(
        display:          DisplayStar;
        d:                Drawable;
        x:                Int;
        y:                Int;
        width:            unsigned_int;
        height:           unsigned_int;
        plane:            unsigned_long;
        format:           Int;
        dest_image:       XImageStar;
        dest_x:           Int;
        dest_y:           Int
): XImageStar RAISES {Error};

(*
 * X function declarations.
 *)

<*EXTERNAL*> PROCEDURE XOpenDisplay(
        display:          const_char_star;
): DisplayStar RAISES {Error};

<*EXTERNAL*> PROCEDURE XrmInitialize(
);

<*EXTERNAL*> PROCEDURE XFetchBytes(
        display:          DisplayStar;
        nbytes:           int_star
): char_star RAISES {Error};

<*EXTERNAL*> PROCEDURE XFetchBuffer(
        display:          DisplayStar;
        nbytes:           int_star;
        buffer:           Int
): char_star RAISES {Error};

<*EXTERNAL*> PROCEDURE XGetAtomName(
        display:          DisplayStar;
        atom:             Atom
): char_star RAISES {Error};

<*EXTERNAL*> PROCEDURE XGetDefault(
        display:          DisplayStar;
        program:          const_char_star;
        option:           const_char_star
): char_star RAISES {Error};

<*EXTERNAL*> PROCEDURE XDisplayName(
        string:           const_char_star;
): char_star RAISES {Error};

<*EXTERNAL*> PROCEDURE XKeysymToString(
        keysym:           KeySym;
): char_star RAISES {Error};


TYPE
  XSynchronize   =  PROCEDURE (
        display:    DisplayStar;
        onoff:      Bool
): Int RAISES {Error};


TYPE
  XSetLocalProc      =  PROCEDURE (display: DisplayStar): Int;
  XSetAfterFunction  =  PROCEDURE (display:    DisplayStar;
                                   procedure:  XSetLocalProc
                                   ): Int;


<*EXTERNAL*> PROCEDURE XInternAtom(
        display:          DisplayStar;
        atom:             const_char_star;
        only:             Bool
): Atom RAISES {Error};

<*EXTERNAL*> PROCEDURE XCopyColormapAndFree(
        display:          DisplayStar;
        colormap:         Colormap
): Colormap RAISES {Error};

<*EXTERNAL*> PROCEDURE XCreateColormap(
        display:          DisplayStar;
        w:                Window;
        visual:           VisualStar;
        alloc:            Int
): Colormap RAISES {Error};

<*EXTERNAL*> PROCEDURE XCreatePixmapCursor(
        display:          DisplayStar;
        source:           Pixmap;
        mask:             Pixmap;
        foreground:       XColorStar;
        background:       XColorStar;
        x:                unsigned_int;
        y:                unsigned_int
): Cursor RAISES {Error};

<*EXTERNAL*> PROCEDURE XCreateGlyphCursor(
        display:          DisplayStar;
        source_font:      Font;
        mask_font:        Font;
        source_char:      unsigned_int;
        mask_char:        unsigned_int;
        foreground:       XColorStar;
        background:       XColorStar
): Cursor RAISES {Error};

<*EXTERNAL*> PROCEDURE XCreateFontCursor(
        display:          DisplayStar;
        shape:            unsigned_int
): Cursor RAISES {Error};

<*EXTERNAL*> PROCEDURE XLoadFont(
        display:          DisplayStar;
        name:             const_char_star
): Font RAISES {Error};

<*EXTERNAL*> PROCEDURE XCreateGC(
        display:          DisplayStar;
        d:                Drawable;
        valuemask:        unsigned_long;
        values:           XGCValuesStar
): GC RAISES {Error};

<*EXTERNAL*> PROCEDURE XGContextFromGC(
        gc:               GC
): GContext RAISES {Error};

<*EXTERNAL*> PROCEDURE XCreatePixmap(
        display:          DisplayStar;
        d:                Drawable;
        width:            unsigned_int;
        height:           unsigned_int;
        depth:            unsigned_int
): Pixmap RAISES {Error};

<*EXTERNAL*> PROCEDURE XCreateBitmapFromData(
        display:          DisplayStar;
        d:                Drawable;
        data:             const_char_star;
        width:            unsigned_int;
        height:           unsigned_int
): Pixmap RAISES {Error};

<*EXTERNAL*> PROCEDURE XCreatePixmapFromBitmapData(
        display:          DisplayStar;
        d:                Drawable;
        data:             char_star;
        width:            unsigned_int;
        height:           unsigned_int;
        fg:               unsigned_long;
        bg:               unsigned_long;
        depth:            unsigned_int
): Pixmap RAISES {Error};

<*EXTERNAL*> PROCEDURE XCreateSimpleWindow(
        display:          DisplayStar;
        parent:           Window;
        x:                Int;
        y:                Int;
        width:            unsigned_int;
        height:           unsigned_int;
        border_width:     unsigned_int;
        border:           unsigned_long;
        background:       unsigned_long
): Window RAISES {Error};

<*EXTERNAL*> PROCEDURE XGetSelectionOwner(
        display:          DisplayStar;
        selection:        Atom
): Window RAISES {Error};

<*EXTERNAL*> PROCEDURE XCreateWindow(
        display:          DisplayStar;
        parent:           Window;
        x:                Int;
        y:                Int;
        width:            unsigned_int;
        height:           unsigned_int;
        border:           unsigned_int;
        depth:            Int;
        class:            unsigned_int;
        visual:           VisualStar;
        valuemask:        unsigned_long;
        attributes:       XSetWindowAttributesStar
): Window RAISES {Error};

<*EXTERNAL*> PROCEDURE XListInstalledColormaps(
        display:          DisplayStar;
        w:                Window;
        num:              int_star
): ColormapStar RAISES {Error};

<*EXTERNAL*> PROCEDURE XListFonts(
        display:          DisplayStar;
        pattern:          const_char_star;
        maxnames:         Int;
        actual:           int_star
): char_star_star RAISES {Error};

<*EXTERNAL*> PROCEDURE XListFontsWithInfo(
        display:          DisplayStar;
        pattern:          const_char_star;
        maxnames:         Int;
        count:            int_star;
        info:             XFontStructStarStar
): char_star_star RAISES {Error};

<*EXTERNAL*> PROCEDURE XGetFontPath(
        display:          DisplayStar;
        npaths:           int_star
): char_star_star RAISES {Error};

<*EXTERNAL*> PROCEDURE XListExtensions(
        display:          DisplayStar;
        nextensions:      int_star
): char_star_star RAISES {Error};

<*EXTERNAL*> PROCEDURE XListProperties(
        display:          DisplayStar;
        w:                Window;
        num:              int_star
): AtomStar RAISES {Error};

<*EXTERNAL*> PROCEDURE XListHosts(
        display:          DisplayStar;
        nhosts:           int_star;
        state:            BoolStar;
): XHostAddressStar RAISES {Error};

<*EXTERNAL*> PROCEDURE XKeycodeToKeysym(
        display:          DisplayStar;
        keycode:          KeyCode
): KeySym RAISES {Error};

<*EXTERNAL*> PROCEDURE XLookupKeysym(
        key:              XKeyEventStar;
        index:            Int
): KeySym RAISES {Error};

<*EXTERNAL*> PROCEDURE XGetKeyboardMapping(
        display:          DisplayStar;
        first:            KeyCode;
        keycode:          Int;
        keysyms:          int_star
): KeySymStar RAISES {Error};

<*EXTERNAL*> PROCEDURE XStringToKeysym(
        string:           const_char_star
): KeySym;

<*EXTERNAL*> PROCEDURE XMaxRequestSize(
        display:          DisplayStar
): Long;

<*EXTERNAL*> PROCEDURE XResourceManagerString(
        display:          DisplayStar
): char_star RAISES {Error};

<*EXTERNAL*> PROCEDURE XDisplayMotionBufferSize(
        display:          DisplayStar
): unsigned_long;

<*EXTERNAL*> PROCEDURE XVisualIDFromVisual(
        visual:           VisualStar
): VisualID;

(* routines for dealing with extensions *)

<*EXTERNAL*> PROCEDURE XInitExtension(
        display:          DisplayStar;
        name:             const_char_star
): XExtCodesStar;

<*EXTERNAL*> PROCEDURE XAddExtension(
        display:          DisplayStar
): XExtCodesStar;

<*EXTERNAL*> PROCEDURE XFindOnExtensionList(
        structure:        XExtDataStarStar;
        number:           Int
): XExtDataStar;

<*EXTERNAL*> PROCEDURE XEHeadOfExtensionList(
        object:           XEDataObject
): XExtDataStarStar;

(* these are routines for which there are also macros *)

<*EXTERNAL*> PROCEDURE XRootWindow(
        display:          DisplayStar;
        screen:           Int
): Window;

<*EXTERNAL*> PROCEDURE XDefaultRootWindow(
        display:          DisplayStar
): Window;

<*EXTERNAL*> PROCEDURE XRootWindowOfScreen(
        screen:           ScreenStar
): Window;

<*EXTERNAL*> PROCEDURE XDefaultVisual(
        display:          DisplayStar;
        screen:           Int
): VisualStar;

<*EXTERNAL*> PROCEDURE XDefaultVisualOfScreen(
        screen:           ScreenStar
): VisualStar;

<*EXTERNAL*> PROCEDURE XDefaultGC(
        display:          DisplayStar;
        screen:           Int
): GC;

<*EXTERNAL*> PROCEDURE XDefaultGCOfScreen(
        screen:           ScreenStar
): GC;

<*EXTERNAL*> PROCEDURE XBlackPixel(
        display:          DisplayStar;
        screen:           Int
): unsigned_long;

<*EXTERNAL*> PROCEDURE XWhitePixel(
        display:          DisplayStar;
        screen:           Int
): unsigned_long;

<*EXTERNAL*> PROCEDURE XAllPlanes(
): unsigned_long;

<*EXTERNAL*> PROCEDURE XBlackPixelOfScreen(
        screen:           ScreenStar
): unsigned_long;

<*EXTERNAL*> PROCEDURE XWhitePixelOfScreen(
        screen:           ScreenStar
): unsigned_long;

<*EXTERNAL*> PROCEDURE XNextRequest(
        display:          DisplayStar
): unsigned_long;


(* ---- double definition ------
<*EXTERNAL*> PROCEDURE XLastKnownRequestProcessed(
        display:          DisplayStar
): unsigned_long;  *)

<*EXTERNAL*> PROCEDURE XServerVendor(
        display:          DisplayStar
): char_star;

<*EXTERNAL*> PROCEDURE XDisplayString(
        display:          DisplayStar
): char_star;

<*EXTERNAL*> PROCEDURE XDefaultColormap(
        display:          DisplayStar;
        screen:           Int
): Colormap;

<*EXTERNAL*> PROCEDURE XDefaultColormapOfScreen(
        screen:           ScreenStar
): Colormap;

<*EXTERNAL*> PROCEDURE XDisplayOfScreen(
        screen:           ScreenStar
): DisplayStar;

<*EXTERNAL*> PROCEDURE XScreenOfDisplay(
        display:          DisplayStar;
        screen:           Int
): ScreenStar;

<*EXTERNAL*> PROCEDURE XDefaultScreenOfDisplay(
        display:          DisplayStar
): ScreenStar;

<*EXTERNAL*> PROCEDURE XEventMaskOfScreen(
        screen:           ScreenStar
): Long RAISES {Error};

<*EXTERNAL*> PROCEDURE XScreenNumberOfScreen(
        screen:           ScreenStar
): Int;

TYPE XErrorHandler = PROCEDURE (      (* WARNING, this type not in Xlib spec *)
        display:          DisplayStar;
        error:            XErrorEventStar
): Int RAISES {Error};

<*EXTERNAL*> PROCEDURE XSetErrorHandler (
        handler:          XErrorHandler
): XErrorHandler;

TYPE XIOErrorHandler = PROCEDURE (    (* WARNING, this type not in Xlib spec *)
        display:          DisplayStar
): Int RAISES {Error};

<*EXTERNAL*> PROCEDURE XSetIOErrorHandler (
        handler:          XIOErrorHandler
): XIOErrorHandler;


<*EXTERNAL*> PROCEDURE XListPixmapFormats(
        display:          DisplayStar;
        count:            int_star
): XPixmapFormatValuesStar;

<*EXTERNAL*> PROCEDURE XListDepths(
        display:          DisplayStar;
        screen:           Int;
        count:            int_star
): int_star;

(* ICCCM routines for things that don't require special include files; *)
(* other declarations are given in Xutil.h                             *)

<*EXTERNAL*> PROCEDURE XReconfigureWMWindow(
        display:          DisplayStar;
        w:                Window;
        screen:           Int;
        mask:             unsigned_int;
        changes:          XWindowChangesStar
): Status RAISES {Error};

<*EXTERNAL*> PROCEDURE XGetWMProtocols(
        display:          DisplayStar;
        w:                Window;
        protocols:        AtomStarStar;
        count:            int_star
): Status RAISES {Error};

<*EXTERNAL*> PROCEDURE XSetWMProtocols(
        display:          DisplayStar;
        w:                Window;
        protocols:        AtomStar;
        count:            Int
): Status RAISES {Error};

<*EXTERNAL*> PROCEDURE XIconifyWindow(
        display:          DisplayStar;
        w:                Window;
        screen:           Int
): Status RAISES {Error};

<*EXTERNAL*> PROCEDURE XWithdrawWindow(
        display:          DisplayStar;
        w:                Window;
        screen:           Int
): Status RAISES {Error};

<*EXTERNAL*> PROCEDURE XGetCommand(
        display:          DisplayStar;
        w:                Window;
        argv:             Argv;
        argc:             int_star
): Status RAISES {Error};

<*EXTERNAL*> PROCEDURE XGetWMColormapWindows(
        display:          DisplayStar;
        w:                Window;
        windows:          WindowStarStar;
        count:            int_star
): Status RAISES {Error};

<*EXTERNAL*> PROCEDURE XSetWMColormapWindows(
        display:          DisplayStar;
        w:                Window;
        colormap:         WindowStar;
        count:            Int
): Status RAISES {Error};

<*EXTERNAL*> PROCEDURE XFreeStringList(
        list:             char_star_star
): void_star  RAISES {Error};

<*EXTERNAL*> PROCEDURE XSetTransientForHint(
        display:          DisplayStar;
        w:                Window;
        prop:             Window
) RAISES {Error};

(* The following are given in alphabetical order *)

<*EXTERNAL*> PROCEDURE XActivateScreenSaver(
        display:          DisplayStar
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XAddHost(
        display:          DisplayStar;
        host:             XHostAddressStar
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XAddHosts(
        display:          DisplayStar;
        hosts:            XHostAddressStar;
        num:              Int
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XAddToExtensionList(
        structure:        ADDRESS;
        ext:              XExtDataStar
);

<*EXTERNAL*> PROCEDURE XAddToSaveSet(
        display:          DisplayStar;
        w:                Window
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XAllocColor(
        display:          DisplayStar;
        colormap:         Colormap;
        color:            XColorStar
): Status RAISES {Error};

<*EXTERNAL*> PROCEDURE XAllocColorCells(
        display:          DisplayStar;
        colormap:         Colormap;
        contig:           Bool;
        plane:            unsigned_long_star;
        nplanes:          unsigned_int;
        pixels:           unsigned_long_star;
        npixels:          unsigned_int
): Status RAISES {Error};

<*EXTERNAL*> PROCEDURE XAllocColorPlanes(
        display:          DisplayStar;
        colormap:         Colormap;
        contig:           Bool;
        pixels:           unsigned_long_star;
        ncolors:          Int;
        nreds:            Int;
        ngreens:          Int;
        nblues:           Int;
        rmask:            unsigned_long_star;
        gmask:            unsigned_long_star;
        bmask:            unsigned_long_star
): Status RAISES {Error};

<*EXTERNAL*> PROCEDURE XAllocNamedColor(
        display:          DisplayStar;
        colormap:         Colormap;
        color:            const_char_star;
        screen:           XColorStar;
        exact:            XColorStar
): Status RAISES {Error};

<*EXTERNAL*> PROCEDURE XAllowEvents(
        display:          DisplayStar;
        event:            Int;
        time:             Time
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XAutoRepeatOff(
        display:          DisplayStar
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XAutoRepeatOn(
        display:          DisplayStar
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XBell(
        display:          DisplayStar;
        percent:          Int
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XBitmapBitOrder(
        display:          DisplayStar
): Int RAISES {Error};

<*EXTERNAL*> PROCEDURE XBitmapPad(
        display:          DisplayStar
): Int RAISES {Error};

<*EXTERNAL*> PROCEDURE XBitmapUnit(
        display:          DisplayStar
): Int RAISES {Error};

<*EXTERNAL*> PROCEDURE XCellsOfScreen(
        screen:           ScreenStar
): Int RAISES {Error};

<*EXTERNAL*> PROCEDURE XChangeActivePointerGrab(
        display:          DisplayStar;
        event:            unsigned_int;
        cursor:           Cursor;
        time:             Time
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XChangeGC(
        display:          DisplayStar;
        gc:               GC;
        valuemask:        unsigned_long;
        values:           XGCValuesStar
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XChangeKeyboardControl(
        display:          DisplayStar;
        value:            unsigned_long;
        values:           XKeyboardControlStar
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XChangeKeyboardMapping(
        display:          DisplayStar;
        first:            Int;
        keysyms_per_key:  Int;
        keysyms:          KeySymStar;
        num:              Int
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XChangePointerControl(
        display:          DisplayStar;
        do_accel:         Bool;
        do_threshold:     Bool;
        accel_num:        Int;
        accel_den:        Int;
        threshold:        Int
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XChangeProperty(
        display:          DisplayStar;
        w:                Window;
        property:         Atom;
        type:             Atom;
        format:           Int;
        mode:             Int;
        data:             const_unsigned_char_star;
        nelements:        Int
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XChangeSaveSet(
        display:          DisplayStar;
        w:                Window;
        change:           Int
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XChangeWindowAttributes(
        display:          DisplayStar;
        w:                Window;
        valuemask:        unsigned_long;
        attributes:       XSetWindowAttributesStar
) RAISES {Error};


TYPE XIfEventProc = PROCEDURE (
        display:          DisplayStar;
        event:            XEventStar;
        arg:              char_star
  ): Bool RAISES {Error};

<*EXTERNAL*> PROCEDURE XCheckIfEvent(
        display:          DisplayStar;
        event:            XEventStar;
        predicate:        XIfEventProc;
        arg:              char_star;
):  Bool RAISES {Error};

<*EXTERNAL*> PROCEDURE XCheckMaskEvent(
        display:          DisplayStar;
        event_mask:       Long;
        event_return:     XEventStar
): Bool RAISES {Error};

<*EXTERNAL*> PROCEDURE XCheckTypedEvent(
        display:          DisplayStar;
        event_type:       Int;
        event_return:     XEventStar
): Bool RAISES {Error};

<*EXTERNAL*> PROCEDURE XCheckTypedWindowEvent(
        display:          DisplayStar;
        w:                Window;
        event_type:       Int;
        event_return:     XEventStar
): Bool RAISES {Error};

<*EXTERNAL*> PROCEDURE XCheckWindowEvent(
        display:          DisplayStar;
        w:                Window;
        event_mask:       Long;
        event_return:     XEventStar
): Bool RAISES {Error};

<*EXTERNAL*> PROCEDURE XCirculateSubwindows(
        display:          DisplayStar;
        w:                Window;
        direction:        Int
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XCirculateSubwindowsDown(
        display:          DisplayStar;
        w:                Window
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XCirculateSubwindowsUp(
        display:          DisplayStar;
        w:                Window
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XClearArea(
        display:          DisplayStar;
        w:                Window;
        x:                Int;
        y:                Int;
        width:            unsigned_int;
        height:           unsigned_int;
        exposures:        Bool
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XClearWindow(
        display:          DisplayStar;
        w:                Window
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XCloseDisplay(
        display:          DisplayStar
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XConfigureWindow(
        display:          DisplayStar;
        w:                Window;
        value:            unsigned_int;
        values:           XWindowChangesStar
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XConnectionNumber(
        display:          DisplayStar
): Int RAISES {Error};

<*EXTERNAL*> PROCEDURE XConvertSelection(
        display:          DisplayStar;
        selection:        Atom;
        target:           Atom;
        property:         Atom;
        requestor:        Window;
        time:             Time
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XCopyArea(
        display:          DisplayStar;
        src:              Drawable;
        dest:             Drawable;
        gc:               GC;
        src_x:            Int;
        src_y:            Int;
        width:            unsigned_int;
        height:           unsigned_int;
        dest_x:           Int;
        dest_y:           Int
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XCopyGC(
        display:          DisplayStar;
        src:              GC;
        valuemask:        unsigned_long;
        dest:             GC
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XCopyPlane(
        display:          DisplayStar;
        src:              Drawable;
        dest:             Drawable;
        gc:               GC;
        src_x:            Int;
        src_y:            Int;
        width:            unsigned_int;
        height:           unsigned_int;
        dest_x:           Int;
        dest_y:           Int;
        plane:            unsigned_long
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XDefaultDepth(
        display:          DisplayStar;
        screen:           Int
): Int;

<*EXTERNAL*> PROCEDURE XDefaultDepthOfScreen(
        screen:           ScreenStar
): Int;

<*EXTERNAL*> PROCEDURE XDefaultScreen(
        display:          DisplayStar
): Int;

<*EXTERNAL*> PROCEDURE XDefineCursor(
        display:          DisplayStar;
        w:                Window;
        cursor:           Cursor
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XDeleteProperty(
        display:          DisplayStar;
        w:                Window;
        property:         Atom
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XDestroyWindow(
        display:          DisplayStar;
        w:                Window
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XDestroySubwindows(
        display:          DisplayStar;
        w:                Window
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XDoesBackingStore(
        screen:           ScreenStar
): Int RAISES {Error};

<*EXTERNAL*> PROCEDURE XDoesSaveUnders(
        screen:           ScreenStar
): Bool RAISES {Error};

<*EXTERNAL*> PROCEDURE XDisableAccessControl(
        display:          DisplayStar
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XDisplayCells(
        display:          DisplayStar;
        screen:           Int
): Int RAISES {Error};

<*EXTERNAL*> PROCEDURE XDisplayHeight(
        display:          DisplayStar;
        screen:           Int
): Int RAISES {Error};

<*EXTERNAL*> PROCEDURE XDisplayHeightMM(
        display:          DisplayStar;
        screen:           Int
): Int RAISES {Error};

<*EXTERNAL*> PROCEDURE XDisplayKeycodes(
        display:          DisplayStar;
        min:              int_star;
        max:              int_star
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XDisplayPlanes(
        display:          DisplayStar;
        screen:           Int
): Int RAISES {Error};

<*EXTERNAL*> PROCEDURE XDisplayWidth(
        display:          DisplayStar;
        screen:           Int
): Int RAISES {Error};

<*EXTERNAL*> PROCEDURE XDisplayWidthMM(
        display:          DisplayStar;
        screen:           Int
): Int RAISES {Error};

<*EXTERNAL*> PROCEDURE XDrawArc(
        display:          DisplayStar;
        d:                Drawable;
        gc:               GC;
        x:                Int;
        y:                Int;
        width:            unsigned_int;
        height:           unsigned_int;
        angle1:           Int;
        angle2:           Int
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XDrawArcs(
        display:          DisplayStar;
        d:                Drawable;
        gc:               GC;
        arcs:             XArcStar;
        narcs:            Int
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XDrawImageString(
        display:          DisplayStar;
        d:                Drawable;
        gc:               GC;
        x:                Int;
        y:                Int;
        string:           const_char_star;
        length:           Int
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XDrawImageString16(
        display:          DisplayStar;
        d:                Drawable;
        gc:               GC;
        x:                Int;
        y:                Int;
        string:           XChar2bStar;
        length:           Int
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XDrawLine(
        display:          DisplayStar;
        d:                Drawable;
        gc:               GC;
        x1:               Int;
        x2:               Int;
        y1:               Int;
        y2:               Int
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XDrawLines(
        display:          DisplayStar;
        d:                Drawable;
        gc:               GC;
        points:           XPoint_star;
        npoints:          Int;
        mode:             Int
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XDrawPoint(
        display:          DisplayStar;
        d:                Drawable;
        gc:               GC;
        x:                Int;
        y:                Int
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XDrawPoints(
        display:          DisplayStar;
        d:                Drawable;
        gc:               GC;
        points:           XPoint_star;
        npoints:          Int;
        mode:             Int
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XDrawRectangle(
        display:          DisplayStar;
        d:                Drawable;
        gc:               GC;
        x:                Int;
        y:                Int;
        width:            unsigned_int;
        height:           unsigned_int
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XDrawRectangles(
        display:          DisplayStar;
        d:                Drawable;
        gc:               GC;
        rectangles:       XRectangleStar;
        nrectangles:      Int
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XDrawSegments(
        display:          DisplayStar;
        d:                Drawable;
        gc:               GC;
        segments:         XSegmentStar;
        nsegments:        Int
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XDrawString(
        display:          DisplayStar;
        d:                Drawable;
        gc:               GC;
        x:                Int;
        y:                Int;
        string:           const_char_star;
        length:           Int
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XDrawString16(
        display:          DisplayStar;
        d:                Drawable;
        gc:               GC;
        x:                Int;
        y:                Int;
        string:           XChar2bStar;
        length:           Int
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XDrawText(
        display:          DisplayStar;
        d:                Drawable;
        gc:               GC;
        x:                Int;
        y:                Int;
        items:            XTextItemStar;
        nitems:           Int
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XDrawText16(
        display:          DisplayStar;
        d:                Drawable;
        gc:               GC;
        x:                Int;
        y:                Int;
        items:            XTextItem16Star;
        nitems:           Int
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XEnableAccessControl(
        display:          DisplayStar
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XEventsQueued(
        display:          DisplayStar;
        mode:             Int
): Int RAISES {Error};

<*EXTERNAL*> PROCEDURE XFetchName(
        display:          DisplayStar;
        w:                Window;
        window:           char_star_star
): Status RAISES {Error};

<*EXTERNAL*> PROCEDURE XFillArc(
        display:          DisplayStar;
        d:                Drawable;
        gc:               GC;
        x:                Int;
        y:                Int;
        width:            unsigned_int;
        height:           unsigned_int;
        angle1:           Int;
        angle2:           Int
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XFillArcs(
        display:          DisplayStar;
        d:                Drawable;
        gc:               GC;
        arcs:             XArcStar;
        narcs:            Int
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XFillPolygon(
        display:          DisplayStar;
        d:                Drawable;
        gc:               GC;
        points:           XPoint_star;
        npoints:          Int;
        shape:            Int;
        mode:             Int
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XFillRectangle(
        display:          DisplayStar;
        d:                Drawable;
        gc:               GC;
        x:                Int;
        y:                Int;
        width:            unsigned_int;
        height:           unsigned_int
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XFillRectangles(
        display:          DisplayStar;
        d:                Drawable;
        gc:               GC;
        rectangles:       XRectangleStar;
        nrectangles:      Int
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XFlush(
        display:          DisplayStar
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XForceScreenSaver(
        display:          DisplayStar;
        mode:             Int
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XFree(
        data:             char_star
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XFreeColormap(
        display:          DisplayStar;
        colormap:         Colormap
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XFreeColors(
        display:          DisplayStar;
        colormap:         Colormap;
        pixels:           unsigned_long_star;
        npixels:          Int;
        planes:           unsigned_long
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XFreeCursor(
        display:          DisplayStar;
        cursor:           Cursor
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XFreeExtensionList(
        list:             char_star_star
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XFreeFont(
        display:          DisplayStar;
        font:             XFontStructStar
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XFreeFontInfo(
        names:            char_star_star;
        free:             XFontStructStar;
        actual:           Int
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XFreeFontNames(
        list:             char_star_star
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XFreeFontPath(
        list:             char_star_star
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XFreeGC(
        display:          DisplayStar;
        gc:               GC
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XFreeModifiermap(
        modmap:           XModifierKeymapStar
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XFreePixmap(
        display:          DisplayStar;
        pixmap:           Pixmap
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XGeometry(
        display:          DisplayStar;
        screen:           Int;
        position:         const_char_star;
        default:          const_char_star;
        bwidth:           unsigned_int;
        fwidth:           unsigned_int;
        fheight:          unsigned_int;
        xadder:           Int;
        yadder:           Int;
        x:                int_star;
        y:                int_star;
        width:            int_star;
        height:           int_star
): Int RAISES {Error};

<*EXTERNAL*> PROCEDURE XGetErrorDatabaseText(
        display:          DisplayStar;
        name:             const_char_star;
        message:          const_char_star;
        default:          const_char_star;
        buffer:           char_star;
        length:           Int
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XGetErrorText(
        display:          DisplayStar;
        code:             Int;
        buffer:           char_star;
        length:           Int
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XGetFontProperty(
        font:             XFontStructStar;
        atom:             Atom;
        value:            unsigned_long_star
): Bool RAISES {Error};

<*EXTERNAL*> PROCEDURE XGetGCValues(
        display:          DisplayStar;
        gc:               GC;
        valuemask:        unsigned_long;
        values:           XGCValuesStar
): Status RAISES {Error};

<*EXTERNAL*> PROCEDURE XGetGeometry(
        display:          DisplayStar;
        d:                Drawable;
        root:             WindowStar;
        x:                int_star;
        y:                int_star;
        width:            unsigned_int_star;
        height:           unsigned_int_star;
        border:           unsigned_int_star;
        depth:            unsigned_int_star
): Status RAISES {Error};

<*EXTERNAL*> PROCEDURE XGetIconName(
        display:          DisplayStar;
        w:                Window;
        icon:             char_star_star
): Status RAISES {Error};

<*EXTERNAL*> PROCEDURE XGetInputFocus(
        display:          DisplayStar;
        focus:            WindowStar;
        revert:           int_star
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XGetKeyboardControl(
        display:          DisplayStar;
        values:           XKeyboardStateStar
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XGetPointerControl(
        display:          DisplayStar;
        accel_num_ret:    int_star;
        accel_den_ret:    int_star;
        threshold_ret:    int_star
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XGetPointerMapping(
        display:          DisplayStar;
        map:              unsigned_char_star;
        nmap:             Int
): Int RAISES {Error};

<*EXTERNAL*> PROCEDURE XGetScreenSaver(
        display:          DisplayStar;
        timeout:          int_star;
        interval:         int_star;
        prefer:           int_star;
        allow:            int_star
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XGetTransientForHint(
        display:          DisplayStar;
        w:                Window;
        prop:             WindowStar
): Status RAISES {Error};

<*EXTERNAL*> PROCEDURE XGetWindowProperty(
        display:          DisplayStar;
        w:                Window;
        property:         Atom;
        long_offset:      Long;
        long_length:      Long;
        delete:           Bool;
        req:              Atom;
        actual_type:      AtomStar;
        actual_format:    int_star;
        nitems:           unsigned_long_star;
        bytes:            unsigned_long_star;
        prop:             unsigned_char_star_star
): Int RAISES {Error};

<*EXTERNAL*> PROCEDURE XGetWindowAttributes(
        display:          DisplayStar;
        w:                Window;
        window:           XWindowAttributesStar
): Status RAISES {Error};

<*EXTERNAL*> PROCEDURE XGrabButton(
        display:          DisplayStar;
        button:           unsigned_int;
        modifiers:        unsigned_int;
        grab:             Window;
        owner:            Bool;
        event:            unsigned_int;
        pointer:          Int;
        keyboard:         Int;
        confine:          Window;
        cursor:           Cursor
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XGrabKey(
        display:          DisplayStar;
        keycode:          Int;
        modifiers:        unsigned_int;
        grab:             Window;
        owner:            Bool;
        pointer:          Int;
        keyboard:         Int
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XGrabKeyboard(
        display:          DisplayStar;
        grab:             Window;
        owner:            Bool;
        pointer:          Int;
        keyboard:         Int;
        time:             Time
): Int RAISES {Error};

<*EXTERNAL*> PROCEDURE XGrabPointer(
        display:          DisplayStar;
        grab:             Window;
        owner:            Bool;
        event:            unsigned_int;
        pointer:          Int;
        keyboard:         Int;
        confine:          Window;
        cursor:           Cursor;
        time:             Time
): Int RAISES {Error};

<*EXTERNAL*> PROCEDURE XGrabServer(
        display:          DisplayStar
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XHeightMMOfScreen(
        screen:           ScreenStar
): Int RAISES {Error};

<*EXTERNAL*> PROCEDURE XHeightOfScreen(
        screen:           ScreenStar
): Int RAISES {Error};

<*EXTERNAL*> PROCEDURE XIfEvent(
        display:          DisplayStar;
        event:            XEventStar;
        predicate:        XIfEventProc;
        arg:              char_star
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XImageByteOrder(
        display:          DisplayStar
): Int RAISES {Error};

<*EXTERNAL*> PROCEDURE XInstallColormap(
        display:          DisplayStar;
        colormap:         Colormap
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XKeysymToKeycode(
        display:          DisplayStar;
        keysym:           KeySym
): KeyCode RAISES {Error};

<*EXTERNAL*> PROCEDURE XKillClient(
        display:          DisplayStar;
        resource:         XID
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XLastKnownRequestProcessed(
        display:          DisplayStar
): unsigned_long  RAISES {Error};

<*EXTERNAL*> PROCEDURE XLookupColor(
        display:          DisplayStar;
        colormap:         Colormap;
        color:            const_char_star;
        exact:            XColorStar;
        screen:           XColorStar
): Status RAISES {Error};

<*EXTERNAL*> PROCEDURE XLowerWindow(
        display:          DisplayStar;
        w:                Window
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XMapRaised(
        display:          DisplayStar;
        w:                Window
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XMapSubwindows(
        display:          DisplayStar;
        w:                Window
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XMapWindow(
        display:          DisplayStar;
        w:                Window
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XMaskEvent(
        display:          DisplayStar;
        event_mask:       Long;
        event_return:     XEventStar
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XMaxCmapsOfScreen(
        screen:           ScreenStar
): Int RAISES {Error};

<*EXTERNAL*> PROCEDURE XMinCmapsOfScreen(
        screen:           ScreenStar
): Int RAISES {Error};

<*EXTERNAL*> PROCEDURE XMoveResizeWindow(
        display:          DisplayStar;
        w:                Window;
        x:                Int;
        y:                Int;
        width:            unsigned_int;
        height:           unsigned_int
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XMoveWindow(
        display:          DisplayStar;
        w:                Window;
        x:                Int;
        y:                Int
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XNextEvent(
        display:          DisplayStar;
        event:            XEventStar
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XNoOp(
        display:          DisplayStar
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XParseColor(
        display:          DisplayStar;
        colormap:         Colormap;
        spec:             const_char_star;
        exact:            XColorStar
): Status RAISES {Error};

<*EXTERNAL*> PROCEDURE XParseGeometry(
        parsestring:      const_char_star;
        x:                int_star;
        y:                int_star;
        width:            unsigned_int_star;
        height:           unsigned_int_star
): Int RAISES {Error};

<*EXTERNAL*> PROCEDURE XPeekEvent(
        display:          DisplayStar;
        event:            XEventStar
) RAISES {Error};


<*EXTERNAL*> PROCEDURE XPeekIfEvent(
        display:          DisplayStar;
        event:            XEventStar;
        predicate:        XIfEventProc;
        arg:              char_star
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XPending(
        display:          DisplayStar
): Int RAISES {Error};

<*EXTERNAL*> PROCEDURE XPlanesOfScreen(
        screen:           ScreenStar
): Int RAISES {Error};

<*EXTERNAL*> PROCEDURE XProtocolRevision(
        display:          DisplayStar
): Int RAISES {Error};

<*EXTERNAL*> PROCEDURE XProtocolVersion(
        display:          DisplayStar
): Int RAISES {Error};

<*EXTERNAL*> PROCEDURE XPutBackEvent(
        display:          DisplayStar;
        event:            XEventStar
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XPutImage(
        display:          DisplayStar;
        d:                Drawable;
        gc:               GC;
        image:            XImageStar;
        src_x:            Int;
        src_y:            Int;
        dest_x:           Int;
        dest_y:           Int;
        width:            unsigned_int;
        height:           unsigned_int
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XQLength(
        display:          DisplayStar
): Int RAISES {Error};

<*EXTERNAL*> PROCEDURE XQueryBestCursor(
        display:          DisplayStar;
        d:                Drawable;
        width:            unsigned_int;
        height:           unsigned_int;
        width_return:     unsigned_int_star;
        height_return:    unsigned_int_star
): Status RAISES {Error};

<*EXTERNAL*> PROCEDURE XQueryBestSize(
        display:          DisplayStar;
        class:            Int;
        which:            Drawable;
        width:            unsigned_int;
        height:           unsigned_int;
        width_return:     unsigned_int_star;
        height_return:    unsigned_int_star
): Status RAISES {Error};

<*EXTERNAL*> PROCEDURE XQueryBestStipple(
        display:          DisplayStar;
        which:            Drawable;
        width:            unsigned_int;
        height:           unsigned_int;
        width_return:     unsigned_int_star;
        height_return:    unsigned_int_star
): Status RAISES {Error};

<*EXTERNAL*> PROCEDURE XQueryBestTile(
        display:          DisplayStar;
        which:            Drawable;
        width:            unsigned_int;
        height:           unsigned_int;
        width_return:     unsigned_int_star;
        height_return:    unsigned_int_star
): Status RAISES {Error};

<*EXTERNAL*> PROCEDURE XQueryColor(
        display:          DisplayStar;
        colormap:         Colormap;
        def:              XColorStar
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XQueryColors(
        display:          DisplayStar;
        colormap:         Colormap;
        defs:             XColorStar;
        ncolors:          Int
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XQueryExtension(
        display:          DisplayStar;
        name:             const_char_star;
        major_opcode:     int_star;
        first_event:      int_star;
        first_error:      int_star
): Bool RAISES {Error};

<*EXTERNAL*> PROCEDURE XQueryKeymap(
        display:          DisplayStar;
        keys:             Char
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XQueryPointer(
        display:          DisplayStar;
        w:                Window;
        root:             WindowStar;
        child:            WindowStar;
        root_x:           int_star;
        root_y:           int_star;
        win_x:            int_star;
        win_y:            int_star;
        mask:             unsigned_int_star
): Bool RAISES {Error};

<*EXTERNAL*> PROCEDURE XQueryTextExtents(
        display:          DisplayStar;
        font_ID:          XID;
        string:           const_char_star;
        nchars:           Int;
        direction:        int_star;
        font_ascent:      int_star;
        font_descent:     int_star;
        overall:          XCharStructStar
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XQueryTextExtents16(
        display:          DisplayStar;
        font_ID:          XID;
        string:           XChar2bStar;
        nchars:           Int;
        direction:        int_star;
        font_ascent:      int_star;
        font_descent:     int_star;
        overall:          XCharStructStar
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XQueryTree(
        display:          DisplayStar;
        w:                Window;
        root:             WindowStar;
        parent:           WindowStar;
        children:         WindowStarStar;
        nchildren:        unsigned_int_star
): Status RAISES {Error};

<*EXTERNAL*> PROCEDURE XRaiseWindow(
        display:          DisplayStar;
        w:                Window
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XReadBitmapFile(
        display:          DisplayStar;
        d:                Drawable;
        filename:         const_char_star;
        width:            unsigned_int_star;
        height:           unsigned_int_star;
        bitmap:           PixmapStar;
        x:                int_star;
        y:                int_star
): Int RAISES {Error};

<*EXTERNAL*> PROCEDURE XRebindKeysym(
        display:          DisplayStar;
        keysym:           KeySym;
        list:             KeySymStar;
        mod:              Int;
        string:           const_unsigned_char_star;
        bytes:            Int
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XRecolorCursor(
        display:          DisplayStar;
        cursor:           Cursor;
        foreground:       XColorStar;
        background:       XColorStar
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XRefreshKeyboardMapping(
        event:            XMappingEventStar
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XRemoveFromSaveSet(
        display:          DisplayStar;
        w:                Window
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XRemoveHost(
        display:          DisplayStar;
        host:             XHostAddressStar
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XRemoveHosts(
        display:          DisplayStar;
        hosts:            XHostAddressStar;
        num:              Int
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XReparentWindow(
        display:          DisplayStar;
        w:                Window;
        parent:           Window;
        x:                Int;
        y:                Int
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XResetScreenSaver(
        display:          DisplayStar
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XResizeWindow(
        display:          DisplayStar;
        w:                Window;
        width:            unsigned_int;
        height:           unsigned_int
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XRestackWindows(
        display:          DisplayStar;
        windows:          WindowStar;
        nwindows:         Int
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XRotateBuffers(
        display:          DisplayStar;
        rotate:           Int
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XRotateWindowProperties(
        display:          DisplayStar;
        w:                Window;
        properties:       AtomStar;
        num:              Int;
        npositions:       Int
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XScreenCount(
        display:          DisplayStar
): Int RAISES {Error};

<*EXTERNAL*> PROCEDURE XSelectInput(
        display:          DisplayStar;
        w:                Window;
        event:            Long
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XSendEvent(
        display:          DisplayStar;
        w:                Window;
        propagate:        Bool;
        event_mask:       Long;
        event_send:       XEventStar
): Status RAISES {Error};

<*EXTERNAL*> PROCEDURE XSetAccessControl(
        display:          DisplayStar;
        mode:             Int
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XSetArcMode(
        display:          DisplayStar;
        gc:               GC;
        arc:              Int
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XSetBackground(
        display:          DisplayStar;
        gc:               GC;
        background:       unsigned_long
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XSetClipMask(
        display:          DisplayStar;
        gc:               GC;
        pixmap:           Pixmap
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XSetClipOrigin(
        display:          DisplayStar;
        gc:               GC;
        clip_x:           Int;
        clip_y:           Int
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XSetClipRectangles(
        display:          DisplayStar;
        gc:               GC;
        clip_x:           Int;
        clip_y:           Int;
        rectangles:       XRectangleStar;
        n:                Int;
        ordering:         Int
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XSetCloseDownMode(
        display:          DisplayStar;
        close:            Int
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XSetCommand(
        display:          DisplayStar;
        w:                Window;
        argv:             Argv;
        argc:             Int
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XSetDashes(
        display:          DisplayStar;
        gc:               GC;
        dash_offset:      Int;
        dash_list:        const_char_star;
        n:                Int
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XSetFillRule(
        display:          DisplayStar;
        gc:               GC;
        fill:             Int
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XSetFillStyle(
        display:          DisplayStar;
        gc:               GC;
        fill:             Int
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XSetFont(
        display:          DisplayStar;
        gc:               GC;
        font:             Font
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XSetFontPath(
        display:          DisplayStar;
        directories:      char_star_star;
        ndirs:            Int
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XSetForeground(
        display:          DisplayStar;
        gc:               GC;
        foreground:       unsigned_long
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XSetFunction(
        display:          DisplayStar;
        gc:               GC;
        function:         Int
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XSetGraphicsExposures(
        display:          DisplayStar;
        gc:               GC;
        graphics:         Bool
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XSetIconName(
        display:          DisplayStar;
        w:                Window;
        icon:             const_char_star
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XSetInputFocus(
        display:          DisplayStar;
        focus:            Window;
        revert_to:        Int;
        time:             Time
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XSetLineAttributes(
        display:          DisplayStar;
        gc:               GC;
        line_width:       unsigned_int;
        line_style:       Int;
        cap_style:        Int;
        join_style:       Int
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XSetModifierMapping(
        display:          DisplayStar;
        modmap:           XModifierKeymapStar
): Int RAISES {Error};

<*EXTERNAL*> PROCEDURE XSetPlaneMask(
        display:          DisplayStar;
        gc:               GC;
        plane:            unsigned_long
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XSetPointerMapping(
        display:          DisplayStar;
        map:              const_unsigned_char_star;
        nmap:             Int
): Int RAISES {Error};

<*EXTERNAL*> PROCEDURE XSetScreenSaver(
        display:          DisplayStar;
        timeout:          Int;
        interval:         Int;
        prefer:           Int;
        allow:            Int
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XSetSelectionOwner(
        display:          DisplayStar;
        selection:        Atom;
        owner:            Window;
        time:             Time
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XSetState(
        display:          DisplayStar;
        gc:               GC;
        foreground:       unsigned_long;
        background:       unsigned_long;
        function:         Int;
        plane:            unsigned_long
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XSetStipple(
        display:          DisplayStar;
        gc:               GC;
        stipple:          Pixmap
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XSetSubwindowMode(
        display:          DisplayStar;
        gc:               GC;
        subwindow:        Int
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XSetTSOrigin(
        display:          DisplayStar;
        gc:               GC;
        ts_x_origin:      Int;
        ts_y_origin:      Int
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XSetTile(
        display:          DisplayStar;
        gc:               GC;
        tile:             Pixmap
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XSetWindowBackground(
        display:          DisplayStar;
        w:                Window;
        background:       unsigned_long
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XSetWindowBackgroundPixmap(
        display:          DisplayStar;
        w:                Window;
        background:       Pixmap
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XSetWindowBorder(
        display:          DisplayStar;
        w:                Window;
        border:           unsigned_long
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XSetWindowBorderPixmap(
        display:          DisplayStar;
        w:                Window;
        border:           Pixmap
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XSetWindowBorderWidth(
        display:          DisplayStar;
        w:                Window;
        width:            unsigned_int
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XSetWindowColormap(
        display:          DisplayStar;
        w:                Window;
        colormap:         Colormap
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XStoreBuffer(
        display:          DisplayStar;
        bytes:            const_char_star;
        nbytes:           Int;
        buffer:           Int
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XStoreBytes(
        display:          DisplayStar;
        bytes:            const_char_star;
        nbytes:           Int
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XStoreColor(
        display:          DisplayStar;
        colormap:         Colormap;
        color:            XColorStar
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XStoreColors(
        display:          DisplayStar;
        colormap:         Colormap;
        color:            XColorStar;
        ncolors:          Int
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XStoreName(
        display:          DisplayStar;
        w:                Window;
        window:           const_char_star
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XStoreNamedColor(
        display:          DisplayStar;
        colormap:         Colormap;
        color:            const_char_star;
        pixel:            unsigned_long;
        flags:            Int
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XSync(
        display:          DisplayStar;
        discard:          Bool
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XTextExtents(
        font:             XFontStructStar;
        string:           const_char_star;
        nchars:           Int;
        direction_ret:    int_star;
        font_ascent_ret:  int_star;
        font_descent_ret: int_star;
        overall_ret:      XCharStructStar
);

<*EXTERNAL*> PROCEDURE XTextExtents16(
        font:             XFontStructStar;
        string:           XChar2bStar;
        nchars:           Int;
        direction:        int_star;
        font_ascent:      int_star;
        font_descent:     int_star;
        overall:          XCharStructStar
);

<*EXTERNAL*> PROCEDURE XTextWidth(
        font:             XFontStructStar;
        string:           const_char_star;
        count:            Int
): Int RAISES {Error};

<*EXTERNAL*> PROCEDURE XTextWidth16(
        font:             XFontStructStar;
        string:           XChar2bStar;
        count:            Int
): Int RAISES {Error};

<*EXTERNAL*> PROCEDURE XTranslateCoordinates(
        display:          DisplayStar;
        src_w:            Window;
        dest_w:           Window;
        src_x:            Int;
        src_y:            Int;
        dest_x_return:    int_star;
        dest_y_return:    int_star;
        child_return:     WindowStar
): Bool RAISES {Error};

<*EXTERNAL*> PROCEDURE XUndefineCursor(
        display:          DisplayStar;
        w:                Window
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XUngrabButton(
        display:          DisplayStar;
        button:           unsigned_int;
        modifiers:        unsigned_int;
        grab:             Window
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XUngrabKey(
        display:          DisplayStar;
        keycode:          Int;
        modifiers:        unsigned_int;
        grab:             Window
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XUngrabKeyboard(
        display:          DisplayStar;
        time:             Time
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XUngrabPointer(
        display:          DisplayStar;
        time:             Time
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XUngrabServer(
        display:          DisplayStar
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XUninstallColormap(
        display:          DisplayStar;
        colormap:         Colormap
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XUnloadFont(
        display:          DisplayStar;
        font:             Font
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XUnmapSubwindows(
        display:          DisplayStar;
        w:                Window
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XUnmapWindow(
        display:          DisplayStar;
        w:                Window
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XVendorRelease(
        display:          DisplayStar
): Int RAISES {Error};

<*EXTERNAL*> PROCEDURE XWarpPointer(
        display:          DisplayStar;
        src_w:            Window;
        dest_w:           Window;
        src_x:            Int;
        src_y:            Int;
        src_width:        unsigned_int;
        src_height:       unsigned_int;
        dest_x:           Int;
        dest_y:           Int
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XWidthMMOfScreen(
        screen:           ScreenStar
): Int RAISES {Error};

<*EXTERNAL*> PROCEDURE XWidthOfScreen(
        screen:           ScreenStar
): Int RAISES {Error};

<*EXTERNAL*> PROCEDURE  XWindowEvent(
        display:          DisplayStar;
        w:                Window;
        event_mask:       Long;
        event_return:     XEventStar
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XWriteBitmapFile(
        display:          DisplayStar;
        filename:         char_star;
        bitmap:           Pixmap;
        width:            unsigned_int;
        height:           unsigned_int;
        x_hot:            Int;
        y_hot:            Int
): Int RAISES {Error};

(***************************************************************

                /usr/include/X11/Xutil.h

/* $XConsortium: Xutil.h,v 11.58 89/12/12 20:15:40 jim Exp $ */
 ***************************************************************)


(*
 * Bitmask returned by XParseGeometry().  Each bit tells if the corresponding
 * value (x, y, width, height) was found in the parsed string.
 *)

CONST
  NoValue =     16_0000;
  XValue =      16_0001;
  YValue =      16_0002;
  WidthValue =  16_0004;
  HeightValue = 16_0008;
  AllValues =   16_000F;
  XNegative =   16_0010;
  YNegative =   16_0020;

(*
 * new version containing base_width, base_height, and win_gravity fields;
 * used with WM_NORMAL_HINTS.
 *)

TYPE
  XSizeHints = RECORD
    flags: Long;    (* marks which fields in this structure are defined *)
    x, y: Int;      (* obsolete for new window mgrs, but clients *)
    width, height: Int; (* should set so old wm's don't mess up *)
    min_width, min_height: Int;
    max_width, max_height: Int;
    width_inc, height_inc: Int;
    min_aspect, max_aspect: RECORD
        x: Int; (* numerator *)
        y: Int; (* denominator *)
    END;
    base_width, base_height: Int;   (* added by ICCCM version 1 *)
    win_gravity: Int;               (* added by ICCCM version 1 *)
  END;
  XSizeHintsStar = UNTRACED REF XSizeHints;


(*
 * The next block of definitions are for window manager properties that
 * clients and applications use for communication.
 *)

(* flags argument in size hints *)
CONST
  USPosition =  Word.Shift(1, 0); (* user specified x, y *)
  USSize =  Word.Shift(1, 1); (* user specified width, height *)

  PPosition =   Word.Shift(1, 2); (* program specified position *)
  PSize     =   Word.Shift(1, 3); (* program specified size *)
  PMinSize  =   Word.Shift(1, 4); (* program specified minimum size *)
  PMaxSize  =   Word.Shift(1, 5); (* program specified maximum size *)
  PResizeInc =  Word.Shift(1, 6); (* program specified resize increments *)
  PAspect   =   Word.Shift(1, 7); (* program specified min and max aspect ratios *)
  PBaseSize =   Word.Shift(1, 8); (* program specified base for incrementing *)
  PWinGravity = Word.Shift(1, 9); (* program specified window gravity *)

(*?!?!?!
(* obsolete *)
#define PAllHints (PPosition|PSize|PMinSize|PMaxSize|PResizeInc|PAspect)
?!?!?!?!*)

TYPE
  XWMHints = RECORD
    flags: Long;    (* marks which fields in this structure are defined *)
    input: Bool;    (* does this application rely on the window manager to
                        get keyboard input? *)
    initial_state: Int; (* see below *)
    icon_pixmap: Pixmap;    (* pixmap to be used as icon *)
    icon_window: Window;    (* window to be used as icon *)
    icon_x, icon_y: Int;    (* initial position of icon *)
    icon_mask: Pixmap;      (* icon mask bitmap *)
    window_group: XID;      (* id of related window group *)
    (* this structure may be extended in the future *)
  END;
  XWMHintsStar = UNTRACED REF XWMHints;

(* definition for flags of XWMHints *)

CONST
  InputHint        = Word.Shift(1, 0);
  StateHint        = Word.Shift(1, 1);
  IconPixmapHint   = Word.Shift(1, 2);
  IconWindowHint   = Word.Shift(1, 3);
  IconPositionHint = Word.Shift(1, 4);
  IconMaskHint     = Word.Shift(1, 5);
  WindowGroupHint  = Word.Shift(1, 6);
  AllHints         = Word.Or(InputHint, Word.Or(StateHint, Word.Or(IconPixmapHint, Word.Or(IconWindowHint, Word.Or(IconPositionHint, Word.Or(IconMaskHint, WindowGroupHint))))));

(* definitions for initial window state *)
  WithdrawnState = 0;   (* for windows that are not mapped *)
  NormalState = 1;      (* most applications want to start this way *)
  IconicState = 3;      (* application wants to start as an icon *)

(*
 * Obsolete states no longer defined by ICCCM
 *)
  DontCareState = 0;    (* don't know or care *)
  ZoomState = 2;        (* application wants to start zoomed *)
  InactiveState = 4;    (* application believes it is seldom used; *)
                        (* some wm's may put it on inactive menu *)


(*
 * new structure for manipulating TEXT properties; used with WM_NAME,
 * WM_ICON_NAME, WM_CLIENT_MACHINE, and WM_COMMAND.
 *)
TYPE
  XTextProperty = RECORD
    value: unsigned_char_star;  (* same as Property routines *)
    encoding: Atom;             (* prop type *)
    format: Int;                (* prop data format: 8, 16, or 32 *)
    nitems: unsigned_long;      (* number of data items in value *)
  END;
  XTextPropertyStar = UNTRACED REF XTextProperty;

  XIconSize = RECORD
    min_width, min_height: Int;
    max_width, max_height: Int;
   width_inc, height_inc: Int
  END;
  XIconSizeStar     = UNTRACED REF XIconSize;
  XIconSizeStarStar = UNTRACED REF XIconSizeStar;

  XClassHint = RECORD
    res_name: char_star;
    res_class: char_star
  END;

  XClassHint_star   = UNTRACED REF XClassHint;


(* ?!?!?!
(*
 * These macros are used to give some sugar to the image routines so that
 * naive people are more comfortable with them.
 *)
#define XDestroyImage(ximage) \
        (( *((ximage)->f.destroy_image))((ximage)))
#define XGetPixel(ximage, x, y) \
        (( *((ximage)->f.get_pixel))((ximage), (x), (y)))
#define XPutPixel(ximage, x, y, pixel) \
        (( *((ximage)->f.put_pixel))((ximage), (x), (y), (pixel)))
#define XSubImage(ximage, x, y, width, height)  \
        (( *((ximage)->f.sub_image))((ximage), (x), (y), (width), (height)))
#define XAddPixel(ximage, value) \
        (( *((ximage)->f.add_pixel))((ximage), (value)))

?!?!?! *)

(*
 * Compose sequence status structure, used in calling XLookupString.
 *)

TYPE
  XComposeStatus = RECORD
    compose_ptr: char_star; (* state table pointer *)
    chars_matched: Int;     (* match state *)
  END;
  XComposeStatusStar = UNTRACED REF XComposeStatus;

(* ?!?!?!
(*
 * Keysym macros, used on Keysyms to test for classes of symbols
 *)
#define IsKeypadKey(keysym) \
  (((unsigned)(keysym) >= XK_KP_Space) && ((unsigned)(keysym) <= XK_KP_Equal))

#define IsCursorKey(keysym) \
  (((unsigned)(keysym) >= XK_Home)     && ((unsigned)(keysym) <  XK_Select))

#define IsPFKey(keysym) \
  (((unsigned)(keysym) >= XK_KP_F1)     && ((unsigned)(keysym) <= XK_KP_F4))

#define IsFunctionKey(keysym) \
  (((unsigned)(keysym) >= XK_F1)       && ((unsigned)(keysym) <= XK_F35))

#define IsMiscFunctionKey(keysym) \
  (((unsigned)(keysym) >= XK_Select)   && ((unsigned)(keysym) <  XK_KP_Space))

#define IsModifierKey(keysym) \
  (((unsigned)(keysym) >= XK_Shift_L)  && ((unsigned)(keysym) <= XK_Hyper_R))

(*
 * opaque reference to Region data type
 *)
typedef struct _XRegion *Region;
?!?!? *)

TYPE
  Region = ADDRESS;

(* Return values from XRectInRegion() *)

CONST
  RectangleOut = 0;
  RectangleIn =  1;
  RectanglePart = 2;


(*
 * Information used by the visual utility routines to find desired visual
 * type from the many visuals a display may support.
 *)

TYPE
  XVisualInfo = RECORD
    visual: VisualStar;
    visualid: VisualID;
    screen: Int;
    depth: Int;
    class: Int;
    red_mask: unsigned_long;
    green_mask: unsigned_long;
    blue_mask: unsigned_long;
    colormap_size: Int;
    bits_per_rgb: Int;
  END;
  XVisualInfoStar = UNTRACED REF XVisualInfo;

CONST
  VisualNoMask          =   16_0;
  VisualIDMask          =   16_1;
  VisualScreenMask      =   16_2;
  VisualDepthMask       =   16_4;
  VisualClassMask       =   16_8;
  VisualRedMaskMask     =   16_10;
  VisualGreenMaskMask   =   16_20;
  VisualBlueMaskMask    =   16_40;
  VisualColormapSizeMask =  16_80;
  VisualBitsPerRGBMask  =   16_100;
  VisualAllMask         =   16_1FF;

(*
 * This defines a window manager property that clients may use to
 * share standard color maps of type RGB_COLOR_MAP:
 *)

TYPE
  XStandardColormap = RECORD
    colormap: Colormap;
    red_max: unsigned_long;
    red_mult: unsigned_long;
    green_max: unsigned_long;
    green_mult: unsigned_long;
    blue_max: unsigned_long;
    blue_mult: unsigned_long;
    base_pixel: unsigned_long;
    visualid: VisualID;     (* added by ICCCM version 1 *)
    killid: XID;            (* added by ICCCM version 1 *)
  END;
  XStandardColormapStar   = UNTRACED REF XStandardColormap;
  XStandardColormapStarStar   = UNTRACED REF XStandardColormapStar;

CONST
  ReleaseByFreeingColormap: XID = 1;  (* for killid field above *)

(*
 * return codes for XReadBitmapFile and XWriteBitmapFile
 *)

CONST
  BitmapSuccess     = 0;
  BitmapOpenFailed  = 1;
  BitmapFileInvalid = 2;
  BitmapNoMemory    = 3;

(*
 * Declare the routines that don't return int.
 *)

(****************************************************************
 *
 * Context Management
 *
 ****************************************************************)


(* Associative lookup table return codes *)

CONST
  XCSUCCESS = 0;    (* No error. *)
  XCNOMEM =   1;    (* Out of memory *)
  XCNOENT =   2;    (* No entry in table *)

TYPE
  XContext = Int;

(* ?!?!?!?
#define XUniqueContext()       ((XContext) XrmUniqueQuark())
#define XStringToContext(string)   ((XContext) XrmStringToQuark(string))
?!?!?! *)

<*EXTERNAL*> PROCEDURE XSaveContext (
        display:                    DisplayStar;
        w:                          Window;
        context:                    XContext;
        data:                       ADDRESS
): Int RAISES {Error};

<*EXTERNAL*> PROCEDURE XFindContext (
        display:                    DisplayStar;
        w:                          Window;
        context:                    XContext;
        data_return:                ADDRESS
): Int RAISES {Error};

<*EXTERNAL*> PROCEDURE XDeleteContext (
        display:                    DisplayStar;
        w:                          Window;
        context:                    XContext
): Int RAISES {Error};


<*EXTERNAL*> PROCEDURE XGetWMHints (
        display:                    DisplayStar;
        w:                          Window
): XWMHintsStar RAISES {Error};

<*EXTERNAL*> PROCEDURE XCreateRegion (): Region;

<*EXTERNAL*> PROCEDURE XPolygonRegion (
        points:                     XPoint_star;
        n:                          Int;
        fill_rule:                  Int
): Region;

<*EXTERNAL*> PROCEDURE XGetVisualInfo (
        display:                    DisplayStar;
        vinfo_mask:                 Long;
        vinfo_template:             XVisualInfoStar;
        nitems_return:              int_star
): XVisualInfoStar RAISES {Error};

(* Allocation routines for properties that may get longer *)

<*EXTERNAL*> PROCEDURE XAllocSizeHints  (): XSizeHintsStar;

<*EXTERNAL*> PROCEDURE XAllocStandardColormap (): XStandardColormapStar;

<*EXTERNAL*> PROCEDURE XAllocWMHints  (): XWMHintsStar;

<*EXTERNAL*> PROCEDURE XAllocClassHint  (): XClassHint_star;

<*EXTERNAL*> PROCEDURE XAllocIconSize  (): XIconSizeStar;

(* ICCCM routines for data structures defined in this file *)
<*EXTERNAL*> PROCEDURE XGetWMSizeHints (
        display:                    DisplayStar;
        w:                          Window;
        hints_return:               XSizeHintsStar;
        supplied_return:            long_star;
        property:                   Atom
): Status RAISES {Error};

<*EXTERNAL*> PROCEDURE XGetWMNormalHints (
        display:                    DisplayStar;
        w:                          Window;
        hints_return:               XSizeHintsStar;
        supplied_return:            long_star
): Status RAISES {Error};

<*EXTERNAL*> PROCEDURE XGetRGBColormaps (
        display:                    DisplayStar;
        w:                          Window;
        stdcmap_return:             XStandardColormapStarStar;
        count_return:               int_star;
        property:                   Atom
): Status RAISES {Error};

<*EXTERNAL*> PROCEDURE XGetTextProperty (
        display:                    DisplayStar;
        window:                     Window;
        text_prop_return:           XTextPropertyStar;
        property:                   Atom
): Status RAISES {Error};

<*EXTERNAL*> PROCEDURE XGetWMName (
        display:                    DisplayStar;
        w:                          Window;
        text_prop_return:           XTextPropertyStar
): Status RAISES {Error};

<*EXTERNAL*> PROCEDURE XGetWMIconName (
        display:                    DisplayStar;
        w:                          Window;
        text_prop_return:           XTextPropertyStar
): Status RAISES {Error};

<*EXTERNAL*> PROCEDURE XGetWMClientMachine (
        display:                    DisplayStar;
        w:                          Window;
        text_prop_return:           XTextPropertyStar
): Status RAISES {Error};

<*EXTERNAL*> PROCEDURE XSetWMProperties (
        display:                    DisplayStar;
        w:                          Window;
        window_name:                XTextPropertyStar;
        icon_name:                  XTextPropertyStar;
        argv:                       Argv;
        argc:                       Int;
        normal_hints:               XSizeHintsStar;
        wm_hints:                   XWMHintsStar;
        class_hints:                XClassHint_star
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XSetWMSizeHints (
        display:                    DisplayStar;
        w:                          Window;
        hints:                      XSizeHintsStar;
        property:                   Atom
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XSetWMNormalHints (
        display:                    DisplayStar;
        w:                          Window;
        hints:                      XSizeHintsStar
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XSetRGBColormaps (
        display:                    DisplayStar;
        w:                          Window;
        stdcmaps:                   XStandardColormapStar;
        count:                      Int;
        property:                   Atom
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XSetTextProperty (
        display:                    DisplayStar;
        w:                          Window;
        text_prop:                  XTextPropertyStar;
        property:                   Atom
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XSetWMName (
        display:                    DisplayStar;
        w:                          Window;
        text_prop:                  XTextPropertyStar
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XSetWMIconName (
        display:                    DisplayStar;
        w:                          Window;
        text_prop:                  XTextPropertyStar
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XSetWMClientMachine (
        display:                    DisplayStar;
        w:                          Window;
        text_prop:                  XTextPropertyStar
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XStringListToTextProperty (
        list:                       char_star_star;
        count:                      Int;
        text_prop_return:           XTextPropertyStar
): Status RAISES {Error};

<*EXTERNAL*> PROCEDURE XTextPropertyToStringList (
        text_prop:                  XTextPropertyStar;
        list_return:                char_star_star_star;
        count_return:               int_star
): Status RAISES {Error};

(* The following declarations are alphabetized. *)

<*EXTERNAL*> PROCEDURE XClipBox (
        r:                          Region;
        rect_return:                XRectangleStar
);

<*EXTERNAL*> PROCEDURE XDestroyRegion (
        r:                          Region
);

<*EXTERNAL*> PROCEDURE XEmptyRegion (
        r:                          Region
);

<*EXTERNAL*> PROCEDURE XEqualRegion (
        r1:                         Region;
        r2:                         Region
);

<*EXTERNAL*> PROCEDURE XGetClassHint (
        display:                    DisplayStar;
        w:                          Window;
        class_hints_return:         XClassHint_star
): Status RAISES {Error};

<*EXTERNAL*> PROCEDURE XGetIconSizes (
        display:                    DisplayStar;
        w:                          Window;
        size_list_return:           XIconSizeStarStar;
        count_return:               int_star
): Status RAISES {Error};

<*EXTERNAL*> PROCEDURE XGetNormalHints (
        display:                    DisplayStar;
        w:                          Window;
        hints_return:               XSizeHintsStar
): Status RAISES {Error};

<*EXTERNAL*> PROCEDURE XGetSizeHints (
        display:                    DisplayStar;
        w:                          Window;
        hints_return:               XSizeHintsStar;
        property:                   Atom
): Status RAISES {Error};

<*EXTERNAL*> PROCEDURE XGetStandardColormap (
        display:                    DisplayStar;
        w:                          Window;
        colormap_return:            XStandardColormapStar;
        property:                   Atom
): Status RAISES {Error};

<*EXTERNAL*> PROCEDURE XGetZoomHints (
        display:                    DisplayStar;
        w:                          Window;
        zhints_return:              XSizeHintsStar
): Status RAISES {Error};

<*EXTERNAL*> PROCEDURE XIntersectRegion (
        sra:                        Region;
        srb:                        Region;
        dr_return:                  Region
);

<*EXTERNAL*> PROCEDURE XLookupString (
        event_struct:               XKeyEventStar;
        buffer_return:              char_star;
        bytes_buffer:               Int;
        keysym_return:              KeySymStar;
        status_in_out:              XComposeStatusStar
): Int;

<*EXTERNAL*> PROCEDURE XMatchVisualInfo (
        display:                    DisplayStar;
        screen:                     Int;
        depth:                      Int;
        class:                      Int;
        vinfo_return:               XVisualInfoStar
): Status;

<*EXTERNAL*> PROCEDURE XOffsetRegion (
        r:                          Region;
        dx:                         Int;
        dy:                         Int
);

<*EXTERNAL*> PROCEDURE XPointInRegion (
        r:                          Region;
        x:                          Int;
        y:                          Int
): Bool RAISES {Error};

<*EXTERNAL*> PROCEDURE XRectInRegion (
        r:                          Region;
        x:                          Int;
        y:                          Int;
        width:                      unsigned_int;
        height:                     unsigned_int
): Int;

<*EXTERNAL*> PROCEDURE XSetClassHint (
        display:                    DisplayStar;
        w:                          Window;
        class_hints:                XClassHint_star
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XSetIconSizes (
        display:                    DisplayStar;
        w:                          Window;
        size_list:                  XIconSizeStar;
        count:                      Int
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XSetNormalHints (
        display:                    DisplayStar;
        w:                          Window;
        hints:                      XSizeHintsStar
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XSetSizeHints (
        display:                    DisplayStar;
        w:                          Window;
        hints:                      XSizeHintsStar;
        property:                   Atom
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XSetStandardProperties (
        display:                    DisplayStar;
        w:                          Window;
        window_name:                const_char_star;
        icon_name:                  const_char_star;
        icon_pixmap:                Pixmap;
        argv:                       Argv;
        argc:                       Int;
        hints:                      XSizeHintsStar
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XSetWMHints (
        display:                    DisplayStar;
        w:                          Window;
        wm_hints:                   XWMHintsStar
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XSetRegion (
        display:                    DisplayStar;
        gc:                         GC;
        r:                          Region
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XSetStandardColormap (
        display:                    DisplayStar;
        w:                          Window;
        colormap:                   XStandardColormapStar;
        property:                   Atom
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XSetZoomHints (
        display:                    DisplayStar;
        w:                          Window;
        zhints:                     XSizeHintsStar
) RAISES {Error};

<*EXTERNAL*> PROCEDURE XShrinkRegion (
        r:                          Region;
        dx:                         Int;
        dy:                         Int
);

<*EXTERNAL*> PROCEDURE XSubtractRegion (
        sra:                        Region;
        srb:                        Region;
        dr_return:                  Region
);

<*EXTERNAL*> PROCEDURE XUnionRectWithRegion (
        rectangle:                  XRectangleStar;
        src_region:                 Region;
        dest_region_return:         Region
);

<*EXTERNAL*> PROCEDURE XUnionRegion (
        sra:                        Region;
        srb:                        Region;
        dr_return:                  Region
);

<*EXTERNAL*> PROCEDURE XWMGeometry (
        display:                    DisplayStar;
        screen_number:              Int;
        user_geometry:              const_char_star;
        default_geometry:           const_char_star;
        border_width:               unsigned_int;
        hints:                      XSizeHintsStar;
        x_return:                   int_star;
        y_return:                   int_star;
        width_return:               int_star;
        height_return:              int_star;
        gravity_return:             int_star
): Int RAISES {Error};

<*EXTERNAL*> PROCEDURE XXorRegion (
        sra:                        Region;
        srb:                        Region;
        dr_return:                  Region
);

<*EXTERNAL*> PROCEDURE XInitThreads ( );
<*EXTERNAL*> PROCEDURE XLockDisplay (display : DisplayStar);
<*EXTERNAL*> PROCEDURE XUnLockDisplay (display : DisplayStar);

END X.




