(* Copyright (C) 1989, 1990 Digital Equipment Corporation	*)
(* All rights reserved.						*)

(* See the file COPYRIGHT for a full description.		*)
(* File: Xmu.i3							*)
(* Last modified on Fri May  7 16:10:27 PDT 1993 by mjordan     *)
(*      modified on Fri Feb 28 11:18:24 PST 1992 by kalsow      *)
(*      modified on Thu Mar 14 02:37:30 1991 by muller          *)
(*      modified on Fri Feb 23 15:26:28 1990 by jerome          *)

UNSAFE INTERFACE Xmu;

(*==============================================================*)
(*	The X11 R4 Interface for Modula 3			*)
(*								*)
(*	The Miscellaneous Utility Library			*)
(*	contains:						*)
(*								*)
(*			../include/Xmu/Atoms.h			*)
(*			../include/Xmu/CharSet.h		*)
(*			../include/Xmu/CloseHook.h		*)
(*			../include/Xmu/Converters.h		*)
(*			../include/Xmu/CurUtil.h		*)
(*			../include/Xmu/CvtCache.h		*)
(*			../include/Xmu/DisplayQue.h		*)
(*			../include/Xmu/Drawing.h		*)
(*			../include/Xmu/Error.h			*)
(*			../include/Xmu/Initer.h			*)
(*			../include/Xmu/Misc.h			*)
(*			../include/Xmu/StdCmap.h		*)
(*			../include/Xmu/StdSel.h			*)
(*			../include/Xmu/SysUtil.h		*)
(*			../include/Xmu/WinUtil.h		*)
(*			../include/Xmu/Xct.h			*)
(*			../include/Xmu/Xmu.h			*)
(*==============================================================*)

(*
 *
 * Copyright 1988 by the Massachusetts Institute of Technology
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
 * The interfaces described by this header file are for miscellaneous utilities
 * and are not part of the Xlib standard.
 *)

FROM  Ctypes IMPORT char_star, char_star_star, int, int_star, 
                    unsigned_int, unsigned_int_star, 
                    unsigned_long, unsigned_long_star;
FROM  X      IMPORT Atom, AtomStar, Bool, Colormap,
                    DisplayStar, Drawable, Enumeration, GC,
                    Pixmap, ScreenStar, Status, 
                    Time, VisualID, Window;
FROM  Xt     IMPORT AppContext, Boolean, Cardinal, CardinalStar, Pixel, 
                    StringStar, Widget;

IMPORT X,Xrm;

TYPE Int = int;

(*======================================================================
 * $XConsortium: Atoms.h,v 1.3 89/12/08 12:03:56 rws Exp $
 *======================================================================*)

TYPE
  AtomPtr  =  AtomStar;

<* EXTERNAL "_XA_ATOM_PAIR" *>
   VAR XA_ATOM_PAIR: AtomPtr;

<* EXTERNAL "_XA_CHARACTER_POSITION" *>
   VAR XA_CHARACTER_POSITION: AtomPtr;

<* EXTERNAL "_XA_CLASS" *>
   VAR XA_CLASS: AtomPtr;

<* EXTERNAL "_XA_CLIENT_WINDOW" *>
   VAR XA_CLIENT_WINDOW: AtomPtr;

<* EXTERNAL "_XA_CLIPBOARD" *>
   VAR XA_CLIPBOARD: AtomPtr;

<* EXTERNAL "_XA_COMPOUND_TEXT" *>
   VAR XA_COMPOUND_TEXT: AtomPtr;

<* EXTERNAL "_XA_DECNET_ADDRESS" *>
   VAR XA_DECNET_ADDRESS: AtomPtr;

<* EXTERNAL "_XA_DELETE" *>
   VAR XA_DELETE: AtomPtr;

<* EXTERNAL "_XA_FILENAME" *>
   VAR XA_FILENAME: AtomPtr;

<* EXTERNAL "_XA_HOSTNAME" *>
   VAR XA_HOSTNAME: AtomPtr;

<* EXTERNAL "_XA_IP_ADDRESS" *>
   VAR XA_IP_ADDRESS: AtomPtr;

<* EXTERNAL "_XA_LENGTH" *>
   VAR XA_LENGTH: AtomPtr;

<* EXTERNAL "_XA_LIST_LENGTH" *>
   VAR XA_LIST_LENGTH: AtomPtr;

<* EXTERNAL "_XA_NAME" *>
   VAR XA_NAME: AtomPtr;

<* EXTERNAL "_XA_NET_ADDRESS" *>
   VAR XA_NET_ADDRESS: AtomPtr;

<* EXTERNAL "_XA_NULL" *>
   VAR XA_NULL: AtomPtr;

<* EXTERNAL "_XA_OWNER_OS" *>
   VAR XA_OWNER_OS: AtomPtr;

<* EXTERNAL "_XA_SPAN" *>
   VAR XA_SPAN: AtomPtr;

<* EXTERNAL "_XA_TARGETS" *>
   VAR XA_TARGETS: AtomPtr;

<* EXTERNAL "_XA_TEXT" *>
   VAR XA_TEXT: AtomPtr;

<* EXTERNAL "_XA_TIMESTAMP" *>
   VAR XA_TIMESTAMP: AtomPtr;

<* EXTERNAL "_XA_USER" *>
    VAR XA_USER: AtomPtr;

<* EXTERNAL XmuMakeAtom *>
   PROCEDURE MakeAtom (name: char_star): AtomPtr;

<* EXTERNAL XmuInternAtom *>
   PROCEDURE InternAtom (d: DisplayStar; atom_ptr: AtomPtr): Atom;

<* EXTERNAL XmuInternStrings *>
   PROCEDURE InternStrings (d: DisplayStar; names: StringStar;
                            count: Cardinal; atoms:  AtomStar);

<* EXTERNAL XmuGetAtomName *>
   PROCEDURE GetAtomName (d: DisplayStar; atom: Atom): char_star;

<* EXTERNAL XmuNameOfAtom *>
   PROCEDURE NameOfAtom (atom_ptr: AtomPtr): char_star;


(*======================================================================
 * $XConsortium: CharSet.h,v 1.2 89/09/22 15:33:14 jim Exp $
 *======================================================================*)

<* EXTERNAL XmuCopyISOLatin1Lowered *>
   PROCEDURE CopyISOLatin1Lowered (dst: char_star; src: char_star);

<* EXTERNAL XmuCopyISOLatin1Uppered *>
   PROCEDURE CopyISOLatin1Uppered (dst: char_star; src: char_star);

<* EXTERNAL XmuCompareISOLatin1 *>
   PROCEDURE CompareISOLatin1 (first: char_star; second: char_star): Int;


(*======================================================================
 * $XConsortium: CloseHook.h,v 1.1 89/07/14 17:51:53 jim Exp $
 *======================================================================*)

TYPE
   CloseHook  = ADDRESS;
   ProcCloseHook = PROCEDURE (d: DisplayStar; arg: ADDRESS): Int;

<* EXTERNAL XmuAddCloseDisplayHook *>
   PROCEDURE AddCloseDisplayHook (d: DisplayStar; 
                                  func: ProcCloseHook;
                                  arg: ADDRESS): Int;

<* EXTERNAL XmuRemoveCloseDisplayHook *>
   PROCEDURE AddRemoveDisplayHook (d: DisplayStar; 
                                   handle: CloseHook;
                                   func: ProcCloseHook;
                                   arg: ADDRESS): Int;

<* EXTERNAL XmuLookupCloseDisplayHook *>
   PROCEDURE AddLookupDisplayHook (d: DisplayStar; 
                                   handle: CloseHook;
                                   func: ProcCloseHook;
                                   arg: ADDRESS): Int;


(*======================================================================
 * $XConsortium: Converters.h,v 1.3 89/10/09 16:25:30 jim Exp $
 *======================================================================*)

(*
 * Converters - insert in alphabetical order
 *)

(******************************************************************************
 * XmuCvtFunctionToCallback
 *)
<* EXTERNAL XmuCvtFunctionToCallback *>
   PROCEDURE CvtFunctionToCallback (args: Xrm.ValueStar;
                                    num_args: CardinalStar;
                                    fromVal: Xrm.ValuePtr;
                                    toVal: Xrm.ValuePtr);

(******************************************************************************
 * XmuCvtStringToBackingStore
 *)

(* #define XtNbackingStore "backingStore"                      in XtN.i3 *)
(* #define XtCBackingStore "BackingStore"                      in XtC.i3 *)
(* #define XtRBackingStore "BackingStore"                      in XtR.i3 *)
(* #define XtEnotUseful "notUseful"                            in XtE.i3 *)
(* #define XtEwhenMapped "whenMapped"                          in XtE.i3 *)
(* #define XtEalways "always"                                  in XtE.i3 *)
(* #define XtEdefault "default"                                in XtE.i3 *)

<* EXTERNAL XmuCvtStringToBackingStore *>
   PROCEDURE CvtStringToBackingStore (args: Xrm.ValueStar;
                                      num_args: CardinalStar;
                                      fromVal: Xrm.ValuePtr;
                                      toVal: Xrm.ValuePtr);

(******************************************************************************
 * XmuCvtStringToCursor
 *)
<* EXTERNAL XmuCvtStringToCursor *>
   PROCEDURE CvtStringToCursor (args: Xrm.ValueStar;
                                num_args: CardinalStar;
                                fromVal: Xrm.ValuePtr;
                                toVal: Xrm.ValuePtr);

(******************************************************************************
 * XmuCvtStringToJustify
 *)

TYPE
  Justify	= Enumeration;
CONST
  JustifyLeft	= 0;       (* justify text to left side of button   *)
  JustifyCenter	= 1;       (* justify text in center of button      *)
  JustifyRight	= 2;       (* justify text to right side of button  *)

(* #define XtEleft "left"                                      in XtE.i3 *)
(* #define XtEcenter "center"                                  in XtE.i3 *)
(* #define XtEright "right"                                    in XtE.i3 *)

<* EXTERNAL XmuCvtStringToJustify *>
   PROCEDURE CvtStringToJustify (args: Xrm.ValueStar;
                                 num_args: CardinalStar;
                                 fromVal: Xrm.ValuePtr;
                                 toVal: Xrm.ValuePtr);

(******************************************************************************
 * XmuCvtStringToLong
 *)

(* #define XtRLong "Long"                                      in XtR.i3 *)

<* EXTERNAL XmuCvtStringToLong *>
   PROCEDURE CvtStringToLong (args: Xrm.ValueStar;
                              num_args: CardinalStar;
                              fromVal: Xrm.ValuePtr;
                              toVal: Xrm.ValuePtr);

(******************************************************************************
 * XmuCvtStringToOrientation
 *)

TYPE
 Orientation		= Enumeration;
CONST
  orientHorizontal	= 0;
  orientVertical	= 1;

<* EXTERNAL XmuCvtStringToOrientation *>
   PROCEDURE CvtStringToOrientation (args: Xrm.ValueStar;
                                     num_args: CardinalStar;
                                     fromVal: Xrm.ValuePtr;
                                     toVal: Xrm.ValuePtr);

(******************************************************************************
 * XmuCvtStringToBitmap
 *)
<* EXTERNAL XmuCvtStringToBitmap *>
   PROCEDURE CvtStringToBitmap (args: Xrm.ValueStar;
                                num_args: CardinalStar;
                                fromVal: Xrm.ValuePtr;
                                toVal: Xrm.ValuePtr);

(******************************************************************************
 * XmuCvtStringToShapeStyle; is XtTypeConverter (i.e. new style)
 * no conversion arguments, not particularly useful to cache the results.
 *)

(* #define XtRShapeStyle "ShapeStyle"                          in XtR.i3 *)
(* #define XtERectangle "Rectangle"                            in XtE.i3 *)
(* #define XtEOval "Oval"                                      in XtE.i3 *)
(* #define XtEEllipse "Ellipse"                                in XtE.i3 *)
(* #define XtERoundedRectangle "RoundedRectangle"              in XtE.i3 *)

CONST
   ShapeRectangle		= 1;
   ShapeOval			= 2;
   ShapeEllipse 		= 3;
   ShapeRoundedRectangle	= 4;

<* EXTERNAL XmuCvtStringToShapeStyle *>
   PROCEDURE CvtStringToShapeStyle (d: DisplayStar;
                                    args: Xrm.ValueStar;
                                    num_args: CardinalStar;
                                    fromVal: Xrm.ValuePtr;
                                    toVal: Xrm.ValuePtr;
                                    data: ADDRESS): Boolean;

<* EXTERNAL XmuReshapeWidget *>
   PROCEDURE ReshapeWidget (w: Widget;
                            shape_style: Int;
                            corner_width: Int;
                            corner_height: Int);

(*****************************************************************************
 * XmuCvtStringToWidget
 *)
<* EXTERNAL XmuCvtStringToWidget *>
   PROCEDURE CvtStringToWidget (args: Xrm.ValueStar;
                                num_args: CardinalStar;
                                fromVal: Xrm.ValuePtr;
                                toVal: Xrm.ValuePtr);


(*======================================================================
 * $XConsortium: CurUtil.h,v 1.1 89/07/19 15:40:17 jim Exp $
 *======================================================================*)

<* EXTERNAL XmuCursorNameToIndex *>
   PROCEDURE CursorNameToIndex (name: char_star): Int;


(*======================================================================
 * $XConsortium: CvtCache.h,v 1.3 89/11/30 18:21:42 rws Exp $
 *======================================================================*)

(* ?!?!?! not defined .....
typedef struct _XmuCvtCache {
    struct {
	char **bitmapFilePath;
    } string_to_bitmap;
    (* add other per-display data that needs to be cached *)
} XmuCvtCache;

<* EXTERNAL *_XmuCCLookupDisplay *>
   PROCEDURE muCCLookupDisplay (): XmuCvtCache;
?!?!?!?*)


(*======================================================================
 * $XConsortium: DisplayQue.h
 *======================================================================*)

(*
 *			      Public Entry Points
 * 
 * 
 * XmuDisplayQueue *XmuDQCreate (closefunc, freefunc, data)
 *     int ( *closefunc)();
 *     int ( *freefunc)();
 *     caddr_t data;
 * 
 *         Creates and returns a queue into which displays may be placed.  When
 *         the display is closed, the closefunc (if non-NULL) is upcalled with
 *         as follows:
 *
 *                 ( *closefunc) (queue, entry)
 *
 *         The freeproc, if non-NULL, is called whenever the last display is
 *         closed, notifying the creator that display queue may be released
 *         using XmuDQDestroy.
 *
 *
 * Bool XmuDQDestroy (q, docallbacks)
 *     XmuDisplayQueue *q;
 *     Bool docallbacks;
 * 
 *         Releases all memory for the indicated display queue.  If docallbacks
 *         is true, then the closefunc (if non-NULL) is called for each 
 *         display.
 * 
 * 
 * XmuDisplayQueueEntry *XmuDQLookupDisplay (q, dpy)
 *     XmuDisplayQueue *q;
 *     Display *dpy;
 *
 *         Returns the queue entry for the specified display or NULL if the
 *         display is not in the queue.
 *
 * 
 * XmuDisplayQueueEntry *XmuDQAddDisplay (q, dpy, data)
 *     XmuDisplayQueue *q;
 *     Display *dpy;
 *     caddr_t data;
 *
 *         Adds the indicated display to the end of the queue or NULL if it
 *         is unable to allocate memory.  The data field may be used by the
 *         caller to attach arbitrary data to this display in this queue.  The
 *         caller should use XmuDQLookupDisplay to make sure that the display
 *         hasn't already been added.
 * 
 * 
 * Bool XmuDQRemoveDisplay (q, dpy)
 *     XmuDisplayQueue *q;
 *     Display *dpy;
 *
 *         Removes the specified display from the given queue.  If the 
 *         indicated display is not found on this queue, False is returned,
 *         otherwise True is returned.
 *)

(* ?!?!?!?!?
typedef struct _XmuDisplayQueueEntry {
    struct _XmuDisplayQueueEntry *prev, *next;
    Display *display;
    CloseHook closehook;
    caddr_t data;
} XmuDisplayQueueEntry;

typedef struct _XmuDisplayQueue {
    int nentries;
    XmuDisplayQueueEntry *head, *tail;
    int ( *closefunc)();
    int ( *freefunc)();
    caddr_t data;
} XmuDisplayQueue;

<* EXTERNAL *XmuDQCreate *>
   PROCEDURE uDQCreate  (): XmuDisplayQueue;

<* EXTERNAL XmuDQDestroy *>
   PROCEDURE DQDestroy  (): Bool;

<* EXTERNAL *XmuDQLookupDisplay *>
   PROCEDURE uDQLookupDisplay  (): XmuDisplayQueueEntry;

<* EXTERNAL *XmuDQAddDisplay *>
   PROCEDURE uDQAddDisplay  (): XmuDisplayQueueEntry;

<* EXTERNAL XmuDQRemoveDisplay *>
   PROCEDURE DQRemoveDisplay  (): Bool;


#define XmuDQNDisplays(q) ((q)->nentries)
?!?!?!? *)


(*======================================================================
 * $XConsortium: Drawing.h,v 1.3 89/10/03 08:37:53 rws Exp $
 *======================================================================*)

<* EXTERNAL XmuDrawRoundedRectangle *>
   PROCEDURE DrawRoundedRectangle (dpy: DisplayStar;
                                   draw: Drawable;
                                   gc: GC;
                                   x, y, w, h, ew, eh: Int);

<* EXTERNAL XmuFillRoundedRectangle *>
   PROCEDURE FillRoundedRectangle (dpy: DisplayStar;
                                   draw: Drawable;
                                   gc: GC;
                                   x, y, w, h, ew, eh: Int);

<* EXTERNAL XmuDrawLogo *>
   PROCEDURE DrawLogo (dpy: DisplayStar;
                       draw: Drawable;
                       gcFore, gcBack: GC;
                       x, y: Int;
                       w, h: unsigned_int);

<* EXTERNAL XmuCreatePixmapFromBitmap *>
   PROCEDURE CreatePixmapFromBitmap (dpy: DisplayStar;
                                     draw: Drawable;
                                     bitmap: Pixmap;
                                     w, h, depth: unsigned_int;
                                     fore, back: unsigned_long): Pixmap;

<* EXTERNAL XmuCreateStippledPixmap *>
   PROCEDURE CreateStippledPixmap (screen: ScreenStar;
                                   fore, back: Pixel;
                                   depth: unsigned_int): Pixmap;

<* EXTERNAL XmuLocateBitmapFile *>
   PROCEDURE LocateBitmapFile (screen: ScreenStar;
                               name, srcname: char_star;
                               srcnamelen: Int;
                               w, h, xhotp, yhotp: int_star): Pixmap;

<* EXTERNAL XmuReadBitmapData *>
   PROCEDURE ReadBitmapData (stream: char_star;
                             w, h: unsigned_int_star;
                             datap: char_star_star;
                             x_hot, y_hot: int_star): Int;

<* EXTERNAL XmuReadBitmapDataFromFile *>
   PROCEDURE ReadBitmapDataFromFile (file: char_star;
                                     w, h: unsigned_int_star;
                                     datap: char_star_star;
                                     x_hot, y_hot: int_star): Int;


(*======================================================================
 * $XConsortium: Error.h,v 1.2 89/11/14 16:16:30 jim Exp $
 *======================================================================*)

<* EXTERNAL XmuPrintDefaultErrorMessage *>
   PROCEDURE PrintDefaultErrorMessage (dpy: DisplayStar;
                                       event: X.XErrorEventStar;
                                       fp: char_star): Int;

<* EXTERNAL XmuSimpleErrorHandler *>
   PROCEDURE SimpleErrorHandler (dpy: DisplayStar;
                                 errorp: X.XErrorEventStar): Int;


(*======================================================================
 * $XConsortium: Initer.h,v 1.1 89/07/14 17:51:55 jim Exp $
 *======================================================================*)

TYPE
  ProcIniter  =  PROCEDURE (app_con: AppContext; data: ADDRESS);

<* EXTERNAL XmuCallInitializers *>
   PROCEDURE CallInitializers (app_con: AppContext);

<* EXTERNAL XmuAddInitializer *>
   PROCEDURE AddInitializer (func: ProcIniter;
                             data: ADDRESS);


(*======================================================================
 * $XConsortium: Misc.h,v 1.1 89/05/10 16:00:25 jim Exp $
 *======================================================================*)

(* ?!?!?!?
#define MAXDIMENSION	((1 << 31)-1)
#define Max(x, y)	(((x) > (y)) ? (x) : (y))
#define Min(x, y)	(((x) < (y)) ? (x) : (y))
#define AssignMax(x, y)	{if ((y) > (x)) x = (y);}
#define AssignMin(x, y)	{if ((y) < (x)) x = (y);}
?!?!?!? *)


(*======================================================================
 * $XConsortium: StdCmap.h,v 1.1 89/07/14 17:51:56 jim Exp $
 *======================================================================*)

<* EXTERNAL XmuAllStandardColormaps *>
   PROCEDURE AllStandardColormaps (dpy: DisplayStar): Status;

<* EXTERNAL XmuCreateColormap *>
   PROCEDURE CreateColormap (dpy: DisplayStar;
                             colormap: X.XStandardColormapStar): Status;

<* EXTERNAL XmuDeleteStandardColormap *>
   PROCEDURE DeleteStandardColormap (dpy: DisplayStar;
                                     screen: Int;
                                     property: Atom);

<* EXTERNAL XmuGetColormapAllocation *>
   PROCEDURE GetColormapAllocation (vinfo: X.XVisualInfoStar;
                                    property: Atom;
                                    red_max, green_max, blue_max: 
                                              unsigned_long_star): Status;

<* EXTERNAL XmuLookupColormap *>
   PROCEDURE LookupColormap (dpy: DisplayStar;
                             screen: Int;
                             visualid: VisualID;
                             depth: unsigned_int;
                             property: Atom;
                             replace, retain: Bool): Status;

<* EXTERNAL XmuStandardColormap *>
   PROCEDURE StandardColormap (dpy: DisplayStar;
                               screen: Int;
                               visualid: VisualID;
                               depth: unsigned_int;
                               property: Atom;
                               cmap: Colormap;
                               red_max, green_max, blue_max: unsigned_long_star
): X.XStandardColormapStar;

<* EXTERNAL XmuVisualStandardColormaps *>
   PROCEDURE VisualStandardColormaps (dpy: DisplayStar;
                                      screen: Int;
                                      visualid: VisualID;
                                      depth: unsigned_int;
                                      property: Atom;
                                      replace, retain: Bool): Status;


(*======================================================================
 * $XConsortium: StdSel.h,v 1.1 89/07/14 17:51:56 jim Exp $
 *======================================================================*)

<* EXTERNAL XmuConvertStandardSelection *>
   PROCEDURE ConvertStandardSelection (w: Widget;
                                       time: Time;
                                       selection, target, type: AtomStar;
                                       value: ADDRESS;
                                       length: unsigned_long_star;
                                       format: int_star): Boolean;


(*======================================================================
 * $XConsortium: SysUtil.h,v 1.1 89/09/22 12:07:37 jim Exp $
 *======================================================================*)

<* EXTERNAL XmuGetHostname *>
   PROCEDURE GetHostname (buf: char_star; maxlen: Int): Int;


(*======================================================================
 * $XConsortium: WinUtil.h,v 1.3 89/09/22 12:10:20 jim Exp $
 *======================================================================*)

<* EXTERNAL XmuClientWindow *>
   PROCEDURE ClientWindow (dpy: DisplayStar; win: Window): Window;

<* EXTERNAL XmuUpdateMapHints *>
   PROCEDURE UpdateMapHints (dpy: DisplayStar; 
                             win: Window; 
                             hints: X.XSizeHintsStar): Bool;

<* EXTERNAL XmuScreenOfWindow *>
   PROCEDURE ScreenOfWindow (dpy: DisplayStar; w: Window): ScreenStar;


(*======================================================================
 * $XConsortium: Xmu.h,v 1.26 89/07/16 14:12:37 jim Exp $
 *======================================================================*)


END Xmu.
