(* Copyright (C) 1993, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* by Steve Glassman, Mark Manasse and Greg Nelson *)
(* Last modified on Thu Nov 11 10:50:49 PST 1993 by kalsow *)
(*      modified on Fri Nov  6 19:55:55 PST 1992 by msm    *)
(*      modified on Mon Feb 24 13:59:53 PST 1992 by muller *)
<*PRAGMA LL*>

UNSAFE INTERFACE XGC;

IMPORT X, XScreenType, PaintPrivate, Point, TrestleComm, VBT, XScrnPntOp;

REVEAL XScreenType.T <: T;

TYPE
  T_Pub = XScrnPntOp.T OBJECT END;
  T <: T_Pub;

TYPE
  XMode = {UseCopyPlane, UseCopyArea, UseFillRect, UseImageString,
           UseDrawString};

PROCEDURE ResolveTintGC (dpy: X.DisplayStar;
                         w  : X.Window;
                         st : XScreenType.T;
                         op : PaintPrivate.PaintOp): X.GC
  RAISES {TrestleComm.Failure};

PROCEDURE ResolveTextureGC (         dpy: X.DisplayStar;
                                     w  : X.Window;
                                     st : XScreenType.T;
                                     op : PaintPrivate.PaintOp;
                                     pm : PaintPrivate.Pixmap;
                            READONLY del: Point.T               ): X.GC
  RAISES {TrestleComm.Failure};

PROCEDURE ResolveFillGC (         dpy : X.DisplayStar;
                                  w   : X.Window;
                                  st  : XScreenType.T;
                                  op  : PaintPrivate.PaintOp;
                                  pm  : PaintPrivate.Pixmap;
                         READONLY del : Point.T;
                                  wind: VBT.WindingCondition  ): X.GC
  RAISES {TrestleComm.Failure};

PROCEDURE ResolveStrokeGC (         dpy  : X.DisplayStar;
                                    w    : X.Window;
                                    st   : XScreenType.T;
                                    op   : PaintPrivate.PaintOp;
                                    pm   : PaintPrivate.Pixmap;
                           READONLY del  : Point.T;
                                    width: CARDINAL;
                                    end  : VBT.EndStyle;
                                    join : VBT.JoinStyle         ): X.GC
  RAISES {TrestleComm.Failure};

PROCEDURE ResolvePixmapGC (            dpy  : X.DisplayStar;
                                       w    : X.Window;
                                       st   : XScreenType.T;
                                       op   : PaintPrivate.PaintOp;
                                       pm   : PaintPrivate.Pixmap;
                           READONLY    delta: Point.T;
                           VAR (*OUT*) mode : XMode;
                           VAR (*OUT*) src  : X.Pixmap              ): X.GC
  RAISES {TrestleComm.Failure};

PROCEDURE ResolveScrollGC (dpy: X.DisplayStar;
                           w  : X.Window;
                           st : XScreenType.T;
                           op : PaintPrivate.PaintOp): X.GC
  RAISES {TrestleComm.Failure};

PROCEDURE ResolveTextGC (            dpy    : X.DisplayStar;
                                     w      : X.Window;
                                     st     : XScreenType.T;
                                     op     : PaintPrivate.PaintOp;
                                     clipped: BOOLEAN;
                                     fnt    : PaintPrivate.Font;
                         VAR (*OUT*) mode   : XMode     ): X.GC
  RAISES {TrestleComm.Failure};

END XGC.

