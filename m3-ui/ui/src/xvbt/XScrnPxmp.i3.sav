(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* by Steve Glassman, Mark Manasse and Greg Nelson *)
(* Last modified on Fri Mar 11 15:30:30 PST 1994 by gnelson *)
(*      modified on Fri Nov  6 17:50:55 PST 1992 by msm    *)
(*      modified on Mon Feb 24 13:59:53 PST 1992 by muller *)
<*PRAGMA LL*>

UNSAFE INTERFACE XScrnPxmp;

IMPORT Rect, ScrnPixmap, X, XScreenType, XScrnTpRep, TrestleComm;

REVEAL XScreenType.T <: T;

TYPE
  T_Pub = XScrnTpRep.Public OBJECT END;
  T <: T_Pub;

PROCEDURE NewOracle (st: XScreenType.T): ScrnPixmap.Oracle RAISES {};

PROCEDURE FromXPixmap (         st    : XScreenType.T;
                                xpm   : X.Pixmap;
                       READONLY bounds: Rect.T;
                                depth : INTEGER        ): ScrnPixmap.T;
<* LL.sup = st.trsl *>
(* Construct and return the screen pixmap of type st whose contents are
   those of xpm, which has the given bounds and depth. *)

PROCEDURE FakeCapture (         st    : XScreenType.T;
                                w     : X.Drawable;
                       READONLY bounds: Rect.T;
                                depth : INTEGER        ): ScrnPixmap.T;
<* LL.sup = st.trsl *>
(* Construct and return the lazy screen pixmap of type st whose contents
   initially reference the drawable w, which has the given bounds and
   depth. *)

PROCEDURE FinishCapture (st: XScreenType.T; pmId: INTEGER; xpm: X.Pixmap);
<* LL.sup = st.trsl *>
(* Change the lazy pixmap whose id is pmId to reference the drawable xpm,
   and mark the pixmap unlazy. *)

PROCEDURE IsLazy(st: XScreenType.T; pmId: INTEGER): BOOLEAN;
<* LL.sup = st.trsl *>
(* Return whether pmId is lazy *)

PROCEDURE GetDrawable(st: XScreenType.T; pmId: INTEGER): X.Drawable;
<* LL.sup = st.trsl *>
(* Return the drawable assoiciated with pmId *)

PROCEDURE PixmapDomain (st: XScreenType.T; pmId: INTEGER): Rect.T;
<* LL.sup = st.trsl *>
(* return the domain of the ScrnPixmap.T whose id is pmId. *)

PROCEDURE PixmapFromRaw (st: XScreenType.T; pm: ScrnPixmap.Raw): X.Pixmap
  RAISES {TrestleComm.Failure}; <* LL.sup = st.trsl *>

END XScrnPxmp.
