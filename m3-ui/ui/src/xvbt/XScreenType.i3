(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* by Steve Glassman, Mark Manasse and Greg Nelson *)
(* Last modified on Fri Aug  6 17:27:39 PDT 1993 by sfreeman *)
(* modified on Thu May 27 11:41:50 PDT 1993 by msm *)
(* modified on Mon Feb 24 13:59:50 PST 1992 by muller *)
(* modified on Wed Nov 20 18:49:31 PST 1991 by gnelson *)

<*PRAGMA LL*>

UNSAFE INTERFACE XScreenType;

IMPORT X, VBT, Rect, XClient;

PROCEDURE New (trsl: XClient.T; dpy: X.DisplayStar; i: INTEGER): T;
(* Create a screentype for the ith screen of dpy, which is the X connection
   underlying trsl.  LL <= VBT.mu. *)

TYPE
  T <: Public;
  Public =
    VBT.ScreenType OBJECT
      trsl: XClient.T;
      (* Remaining fields protected by the .trsl field. *)
      root: X.Window;
      (* root window for this screentype *)
      rootDom : Rect.T;
      screenID: CARDINAL;
      (* X screen ID of root window. *)
      visual: X.VisualStar;
      (* visual for root window. *)
      backing_store                      := X.NotUseful;
      captureGC, noExposeCaptureGC: X.GC := NIL;
      (* used for implementing capture *)
      imageGC: X.GC := NIL;
      (* used for putting & getting images *)
      nullCursor: X.Cursor := X.None; (* for peekaboo mode, set on cage *)
    END;

END XScreenType.
