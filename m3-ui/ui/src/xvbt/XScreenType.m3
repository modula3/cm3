(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* by Steve Glassman, Mark Manasse and Greg Nelson *)
(* Last modified on Mon Nov 22 13:44:10 PST 1993 by steveg   *)
(*      modified on Wed Jul 14 18:20:59 PDT 1993 by sfreeman *)
(* modified on Thu May 27 11:41:48 PDT 1993 by msm *)


(* modified on Mon Feb 24 13:59:52 PST 1992 by muller *)
(* modified on Mon Dec 30 17:55:00 PST 1991 by gnelson *)
<*PRAGMA LL*>

UNSAFE MODULE XScreenType;

IMPORT X, XClient, Rect, PaintOp, Pixmap, TrestleComm, ScreenType, Axis,
       TrestleOnX, XScrnTpRep, XScrnFont, XScrnCmap, XScrnCrsr, XScrnPntOp,
       XScrnPxmp, XGC, Ctypes, Word;

REVEAL T = XGC.T BRANDED OBJECT END;

PROCEDURE New (trsl: XClient.T; dpy: X.DisplayStar; i: INTEGER): T =
  VAR
    res                         := NEW(T, trsl := trsl);
    n       : Ctypes.int;
    template: X.XVisualInfo;
    visuals : X.XVisualInfoStar;
  BEGIN
    TRY
      TrestleOnX.Enter(trsl);
      TRY
        template.visualid :=
          X.XVisualIDFromVisual(X.XDefaultVisual(dpy, i));
        template.screen := i;
        visuals :=
          X.XGetVisualInfo(dpy, X.VisualIDMask + X.VisualScreenMask,
                           ADR(template), ADR(n));
        TRY
          WITH vis = LOOPHOLE(visuals, UNTRACED REF
                              ARRAY [0 .. 9999] OF X.XVisualInfo) DO
            template.depth := -1;
            FOR i := 0 TO n - 1 DO
              IF vis[i].depth > template.depth THEN template := vis[i]; END
            END
          END
        FINALLY
          X.XFree(LOOPHOLE(visuals, Ctypes.char_star))
        END;
        (* res.depth > 0, since the default visual must be supported. *)
        res.depth := template.depth;
        res.color := (template.class # X.StaticGray)
                       AND (template.class # X.GrayScale);
        res.bg := X.XWhitePixel(dpy, i);
        res.fg := X.XBlackPixel(dpy, i);
        (* res.backing_store := X.XDoesBackingStore(X.XScreenOfDisplay(dpy,
           i)); *)
        New2(dpy, i, res);
        res.font := XScrnFont.NewOracle(res);
        res.cmap := XScrnCmap.NewOracle(res, template);
        res.nullCursor := XScrnCrsr.NullCursor(dpy, res.root)
      FINALLY
        TrestleOnX.Exit(trsl)
      END;
      res.bits := NewDepthOne(trsl, dpy, i)
    EXCEPT
      X.Error, TrestleComm.Failure =>     (*skip*)
    END;
    RETURN res
  END New;

PROCEDURE NewDepthOne (trsl: XClient.T; dpy: X.DisplayStar; i: INTEGER):
  T =
  VAR res := NEW(T, trsl := trsl);
  BEGIN
    TRY
      TrestleOnX.Enter(trsl);
      TRY
        res.depth := 1;
        res.color := FALSE;
        res.bg := 0;
        res.fg := 1;
        res.bits := res;
        New2(dpy, i, res);
        res.font := XScrnFont.NewOracle(res, TRUE);
        res.cmap := NIL
      FINALLY
        TrestleOnX.Exit(trsl)
      END
    EXCEPT
      TrestleComm.Failure =>     (*skip*)
    END;
    RETURN res
  END NewDepthOne;

CONST GCValueMask = Word.Or(X.GCFunction, X.GCPlaneMask);

VAR gcValues := NEW(X.XGCValuesStar);

PROCEDURE New2 (dpy: X.DisplayStar; i: INTEGER; res: T)
  RAISES {TrestleComm.Failure} =
  (* The initialization common to st and st.bits.  LL = trsl *)
  BEGIN
    TRY
    res.res[Axis.T.Hor] :=
      FLOAT(X.XDisplayWidth(dpy, i)) / FLOAT(X.XDisplayWidthMM(dpy, i));
    res.res[Axis.T.Ver] :=
      FLOAT(X.XDisplayHeight(dpy, i)) / FLOAT(X.XDisplayHeightMM(dpy, i));
    res.op := XScrnPntOp.NewOracle(res);
    res.cursor := XScrnCrsr.NewOracle(res);
    res.pixmap := XScrnPxmp.NewOracle(res);
    res.optable :=
      NEW(REF ARRAY OF XScrnTpRep.OpRecord, NUMBER(PaintOp.Predefined));
    res.pmtable :=
      NEW(REF ARRAY OF XScrnTpRep.PixmapRecord, NUMBER(Pixmap.Predefined));
    res.root := X.XRootWindow(dpy, i);
    res.rootDom :=
      Rect.FromSize(X.XDisplayWidth(dpy, i), X.XDisplayHeight(dpy, i));
    res.screenID := i;
    res.visual := X.XDefaultVisual(dpy, i);
    res.imageGC := X.XCreateGC(dpy, res.root, GCValueMask, gcValues);
    EXCEPT X.Error => RAISE TrestleComm.Failure END;
  END New2;

BEGIN
  gcValues.function := X.GXcopy;
  gcValues.plane_mask := X.XAllPlanes();
END XScreenType.
