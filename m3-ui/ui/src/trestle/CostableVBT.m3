(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Jul 15 19:19:14 PDT 1993 by msm                      *)
(*      modified on Mon Feb 24 14:01:20 PST 1992 by muller                   *)
<*PRAGMA LL*>

MODULE CostableVBT;

IMPORT Axis, Filter, Rect, VBT, VBTClass, Trestle, Point, Thread;

CONST
  NoPref = ARRAY Axis.T OF INTEGER {-1, ..};
  BigPref = ARRAY Axis.T OF INTEGER{VBT.DefaultShape.hi, ..};

REVEAL Link = LinkPublic BRANDED OBJECT 
    c: Thread.Condition;
  END;

REVEAL
  T = Public BRANDED OBJECT
        <* LL >= {SELF} *>
        pref     := NoPref;
        scrShape := BigPref;
        proj     := TRUE
      OVERRIDES
        shape    := Shape;
        reshape  := Reshape;
        rescreen := Rescreen;
        init     := Init;
      END;

PROCEDURE Disable(v: VBT.T) = BEGIN
  LOOP
    TYPECASE v OF
      NULL => RETURN
    | T (w) => LOCK w DO w.pref := NoPref END; RETURN
    ELSE v := VBT.Parent(v)
    END
  END
END Disable;

PROCEDURE GetProjecting(v: VBT.T): BOOLEAN = BEGIN
  LOOP
    TYPECASE v OF
      NULL => RETURN FALSE
    | T (w) => LOCK w DO RETURN w.proj END
    ELSE v := VBT.Parent(v)
    END
  END
END GetProjecting;

PROCEDURE SetProjecting (v: VBT.T; proj: BOOLEAN) =
  BEGIN
    LOOP
      TYPECASE v OF
        NULL => RETURN
      | T (w) => LOCK w DO w.proj := proj END; RETURN
      ELSE
        v := VBT.Parent(v)
      END
    END
  END SetProjecting;

PROCEDURE SetShape (v: VBT.T; hPref, vPref: CARDINAL) =
  BEGIN
    LOOP
      TYPECASE v OF
        NULL => RETURN
      | T (w) =>
          LOCK w DO
            IF hPref # 0 THEN w.pref[Axis.T.Hor] := hPref END;
            IF vPref # 0 THEN w.pref[Axis.T.Ver] := vPref END
          END;
          VBT.NewShape(v);
          RETURN
      ELSE
        v := VBT.Parent(v)
      END
    END
  END SetShape;

PROCEDURE Shape (w: T; ax: Axis.T; n: CARDINAL): VBT.SizeRange =
  VAR res: VBT.SizeRange;
  BEGIN
    IF n = 0 THEN n := Rect.Size(ax, VBT.Domain(w)) END;
    res := Filter.T.shape(w, ax, n);
    LOCK w DO
      IF res.lo <= w.pref[ax] AND w.pref[ax] < res.hi THEN
        res.pref := w.pref[ax]
      ELSIF w.pref[ax] < 0 THEN
        res.pref := MAX(res.lo, MIN(res.pref, w.scrShape[ax]))
      ELSIF w.proj THEN
        res.pref := MAX(res.lo, MIN(res.hi - 1, w.pref[ax]))
      END
    END;
    RETURN res
  END Shape;

PROCEDURE Reshape(w: T; READONLY cd: VBT.ReshapeRec) =
  BEGIN
    IF NOT Rect.IsEmpty(cd.new) THEN
      LOCK w DO
        FOR ax := FIRST(Axis.T) TO LAST(Axis.T) DO
          w.pref[ax] := Rect.Size(ax, cd.new)
        END
      END
    END;
    Filter.T.reshape(w, cd)
  END Reshape;

PROCEDURE Rescreen (w: T; READONLY cd: VBT.RescreenRec) =
  VAR dom := Trestle.ScreenOf(w, Point.Origin).dom;
  BEGIN
    FOR ax := FIRST(Axis.T) TO LAST(Axis.T) DO
      w.scrShape[ax] := Rect.Size(ax, dom)
    END;
    Public.rescreen(w, cd)
  END Rescreen;

PROCEDURE Init(v: T; ch: VBT.T; proj: BOOLEAN): T =
  BEGIN
    LOCK v DO v.proj := proj END;
    EVAL Filter.T.init(v, ch);
    RETURN v
  END Init;

PROCEDURE New(ch: VBT.T; project := TRUE): T = BEGIN
    RETURN NEW(T).init(ch, project)
  END New;

BEGIN END CostableVBT.
