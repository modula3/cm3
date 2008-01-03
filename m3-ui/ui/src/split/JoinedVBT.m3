(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Jul 19 10:40:12 PDT 1994 by msm      *)
(*      modified on Wed Jul 28 13:12:40 PDT 1993 by sfreeman *)

<* PRAGMA LL *>

MODULE JoinedVBT;

(* Since JoinVBT is being used inside Trestle to tee the screen for
   Mirage and JoinScreen, it needs to be efficient in the normal case of
   a single parent. A paint short-circuiting similar to that of ZSplit
   is used *)

IMPORT Batch, BatchRep, BatchUtil, Filter, FilterClass, MouseSplit, Rect,
       Region, ScrnCursor, ScrnPixmap, VBT, VBTClass, VBTRep, JoinParent,
       Axis, JoinPixmap, JoinScreen;

REVEAL
  T = JoinParent.Join BRANDED OBJECT
      OVERRIDES
        paintbatch := PaintBatch;
        capture    := Capture;
        sync       := Sync;
        discard    := Discard;
        newShape   := NewShape;
        redisplay  := Redisplay;
        rescreen   := Rescreen;
        reshape    := Reshape;
        repaint    := Repaint;
        position   := Position;
        shape      := Shape;
        init       := Be;
        setcursor  := SetCursor
      END;

TYPE
  ParentList = JoinParent.T;
   
PROCEDURE PaintBatch (v: T; <* UNUSED *> ch: VBT.T; ba: Batch.T) =
  VAR saved, pl: ParentList;
  bb: Batch.T;
  BEGIN                          (* LL = ch *)
    LOCK v DO
      pl := v.parents;
      IF pl = NIL THEN Batch.Free(ba); RETURN END;
      BatchUtil.Tighten(ba);
      WHILE pl # NIL
              AND (pl.st = NIL OR NOT Rect.Overlap(ba.clip, pl.domain)) DO
        pl := pl.link
      END;
      IF pl = NIL THEN Batch.Free(ba); RETURN END;
      saved := pl;
      pl := pl.link;
      WHILE pl # NIL DO
        IF pl.st # NIL AND Rect.Overlap(ba.clip, pl.domain) THEN
          bb := BatchUtil.Copy(ba);
          VBTClass.PaintBatch(pl.ch, bb)
        END;
        pl := pl.link
      END;
      VBTClass.PaintBatch(saved.ch, ba)
    END
  END PaintBatch;

PROCEDURE Capture (                         v   : T;
                   <* UNUSED *>             ch  : VBT.T;
                                READONLY    clip: Rect.T;
                                VAR (*out*) br  : Region.T): ScrnPixmap.T =
  VAR
    pl : ParentList;
    bad: Region.T;
    res: JoinPixmap.T;
  BEGIN
    LOCK v DO
      pl := v.parents;
      IF pl = NIL THEN br := Region.FromRect(clip); RETURN NIL END;
      IF pl.link = NIL OR NOT ISTYPE(v.st, JoinScreen.T) THEN 
        RETURN VBT.Capture(pl.ch, clip, br) 
      END;
      br := Region.Empty;
      res := JoinPixmap.Create(v.st, clip);
      WHILE pl # NIL DO
        JoinPixmap.AddPixmap(res, pl.st, VBT.Capture(pl.ch, clip, bad));
        br := Region.Join(br, bad);
        pl := pl.link
      END;
      RETURN res
    END;
  END Capture;

PROCEDURE Sync(v: T; <* UNUSED *> ch: VBT.T; wait := TRUE)  =
  VAR
    pl: ParentList;
  BEGIN
    LOCK v DO
      pl := v.parents;
      WHILE pl # NIL DO
        VBT.Sync(pl.ch, wait);
        pl := pl.link
      END
    END;
  END Sync;

PROCEDURE SetCursor (v: T; ch: VBT.T) =
  BEGIN                          (* LL = ch *)
    Public.setcursor(v, ch);
    UpdateCursor(v);
  END SetCursor;

PROCEDURE UpdateCursor (v: T) =
  VAR
    pl: ParentList;
    cs: ScrnCursor.T;
  BEGIN
    LOCK v DO
      cs := v.getcursor();
      pl := v.parents;
      WHILE pl # NIL DO JoinParent.SetCursor(pl, cs); pl := pl.link END;
    END;
  END UpdateCursor;

PROCEDURE Discard (v: T) =
  VAR pl: ParentList;
  BEGIN
    LOOP
      LOCK v DO pl := v.parents END;
      IF pl = NIL THEN EXIT END;
      JoinParent.Rem(pl)
    END;
    Filter.T.discard(v);
  END Discard;

(* some prop munging stolen from VBT.NewShape *)
PROCEDURE NewShape (v: T;  <* UNUSED *>ch: VBT.T) =
  VAR
    pl: ParentList;
  BEGIN
    LOCK v DO
      v.props := v.props + VBTRep.Props{VBTRep.Prop.HasNewShape,
                                        VBTRep.Prop.BlockNewShape};
      pl := v.parents;
      WHILE pl # NIL DO VBT.NewShape(pl.ch); pl := pl.link; END;
    END;
  END NewShape;

PROCEDURE ReallyRescreen(v: VBT.T; st: VBT.ScreenType) =
  BEGIN
    IF st # NIL AND v.st = st THEN VBTClass.Rescreen(v, NIL) END;
    VBTClass.Rescreen(v, st)
  END ReallyRescreen;

PROCEDURE Redisplay (v: T) =
  BEGIN
    IF JoinParent.NeedsRescreen(v) THEN
      ReallyRescreen(v, JoinParent.ST(v))
    END;
    IF v.domain # JoinParent.Domain(v) THEN
      VBTClass.Reshape(v, JoinParent.Domain(v), Rect.Empty)
    ELSIF v.ch # NIL THEN
      VBTClass.Repaint(v.ch, Region.Empty)
    END
  END Redisplay;

PROCEDURE Rescreen (v: T; READONLY cd: VBT.RescreenRec) =
  BEGIN
    Public.rescreen(v, cd);
    UpdateCursor(v);
    IF cd.marked AND v.domain # JoinParent.Domain(v) THEN
      VBTClass.Reshape(v, JoinParent.Domain(v), Rect.Empty)
    END
  END Rescreen;

PROCEDURE Reshape (v: T; READONLY cd: VBT.ReshapeRec) =
  BEGIN
    IF cd.marked AND JoinParent.NeedsRescreen(v) THEN
      ReallyRescreen(v, JoinParent.ST(v));
      VBTClass.Reshape(v, cd.new, Rect.Empty)
    ELSE
      Public.reshape(v, cd)
    END
  END Reshape;

PROCEDURE Repaint (v: T; READONLY br: Region.T) =
  BEGIN
    IF VBT.IsMarked(v) THEN
      IF JoinParent.NeedsRescreen(v) THEN
        ReallyRescreen(v, JoinParent.ST(v))
      END;
      IF v.domain # JoinParent.Domain(v) THEN
        VBTClass.Reshape(v, JoinParent.Domain(v), Rect.Empty)
      ELSE
        Public.repaint(v, br)
      END
    ELSE
      Public.repaint(v, br)
    END;
  END Repaint;

PROCEDURE Shape (v: T; axis: Axis.T; n: CARDINAL): VBT.SizeRange =
  BEGIN
    IF VBT.IsMarked(v) AND JoinParent.NeedsRescreen(v) THEN
      ReallyRescreen(v, JoinParent.ST(v))
    END;
    RETURN JoinParent.Join.shape(v, axis, n)
  END Shape;

PROCEDURE Position (v: T; READONLY cd: VBT.PositionRec) =
  BEGIN
    Public.position(v, cd);
    UpdateCursor(v)
  END Position;

PROCEDURE Be (v: T; ch: VBT.T): T =
  BEGIN
    LOCK v DO VBTClass.ClearShortCircuit(v); END;
    RETURN Filter.T.init(v, ch);
  END Be;

PROCEDURE New(ch: VBT.T): T  =
  BEGIN
    RETURN NEW(T).init(ch)
  END New;

BEGIN
END JoinedVBT.
