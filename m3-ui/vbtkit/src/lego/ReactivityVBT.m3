(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Feb  8 22:57:10 PST 1996 by mhb        *)
(*      modified on Tue Jun  7 13:45:14 PDT 1994 by msm        *)
(*      modified on Sat Feb  6 18:05:25 PST 1993 by meehan     *)
(*      modified on Tue Jun 16 13:08:23 PDT 1992 by muller     *)
(*      modified on Fri Mar 27 02:33:52 1992 by steveg     *)
<* PRAGMA LL *>

MODULE ReactivityVBT;

IMPORT Cursor, PaintOp, MouseSplit, VBT, VBTClass, FilterClass,
         Batch, BatchUtil, BatchRep, ETAgent, Rect, Region, Point,
         ScrnPixmap, Pixmap;

REVEAL
  T = Public BRANDED OBJECT
        newState: State;
        newCursor: Cursor.T;
        newOp   : PaintOp.ColorScheme;
        (* state, cursor, op are protected by both VBT.mu and ch, if any: *)
        state: State;
        cursor: Cursor.T;
        op   : PaintOp.ColorScheme;
        (* lastFocus is protected by v *)
        lastFocus: VBT.T := NIL
      OVERRIDES
      (* of ReactivityVBT methods: *)
        init         := Init;
        paintDormant := PaintDormant;
      (* of VBT methods: *)
        capture    := Capture;
        paintbatch := PaintBatch;
        reshape    := Reshape;
        redisplay  := Redisplay;
        repaint    := Repaint;
        locate     := Locate;
        key        := Key;
        misc       := Misc;
        acquire    := Acquire;
      END;

PROCEDURE Init (v: T; ch: VBT.T; colors: PaintOp.ColorScheme := NIL): T =
  BEGIN
    EVAL ETAgent.T.init (v, ch);
    v.state := State.Active;
    v.newState := State.Active;
    v.cursor := Cursor.DontCare;
    v.newCursor := Cursor.DontCare;
    IF colors = NIL THEN colors := PaintOp.bgFg END;
    v.op := colors;
    v.newOp := colors;
    RETURN v
  END Init;

PROCEDURE PaintDormant (v: T; r: Rect.T; colors: PaintOp.ColorScheme) = 
  BEGIN
    VBT.PaintTexture (v, r, colors.transparentBg, Pixmap.Gray, Point.Origin)
  END PaintDormant;

PROCEDURE Set (v: T; state: State; cursor: Cursor.T) =
  BEGIN
    v.newState := state;
    v.newCursor := cursor;
    IF v.state # state OR v.cursor # cursor THEN VBT.Mark (v) END
  END Set;

PROCEDURE Get (v: T): State =
  BEGIN
    RETURN v.newState
  END Get;

PROCEDURE GetCursor (v: T): Cursor.T =
  BEGIN
    RETURN v.newCursor
  END GetCursor;

PROCEDURE SetColors (v: T; colors: PaintOp.ColorScheme) =
  BEGIN
    v.newOp := colors;
    IF v.newOp # v.op AND v.state >= State.Dormant THEN VBT.Mark (v) END
  END SetColors;

PROCEDURE Capture (               v : T;
                   <* UNUSED *> ch  : VBT.T;
                       READONLY rect: Rect.T;
                       VAR      br  : Region.T): ScrnPixmap.T =
  BEGIN
    IF v.state = State.Active THEN
      RETURN VBT.Capture(v, rect, br)
    ELSE
      br := Region.FromRect(rect);
      RETURN NIL
    END
  END Capture;

PROCEDURE UpdateStateAndFixCursor (    v            : T;
                                   VAR state, ostate: State;
                                   VAR op, oop      : PaintOp.ColorScheme) =
  VAR cursor := v.newCursor;
      ocursor := v.cursor;
  BEGIN
    state := v.newState;
    ostate := v.state;
    op := v.newOp;
    oop := v.op;
    IF v.ch # NIL THEN
      LOCK v.ch DO
        v.op := v.newOp;
        v.cursor := v.newCursor;
        v.state := v.newState
      END
    ELSE
      v.op := v.newOp;
      v.cursor := v.newCursor;
      v.state := v.newState
    END;
    IF state # ostate THEN
      IF state = State.Active OR ostate = State.Active THEN
        MouseSplit.InvalidateCache(v)
      END
    END;
    IF cursor # ocursor THEN
      VBT.SetCursor(v, cursor);
    END
  END UpdateStateAndFixCursor;

PROCEDURE Redisplay (v: T) =
  VAR
    cdP          : VBT.ReshapeRec;
    state, ostate: State;
    op, oop      : PaintOp.ColorScheme;
  BEGIN
    IF (v.ch = NIL) OR NOT Rect.Equal(VBT.Domain(v.ch), VBT.Domain(v)) THEN
      cdP.new := VBT.Domain(v);
      cdP.prev := cdP.new;
      cdP.saved := Rect.Empty;
      cdP.marked := TRUE;
      Reshape(v, cdP);
      RETURN
    END;
    UpdateStateAndFixCursor(v, state, ostate, op, oop);
    IF ((state >= State.Dormant) OR (ostate >= State.Dormant))
         AND ((state # ostate) OR (op # oop)) THEN
      IF (state = State.Dormant) AND (ostate # State.Vanish) THEN
        v.paintDormant (Rect.Full, op)
      ELSIF state = State.Vanish THEN
        VBT.PaintTexture(v, Rect.Full, op.bg, Pixmap.Solid, Point.Origin)
      ELSE
        VBTClass.Repaint(v, Region.FromRect(VBT.Domain(v)))
      END
    END
  END Redisplay;

PROCEDURE Reshape (v: T; READONLY cd: VBT.ReshapeRec) =
  VAR
    cdP          : VBT.ReshapeRec;
    state, ostate: State;
    op, oop      : PaintOp.ColorScheme;
  BEGIN
    cdP := cd;
    UpdateStateAndFixCursor (v, state, ostate, op, oop);
    IF state >= State.Dormant THEN cdP.saved := Rect.Empty END;
    ETAgent.T.reshape (v, cdP);
    IF state = State.Vanish THEN
      VBT.PaintTexture (v, Rect.Full, op.bg, Pixmap.Solid, Point.Origin)
    END
  END Reshape;

PROCEDURE Repaint (v: T; READONLY bad: Region.T) =
  BEGIN
    IF v.state = State.Vanish THEN
      VBT.PaintTexture (v, bad.r, v.op.bg, Pixmap.Solid, Point.Origin)
    ELSE
      ETAgent.T.repaint (v, bad)
    END
  END Repaint;

PROCEDURE Locate (v: T; READONLY pt: Point.T; VAR (* out*) r: Rect.T):
  VBT.T =
  BEGIN
    IF v.state # State.Active THEN
      r := Rect.Full;
      RETURN NIL
    ELSE
      RETURN ETAgent.T.locate (v, pt, r)
    END
  END Locate;

PROCEDURE PaintBatch (v: T; ch: VBT.T; ba: Batch.T) =
  VAR rect: Rect.T;
  BEGIN
    IF v.state < State.Dormant THEN
      VBTClass.PaintBatch(v, ba)
    ELSIF v.state = State.Vanish THEN
      Batch.Free(ba)
    ELSE
      BatchUtil.Tighten(ba);
      IF Rect.IsEmpty(ba.scrollSource) THEN
        rect := ba.clip;
	VBT.BeginGroup(v);
        VBTClass.PaintBatch(v, ba);
        v.paintDormant (rect, v.op);
	VBT.EndGroup(v)
      ELSE
        VBTClass.ForceRepaint(ch, Region.FromRect(ba.clip))
      END
    END
  END PaintBatch;

PROCEDURE Key (v: T; READONLY cd: VBT.KeyRec) =
  BEGIN
    IF v.state = State.Active THEN ETAgent.T.key (v, cd) END
  END Key;

PROCEDURE Acquire(v: T; ch: VBT.T; w: VBT.T; 
  s: VBT.Selection; ts: VBT.TimeStamp) 
  RAISES {VBT.Error} = <* LL.sup = ch *>
  BEGIN
    ETAgent.T.acquire(v, ch, w, s, ts);
    IF s = VBT.KBFocus THEN 
      LOCK v DO v.lastFocus := w END
    END
  END Acquire;

PROCEDURE Misc (v: T; READONLY cd: VBT.MiscRec) =
  <* LL.sup = VBT.mu *>
  VAR lastFocus, parent: VBT.T;
  BEGIN
    IF cd.type = VBT.TakeSelection AND cd.selection = VBT.KBFocus THEN
      LOCK v DO lastFocus := v.lastFocus END;                 (* CHANGED *)
      IF lastFocus # NIL THEN
        (* Is it still our descendant? *)
        parent := VBT.Parent (lastFocus); (* REQUIRES LL.sup < lastFocus *)
        WHILE parent # NIL AND parent # v DO
          parent := VBT.Parent (parent)   (* REQUIRES LL.sup < parent *)
        END;
        IF parent = NIL THEN   (* No. *)
          LOCK v DO v.lastFocus := NIL END                    (* CHANGED *)
        ELSE                   (* Yes. *)
          VBTClass.Misc (lastFocus, cd); RETURN
        END
      END
    END;
    ETAgent.T.misc (v, cd)
  END Misc;

BEGIN
END ReactivityVBT.


