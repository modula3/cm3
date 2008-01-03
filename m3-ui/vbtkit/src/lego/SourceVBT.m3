(* Copyright (C) 1992, Digital Equipment Corporation       *)
(* All rights reserved.                                    *)
(* See the file COPYRIGHT for a full description.          *)
(*                                                         *)
(* Last modified on Sat Sep 30 11:57:00 PDT 1995 by mhb    *)
(*      modified on Mon Jan 30 15:14:32 PST 1995 by kalsow *)
(*      modified on Mon Mar 15 16:52:17 PST 1993 by meehan *)
(*      modified on Tue Jun 16 20:46:56 PDT 1992 by muller *)
(*      modified on Fri Mar 27 02:55:27 1992 by steveg     *)

MODULE SourceVBT;

IMPORT Axis, BtnVBTClass, ButtonVBT, Cursor, FeedbackVBT, Filter,
       HighlightVBT, HVSplit, MultiClass, MultiFilter, PaintOp,
       Pixmap, Point, Rect, Split, SwitchVBT,
       TrestleClass, VBT, VBTKitResources, Word;

FROM VBT IMPORT ClickType;

REVEAL
  T = Public BRANDED OBJECT
        root  : VBT.T;
        target: Target;
      OVERRIDES
        init     := Init;
        pre      := Pre;
        post     := Post;
        during   := During;
        callback := Callback;
        hit      := AlwaysHit;
        cancel   := Cancel;
        mouse    := Mouse;
        position := Position;
      END;

TYPE
  MC = SwitchVBT.MC OBJECT END;

PROCEDURE Init (v: T; f: FeedbackVBT.T): T =
  BEGIN
    GetResources();
    EVAL ButtonVBT.T.init(v, f, NIL);
    MultiClass.Be(v, NEW(MC));
    WITH ch = MultiFilter.Child(f) DO
      IF ch # NIL THEN MultiClass.BeChild(v, ch) END;
    END;
    RETURN v
  END Init;

PROCEDURE Callback (<* UNUSED *>          v : T;
                    <* UNUSED *> READONLY cd: VBT.MouseRec) =
  BEGIN
  END Callback;

PROCEDURE Mouse(v: T; READONLY cd: VBT.MouseRec) =
  BEGIN
    Filter.T.mouse(v, cd);
    IF cd.clickType = ClickType.FirstDown THEN
      v.ready := TRUE;
      v.pre();
      VBT.SetCage(v, VBT.CageFromPosition(cd.cp, TRUE));
    ELSIF v.ready THEN
      v.ready := FALSE;
      IF cd.clickType = ClickType.LastUp AND NOT cd.cp.offScreen THEN
        v.post();
        IF v.target # NIL THEN v.callback(cd) END;
      ELSE
        v.cancel();
      END;
    END;
  END Mouse;

PROCEDURE Position (v: T; READONLY cd: VBT.PositionRec) =
  BEGIN
    Filter.T.position(v, cd);
    IF v.ready THEN
      VBT.SetCage(v, VBT.CageFromPosition(cd.cp, TRUE));
      IF NOT cd.cp.offScreen THEN
        v.during(cd);
      END;
    END;
  END Position;

PROCEDURE AlwaysHit (<* UNUSED *> v     : Public;
                     <* UNUSED *> target: VBT.T;
                     <* UNUSED *> READONLY cd: VBT.PositionRec):
  BOOLEAN =
  BEGIN
    RETURN TRUE
  END AlwaysHit;

<* UNUSED *>
PROCEDURE NeverHit (<* UNUSED *>          v     : Public;
                    <* UNUSED *>          target: VBT.T;
                    <* UNUSED *> READONLY cd    : VBT.PositionRec):
  BOOLEAN =
  BEGIN
    RETURN FALSE
  END NeverHit;

REVEAL
  TargetClass = TargetClassPublic BRANDED OBJECT
                OVERRIDES
                  normal  := DullTarget;
                  excited := DullTarget
                END;

PROCEDURE DullTarget (<* UNUSED *> class: TargetClass) =
  BEGIN
  END DullTarget;

TYPE Prop = BRANDED REF TargetClass;

PROCEDURE BeTarget (w: VBT.T; class: TargetClass) =
  VAR p := NEW (Prop);
  BEGIN
    p^ := class;
    VBT.PutProp (w, p);
    class.vbt := w
  END BeTarget;
  
PROCEDURE IsTarget (w: VBT.T): BOOLEAN =
  BEGIN
    RETURN VBT.GetProp(w, TYPECODE(Prop)) # NIL
  END IsTarget;

PROCEDURE GetHighlighter (v: T): HighlightVBT.T =
  BEGIN
    RETURN HighlightVBT.Find(v.root)
  END GetHighlighter;

PROCEDURE GetTarget (v: T): Target =
  BEGIN
    RETURN v.target
  END GetTarget;

PROCEDURE Pre (v: T) =
  BEGIN
    FeedbackVBT.Excited (Filter.Child (v));
    VBT.SetCursor(v, MovingCursor);
    v.root := FindInstalledAncestor(v);
    v.target := NIL;
  END Pre;

PROCEDURE Post (v: T) =
  BEGIN
    FeedbackVBT.Normal(Filter.Child(v));
    Stop(v);
  END Post;

PROCEDURE Cancel (v: T) =
  BEGIN
    FeedbackVBT.Normal(Filter.Child(v));
    Stop(v);
  END Cancel;

PROCEDURE Stop (v: T) =
  BEGIN
    IF v.target # NIL THEN TargetClassOf(v.target).normal() END;
    VBT.SetCursor(v, Cursor.DontCare);
  END Stop;

PROCEDURE During (v: T; READONLY cd: VBT.PositionRec) =
  VAR target := InTarget(v.root, cd.cp.pt);
  BEGIN
    IF target = NIL THEN
      IF v.target # NIL THEN TargetClassOf(v.target).normal() END;
      v.target := NIL;
    ELSIF v.target # target THEN
      IF v.target # NIL THEN TargetClassOf(v.target).normal() END;
      IF v.hit(target, cd) THEN
        TargetClassOf(target).source := v;
        v.target := target;
        TargetClassOf(v.target).excited();
      ELSE
        v.target := NIL
      END
    END
  END During;

PROCEDURE InTarget (root: VBT.T; READONLY pt: Point.T): VBT.T =
  VAR target, v: VBT.T;
  BEGIN
    target := NIL;
    v := root;
    LOOP
      TYPECASE v OF
      | VBT.Split (split) => v := Split.Locate(split, pt);
      | VBT.Leaf => EXIT
      ELSE <* ASSERT FALSE *>
      END;
      IF v = NIL THEN EXIT END;
      IF IsTarget(v) THEN target := v END;
    END;
    RETURN target
  END InTarget;

PROCEDURE FindInstalledAncestor (v: VBT.T): VBT.T =
  VAR p: VBT.T; ir: TrestleClass.InstallRef; BEGIN
    p := v;
    WHILE p # NIL DO
      ir := VBT.GetProp(p, TYPECODE(TrestleClass.InstallRef));
      IF ir # NIL AND ir.installCount > 0 THEN RETURN p END;
      p := VBT.Parent(p)
    END;
    RETURN NIL
  END FindInstalledAncestor;

PROCEDURE TargetClassOf (w: Target): TargetClass =
  VAR p: Prop := VBT.GetProp (w, TYPECODE (Prop));
  BEGIN
    RETURN p^
  END TargetClassOf;

PROCEDURE GetSource (w: Target): T =
  BEGIN
    RETURN TargetClassOf(w).source
  END GetSource;


TYPE
  DefaultTC = TargetClass OBJECT
                hl: HighlightVBT.T; 
                op: PaintOp.T;
              OVERRIDES
                normal  := Normal;
                excited := Excited;
              END;

PROCEDURE NewTarget (op := PaintOp.TransparentSwap): TargetClass =
  BEGIN
    RETURN NEW(DefaultTC, op := op)
  END NewTarget;

PROCEDURE Excited (tc: DefaultTC) =
  BEGIN
    WITH target = tc.vbt DO
      tc.hl := HighlightVBT.Find(target);
      HighlightVBT.SetTexture(tc.hl, Pixmap.Solid, Point.Origin, tc.op);
      HighlightVBT.SetRect(tc.hl, VBT.Domain(target), LAST(CARDINAL));
    END
  END Excited;

PROCEDURE Normal (tc: DefaultTC) =
  BEGIN
    HighlightVBT.SetRect(tc.hl, Rect.Empty)
  END Normal;


TYPE
  SwapTC =
    DefaultTC OBJECT OVERRIDES excited := ExcitedSwap; END;

PROCEDURE NewSwapTarget (op := PaintOp.TransparentSwap): TargetClass =
  BEGIN
    RETURN NEW(SwapTC, op := op)
  END NewSwapTarget;

PROCEDURE ExcitedSwap (tc: SwapTC) =
  BEGIN
    WITH target = tc.vbt, r = VBT.Domain(target) DO
      tc.hl := HighlightVBT.Find(target);
      GridHighlight (tc.hl, tc.op,
        Rect.Middle (r), MAX (Rect.HorSize (r), 17),
        MAX (Rect.VerSize (r), 17))
    END;
  END ExcitedSwap;


TYPE
  InserterTC = DefaultTC OBJECT
               OVERRIDES
                 normal  := NormalInserter;
                 excited := ExcitedInserter
               END;

PROCEDURE NewInserterTarget (op := PaintOp.TransparentSwap): TargetClass =
  BEGIN
    RETURN NEW(InserterTC, op := op)
  END NewInserterTarget;

PROCEDURE NormalInserter (tc: InserterTC) =
  BEGIN
    WITH source = tc.source DO
      HighlightVBT.SetRect(source.root, Rect.Empty)
    END
  END NormalInserter;

PROCEDURE ExcitedInserter (tc: InserterTC) =
  VAR hsz, vsz: CARDINAL;
  BEGIN
    WITH target = tc.vbt,
         source = tc.source,
         r      = VBT.Domain (target) DO
      CASE HVSplit.AxisOf (VBT.Parent (target)) OF
      | Axis.T.Hor =>
          hsz := MAX (Rect.HorSize (r), 65);
          vsz := Rect.VerSize (r);
      | Axis.T.Ver =>
          hsz := Rect.HorSize (r);
          vsz := MAX (Rect.VerSize (r), 65);
      END;
      GridHighlight (source.root, tc.op, Rect.Middle (r), hsz, vsz)
    END
  END ExcitedInserter;

PROCEDURE GridHighlight (hl: VBT.T; op: PaintOp.T; p: Point.T; hor, ver: INTEGER) =
  (* highlight a hor by ver rectangle centered at p, but reduce its size so
     that its borders fall on the grid lines. *)

  PROCEDURE F (n: CARDINAL): INTEGER =
    (* greatest integer at most n congruent to 1 MOD 16 *)
    BEGIN
      RETURN ((n - 1) DIV 16) * 16 + 1
    END F;

  VAR r := Center(Rect.FromSize(F(hor), F(ver)), p);
  BEGIN
    HighlightVBT.SetTexture(hl, Grid, Rect.NorthWest(r), op);
    HighlightVBT.SetRect(hl, r, 99999)
  END GridHighlight;

PROCEDURE Center (READONLY r: Rect.T; p: Point.T): Rect.T =
  (* Like Rect.Center, but produces a rectangle with north and west both
     even, so that the grid texture will look black over the Trestle
     background grey. Assumes both r's dimensions are odd. *)
  BEGIN
    IF Word.And(p.h, 1) = 1 THEN DEC(p.h) END;
    IF Word.And(p.v, 1) = 1 THEN DEC(p.v) END;
    WITH
      h = p.h - ((r.west + r.east) DIV 2),
      v = p.v - ((r.north + r.south) DIV 2) 
    DO
      RETURN Rect.MoveHV(r, h, v)
    END
  END Center;

VAR
  rsrcMu                 := NEW(MUTEX);
  rsrcInit               := FALSE;
  MovingCursor: Cursor.T;
  Grid: Pixmap.T;
  
PROCEDURE GetResources () =
  BEGIN
    LOCK rsrcMu DO
      IF rsrcInit THEN RETURN END;
      MovingCursor := Cursor.FromName(ARRAY OF TEXT{"XC_fleur"});
      Grid := VBTKitResources.GetPixmap("Grid");
      rsrcInit := TRUE;
    END
  END GetResources;


BEGIN
END SourceVBT.
