(* Copyright 1992 Digital Equipment Corporation.           *)
(* Distributed only by permission.                         *)
(* Last modified on Wed Feb  8 16:41:05 PST 1995 by kalsow  *)
(*      modified on Thu Jan  5 22:34:34 PST 1995 by najork  *)
(*      modified on Wed Jan  6 15:16:54 PST 1993 by steveg  *)
(*      modified on Tue Sep 15 02:35:55 PDT 1992 by mhb     *)

MODULE ViewMatch;

<* PRAGMA LL *>

IMPORT Animate, Axis, Fmt, Font, MG, MGPublic, MGV, Pts,
       Rect, R2, Match, MatchIE, MatchSelector, MatchViewClass, Thread,
       VBT, View, ZeusPanel;

REVEAL
  T = MatchViewClass.T BRANDED OBJECT
        mgv  : MGV.V;
        hides: ARRAY [1 .. 16] OF MG.Rectangle;
        clues: ARRAY [1 .. 16] OF MG.Rectangle;
      OVERRIDES
        shape      := Shape;
        startrun   := Startrun;
        reactivity := Reactivity;
        oeSetState := SetState;
        oeInit     := Init;
      END;

PROCEDURE Startrun (t: T) =
  VAR v := t.mgv;
  BEGIN
    LOCK v.mu DO
      v.displayList := NEW(MG.Group).init(3 * 16);
    END;
  END Startrun;

PROCEDURE Reactivity (<* UNUSED *> t: T; <* UNUSED *> on: BOOLEAN) =
  BEGIN
  END Reactivity;

PROCEDURE Shape (v: T; ax: Axis.T; <* UNUSED *> n: CARDINAL): VBT.SizeRange =
  BEGIN
    IF ax = Axis.T.Hor THEN
      WITH sz = Pts.ToScreenPixels(v, 4.0 * WidthPts, ax) DO
        RETURN VBT.SizeRange{sz, sz, sz + 1}
      END;
    ELSE
      WITH sz = Pts.ToScreenPixels(v, 4.0 * HeightPts, ax) DO
        RETURN VBT.SizeRange{sz, sz, sz + 1}
      END;
    END;
  END Shape;

PROCEDURE Selectee (<* UNUSED *>          s : MGV.Selectee;
                                          v : MG.V;
                                          t : MG.T;
                                 READONLY cd: VBT.MouseRec  ) =
  VAR realID: INTEGER;
  BEGIN
    IF t.id > 100 THEN
      realID := t.id MOD 100
    ELSE
      realID := 0
    END;
    TRY
      MatchIE.Selected(v.view, realID, cd);
    EXCEPT
      Thread.Alerted =>
    END;
  END Selectee;

CONST
  HeightPts = 100.0;
  WidthPts = 100.0;

VAR
  msg := ARRAY [0..3] OF TEXT {
                 "SRC 1st Annual",
                 "ANIMATION FESTIVAL",
                 "July 20-31, 1992",
                 "Don't miss it!"};
  msgColor := MGPublic.ColorFromText("LightBlue");
  msgFont := Font.FromName (ARRAY [0..0] OF TEXT{"*helvetica*240*"});
  clueColor := MGPublic.ColorFromText("LightRed");
  clueFont := Font.FromName (ARRAY [0..0] OF TEXT{"*helvetica*-r-*140*"});
  hideColor:= MGPublic.ColorFromText("LightGreen");
  hideFont := Font.FromName (ARRAY[0..0] OF TEXT{"*helvetica*180*"});

CONST
  Border = 3.0;

PROCEDURE NW (i: INTEGER): R2.T =
  BEGIN
    WITH row = (i - 1) DIV 4,
         col = (i - 1) MOD 4  DO
      RETURN R2.T{FLOAT(col) * WidthPts + Border,
                  FLOAT(4 - row) * HeightPts - Border}
    END
  END NW;

PROCEDURE SE (i: INTEGER): R2.T =
  BEGIN
    WITH row = (i - 1) DIV 4,
         col = (i - 1) MOD 4  DO
      RETURN R2.T{FLOAT(col+1) * WidthPts - Border,
                  FLOAT(4 - row - 1) * HeightPts + Border}
    END
  END SE;

PROCEDURE S (k: INTEGER): Sides =
  VAR sides:= Sides{};
  BEGIN
    DEC(k);
    IF k MOD 2 = 1 THEN sides := sides + Sides{Side.Top} END;
    IF (k DIV 2) MOD 2 = 1 THEN
      sides := sides + Sides{Side.Bottom}
    END;
    IF (k DIV 4) MOD 2 = 1 THEN
      sides := sides + Sides{Side.Left}
    END;
    IF (k DIV 8) MOD 2 = 1 THEN
      sides := sides + Sides{Side.Right}
    END;
    IF sides = Sides{} THEN
      sides := Sides{Side.Left.. Side.Bottom}
    END;
    RETURN sides;
  END S;

PROCEDURE Init (t: T; READONLY clues: Match.Clues) =
  CONST MsgHeight = 4.0 / FLOAT(NUMBER(msg)) * HeightPts;
  VAR v := t.mgv;
  BEGIN
    MGPublic.ResetLookups(v);
    FOR i := 0 TO LAST(msg) DO
      WITH nw = R2.T{0.0, FLOAT(NUMBER(msg) - i) * MsgHeight},
           se = R2.T{4.0 * WidthPts,
                     nw[1] - MsgHeight} DO
        EVAL
          NEW(MG.Rectangle, id := i, color := msgColor,
              font := msgFont, label := msg[i]).init(nw, se, v);
      END;
    END;
    FOR k := 1 TO 16 DO
      t.clues[k] := NEW(MG.Rectangle, id := 100 + k,
                        appearance := NEW(ClippedAppearance, sides := S(k)),
                        color := clueColor, font := clueFont,
                        label := clues[k]).init(NW(k), SE(k), v);
    END;
    FOR k := 1 TO 16 DO
      t.hides[k] :=
        NEW(MG.Rectangle, id := 200 + k, color := hideColor,
            font := hideFont, label := Fmt.Int(k)).init(
          NW(k), SE(k), v);
    END;
    VBT.Mark(v);
  END Init;

<* UNUSED *>
PROCEDURE oldSetState (t: T; i,j: INTEGER; state: Match.State) =
  VAR v := t.mgv;
  BEGIN
    CASE state OF
    | Match.State.Hide =>
        (* restore t.hides[i] *)
        v.displayList.addBefore (v, t.hides[i]);
        IF j # i THEN v.displayList.addBefore (v, t.hides[j]) END
    | Match.State.Clue =>
        (* get rid of t.hides[i] *)
        v.displayList.remove (v, t.hides[i]);
        IF j # i THEN v.displayList.remove (v, t.hides[j]) END
    | Match.State.Reveal => 
        (* t.hides[i] already gone; get rid of t.clues[i] *)
        v.displayList.remove (v, t.clues[i]);
        IF j # i THEN 
          v.displayList.remove (v, t.clues[j]) 
        END
    END;
    VBT.Mark(v);
  END oldSetState;

PROCEDURE SetState (t: T; i, j: INTEGER; state: Match.State) 
    RAISES {Thread.Alerted} =
  VAR v := t.mgv;
  BEGIN
    CASE state OF
    | Match.State.Hide =>
        (* restore t.hides[i] *)
        Grow(v, i, t.hides[i]);
        IF j # i THEN Grow(v, j, t.hides[j]) END
    | Match.State.Clue =>
        (* get rid of t.hides[i] *)
        Shrink(v, i, t.hides[i]);
        IF j # i THEN Shrink(v, j, t.hides[j]) END
    | Match.State.Reveal =>
        (* t.hides[i] already gone; get rid of t.clues[i] *)
        AnimDo(v, i, t.clues[i]);
        IF j # i THEN AnimDo(v, j, t.clues[j]) END
    END;
    MGV.Animation(v)
  END SetState;


TYPE
  MoveCorners = Animate.T OBJECT
                  cell: INTEGER;
                OVERRIDES
                  length := Length;
                  doStep := DoStep;
                END;

PROCEDURE Length (<* UNUSED *> t : MoveCorners;
                               v : MG.V;
                  <* UNUSED *> mg: MG.T         ): INTEGER =
  BEGIN
    RETURN Pts.ToScreenPixels(v, HeightPts / 2.0, Axis.T.Ver);
  END Length;

CONST
  InnerW = WidthPts - Border - Border;
  InnerH = HeightPts - Border - Border;

PROCEDURE DoStep (             t       : MoveCorners;
                               time    : REAL;
                  <* UNUSED *> timePrev: REAL;
                               v       : MG.V;
                               mg      : MG.T           ) =
  BEGIN
    WITH nw = NW(t.cell),
         se = SE(t.cell)  DO
      NARROW(mg, MG.Rectangle).reshape(
        v,
        R2.Add(nw, R2.Scale(time / 2.0, R2.T{InnerW, -InnerH})),
        R2.Add(se, R2.Scale(time / 2.0, R2.T{-InnerW, InnerH})));
    END;
  END DoStep;

PROCEDURE Shrink (v: MGV.V; i: INTEGER; cell: MG.Rectangle) =
  BEGIN
    WITH anim = NEW(MoveCorners, cell := i).init() DO
      MGV.AddAnimation(v, anim, cell)
    END
  END Shrink;

PROCEDURE Grow (v: MGV.V; i: INTEGER; cell: MG.Rectangle) =
  BEGIN
    WITH anim = NEW(MoveCorners, cell := i).init(
                  Animate.tfInverse) DO
      MGV.AddAnimation(v, anim, cell)
    END
  END Grow;


TYPE
  MoveSide = MoveCorners OBJECT
    side: Side;
  OVERRIDES
    doStep := DoStepMoveSide;
  END;

PROCEDURE DoStepMoveSide (     t       : MoveSide;
                               time    : REAL;
                  <* UNUSED *> timePrev: REAL;
                               v       : MG.V;
                               mg      : MG.T      ) =
  VAR
    nw := NW(t.cell);
    se := SE(t.cell);
  BEGIN
    CASE t.side OF
    | Side.Top => nw[1] := nw[1] - time * InnerH;
    | Side.Bottom => se[1] := se[1] + time * InnerH;
    | Side.Left => nw[0] := nw[0] + time * InnerW;
    | Side.Right => se[0] := se[0] - time * InnerW;
    ELSE <* ASSERT FALSE *>
    END;
    NARROW(mg, MG.Rectangle).reshape(v, nw, se);
  END DoStepMoveSide;


VAR
  animHighlight := NEW(Animate.Highlight).init();

PROCEDURE AnimDo (             v   : MGV.V;
                  <* UNUSED *> i   : INTEGER;
                               cell: MG.Rectangle) =
  BEGIN
    MGV.AddAnimation(v, animHighlight, cell)
  END AnimDo;



TYPE
  Side = {Left, Right, Top, Bottom};
  Sides = SET OF Side;

CONST
  LeftRight = Sides{Side.Left,Side.Right};
  TopBottom = Sides{Side.Top,Side.Bottom};

TYPE
  ClippedAppearance = MG.AppearanceDefault OBJECT
                        sides: Sides := Sides{Side.Top};
                      OVERRIDES
                        paint := ClippedPaint
                      END;

PROCEDURE ClippedPaint (    self: ClippedAppearance;
                            t   : MG.T;
                            v   : MG.V;
                        VAR clip: Rect.T             ) =
  VAR
    rect           := t.rgn(v).r;
    wd             := FLOAT(Rect.HorSize(rect));
    ht             := FLOAT(Rect.VerSize(rect));
    hscale, vscale := 1.0;
  BEGIN
    IF LeftRight = LeftRight * self.sides THEN hscale := 0.5 END;
    IF TopBottom = TopBottom * self.sides THEN vscale := 0.5 END;
    IF Side.Top IN self.sides THEN
      rect.north :=
        rect.north + ROUND(vscale * t.highlight * ht);
    END;
    IF Side.Bottom IN self.sides THEN
      rect.south :=
        rect.south - ROUND(vscale * t.highlight * ht);
    END;
    IF Side.Left IN self.sides THEN
      rect.west := rect.west + ROUND(hscale * t.highlight * wd);
    END;
    IF Side.Right IN self.sides THEN
      rect.east := rect.east - ROUND(hscale * t.highlight * wd);
    END;
    rect := Rect.Meet(clip, rect);
    MG.AppearanceDefault.paint(self, t, v, rect);
  END ClippedPaint;


PROCEDURE New (): View.T =
  VAR v := NEW(T);
  BEGIN
    v.mgv :=
      NEW(MGV.V, view := v,
          selectee := NEW(MGV.Selectee, select := Selectee),
          selector := MatchSelector.closest).init();
    RETURN v.init(v.mgv);
  END New;

BEGIN
  ZeusPanel.RegisterView (New, "Game Board", "Match");
END ViewMatch.
