(* Copyright (C) 1991, Digital Equipment Corporation *)
(* Copyright 1990 David Lemke and Network Computing Devices *)
(* Copyright (c) 1989, Donald R. Woods and Sun Microsystems, Inc. *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)

(* Last modified on Tue Jan 31 10:27:31 PST 1995 by kalsow *)
(*      modified on Wed Jul 14 16:41:03 PDT 1993 by msm *)


MODULE Card;

IMPORT Point, VBT, ZSplit, Rect, Random,
         CardRank, CardSuit, FaceCards, Gray, HighlightVBT,
         Pixmap, Axis, PaintOp, Region, VBTRep, Filter,
         MouseSplit, Split, Thread, TwoTone;

CONST
  MidH   = Width DIV 2;
  MidV   = Height DIV 2;
  Mid    = Point.T{MidH, MidV};
  TenV1  = (3 * Height) DIV 10;
  TenV2  = Height - TenV1;
  SevenV = (7 * Height) DIV 20;
  EightV = Height - SevenV;
  Col1   = (3 * Width) DIV 10;
  Col3   = Width - Col1;
  Row1   = Height DIV 5;
  Row2   = (2 * Height) DIV 5;
  Row3   = MidV;
  Row4   = Height - Row2;
  Row5   = Height - Row1;
  RankH  = 4;
  RankV  = 7;
  SuitH  = RankH;
  SuitV  = 24;
  RankNW = Point.T{RankH, RankV};
  KnarNW = Point.T{Width - RankH, Height - RankV};
  SuitNW = Point.T{SuitH, SuitV};
  TiusNW = Point.T{Width - SuitH, Height - SuitV};
VAR
  realRed     := PaintOp.FromRGB(0.75, 0.0, 0.0, bw := PaintOp.BW.UseFg);
  red         := PaintOp.Pair(PaintOp.Bg, realRed);
  transpRed   := PaintOp.Pair(PaintOp.Transparent, realRed);
  redSwap     := PaintOp.SwapPair(PaintOp.Bg, realRed);
  black       := PaintOp.BgFg;
  transpBlack := PaintOp.TransparentFg;
  blackSwap   := PaintOp.Swap;
  realFelt    := PaintOp.FromRGB(0.2, 0.8, 0.6);
  feltSwap    := PaintOp.SwapPair(PaintOp.Fg, realFelt);
  back    := PaintOp.Pair(PaintOp.Bg, PaintOp.FromRGB(0.16, 0.30, 0.60));
  backPix := Pixmap.Gray;

REVEAL
  Private = HighlightVBT.T BRANDED OBJECT END;
  T = Public BRANDED OBJECT
        nexthigh: T := NIL;
      OVERRIDES
        mouse    := Mouse;
        position := Position
      END;

<*UNUSED*>
CONST
  Names = ARRAY Value OF
            TEXT{"Min", "A", "2", "3", "4", "5", "6", "7", "8", "9", "10",
                 "J", "Q", "K", "Max", "Talon"};
  Suits = ARRAY Family OF TEXT{"", "S", "H", "D", "C"};

VAR
  dragSource, dest: T   := NIL;
  high            : T   := NIL;
  killGen               := 0;
  tracking              := FALSE;
  highlight             := TRUE;
  chainTime             := 1000;
  log             : Log;

TYPE
  UndoRec = RECORD
              card, prevBelow, newBelow: T;
              userMade                 : BOOLEAN
            END;
  UndoLog = REF ARRAY OF UndoRec;
  Log = REF RECORD
              undo              : UndoLog;
              nextUndo, lastUndo: INTEGER
            END;

PROCEDURE StartUndoLog () =
  BEGIN
    IF log = NIL THEN log := NEW(Log) END;
    IF log.undo = NIL THEN log.undo := NEW(UndoLog, 10) END;
    log.nextUndo := 0;
    log.lastUndo := 0;
  END StartUndoLog;

PROCEDURE MoreUndo () =
  VAR u := NEW(UndoLog, 2 * NUMBER(log.undo^));
  BEGIN
    SUBARRAY(u^, 0, NUMBER(log.undo^)) := log.undo^;
    log.undo := u
  END MoreUndo;

PROCEDURE Undo (): BOOLEAN =
  <*FATAL BadDeal*>
  BEGIN
    IF log = NIL OR log.nextUndo = 0 THEN RETURN FALSE END;
    LOOP
      IF log.nextUndo = 0 THEN EXIT END;
      DEC(log.nextUndo);
      WITH u = log.undo[log.nextUndo] DO
        IF u.prevBelow = NIL THEN
          Flip(u.card, NOT u.card.faceUp);
        ELSE
          Attach(u.card, u.prevBelow);
        END;
        IF u.userMade THEN EXIT END
      END
    END;
    RETURN TRUE
  END Undo;

PROCEDURE Redo (slowly: BOOLEAN): BOOLEAN =
  <*FATAL BadDeal*>
  VAR c: T;
  BEGIN
    IF log = NIL OR log.nextUndo = log.lastUndo THEN RETURN FALSE END;
    LOOP
      WITH u = log.undo[log.nextUndo] DO
        c := u.card;
        IF u.newBelow = NIL THEN
          Flip(u.card, NOT u.card.faceUp);
        ELSE
          Attach(u.card, u.newBelow);
        END;
      END;
      INC(log.nextUndo);
      IF log.nextUndo = log.lastUndo THEN EXIT END;
      IF log.undo^[log.nextUndo].userMade THEN EXIT END;
      IF slowly THEN
        VBTRep.Redisplay();
        VBT.Sync(c);
        Thread.Pause(0.1D0)
      END
    END;
    RETURN TRUE
  END Redo;

PROCEDURE AddLog (cd, pBelow, nBelow: T; uMade: BOOLEAN) =
  BEGIN
    IF (log = NIL) OR (log.undo = NIL) THEN StartUndoLog() END;
    IF log.nextUndo = NUMBER(log.undo^) THEN MoreUndo() END;
    WITH u = log.undo[log.nextUndo] DO
      u.card := cd;
      u.prevBelow := pBelow;
      u.newBelow := nBelow;
      u.userMade := uMade
    END;
    INC(log.nextUndo);
    log.lastUndo := log.nextUndo
  END AddLog;

PROCEDURE Real (c: T): BOOLEAN =
  BEGIN
    RETURN (c.value # Value.Min) AND (c.value # Value.Max)
             AND (c.value # Value.Talon)
  END Real;

PROCEDURE RealCard (c: Card): BOOLEAN =
  BEGIN
    RETURN (c.value # Value.Min) AND (c.value # Value.Max)
             AND (c.value # Value.Talon)
  END RealCard;

PROCEDURE Shape (<*UNUSED*> ch: VBT.T;
                            ax: Axis.T;
                 <*UNUSED*> n : CARDINAL     ): VBT.SizeRange =
  BEGIN
    IF ax = Axis.T.Hor THEN
      RETURN VBT.SizeRange{lo := Width, pref := Width, hi := Width + 1}
    ELSE
      RETURN VBT.SizeRange{lo := Height, pref := Height, hi := Height + 1}
    END
  END Shape;

PROCEDURE KillHigh (VAR high: T) =
  VAR next: T;
  BEGIN
    WHILE high # NIL DO
      next := high.nexthigh;
      high.nexthigh := NIL;
      HighlightVBT.SetRect(high, Rect.Empty, 0);
      high := next
    END;
    INC(killGen)
  END KillHigh;

PROCEDURE AddHigh (VAR high: T; ch: T) =
  BEGIN
    IF ch = NIL OR ch.nexthigh # NIL OR ch = high THEN RETURN END;
    HighlightVBT.SetRect(ch, VBT.Domain(ch), MAX(Width, Height));
    ch.nexthigh := high;
    high := ch
  END AddHigh;

PROCEDURE Position (ch: T; READONLY cd: VBT.PositionRec) =
  BEGIN
    IF cd.cp.gone THEN
      IF (dragSource # NIL) AND (ch = dragSource) AND (dest # NIL) THEN
        dest := NIL;
        KillHigh(high)
      END;
      VBT.SetCage(ch, VBT.GoneCage)
    ELSE
      IF tracking AND cd.modifiers * VBT.Buttons = VBT.Modifiers{} THEN
        dragSource := ch;
        dest := obvious(dragSource);
        AddHigh(high, dest);
	IF chainTime >= 0 AND dest # NIL THEN
          EVAL Thread.Fork(NEW(Lumen, dest := dest, up := TRUE,
	    killGen := killGen, quick := FALSE))
        END
      END;
      VBT.SetCage(ch, VBT.CageFromRect(VBT.Domain(ch), cd.cp))
    END;
    MouseSplit.Position(ch, cd)
  END Position;

TYPE
  Lumen = Thread.Closure OBJECT
            dest   : T;
            up, quick: BOOLEAN;
            killGen: INTEGER
          OVERRIDES
            apply := LumenApply
          END;

PROCEDURE LumenApply (self: Lumen): REFANY =
  VAR timer: LONGREAL; BEGIN
    IF NOT self.quick THEN 
      LOCK VBT.mu DO timer := FLOAT(chainTime, LONGREAL) * 0.01D0 END;
      IF timer < 0.0D0 THEN RETURN NIL END;
      Thread.Pause(timer)
    END;
    LOOP
      LOCK VBT.mu DO
        timer := FLOAT(chainTime, LONGREAL) * 0.001D0
      END;
      IF timer < 0.0D0 THEN EXIT END;
      Thread.Pause(timer);
      LOCK VBT.mu DO
        IF killGen # self.killGen THEN EXIT END;
        IF self.up THEN
          self.dest := obvious(self.dest)
        ELSE
          self.dest := stupid(self.dest)
        END;
        IF self.dest = NIL THEN EXIT END;
        AddHigh(high, self.dest)
      END
    END;
    RETURN NIL
  END LumenApply;

PROCEDURE Mouse (ch: T; READONLY cd: VBT.MouseRec) =
  VAR prev, next, log: T;
  BEGIN
    IF cd.clickType = VBT.ClickType.FirstDown THEN
      dragSource := ch;
      CASE cd.whatChanged OF
        VBT.Modifier.MouseL => dest := obvious(dragSource)
      | VBT.Modifier.MouseM => dest := trivial(dragSource)
      | VBT.Modifier.MouseR => dest := stupid(dragSource)
      ELSE
        dest := NIL
      END;
      KillHigh(high);
      IF highlight THEN
        AddHigh(high, dest);
        IF dest # NIL AND (cd.whatChanged = VBT.Modifier.MouseL
                           OR cd.whatChanged = VBT.Modifier.MouseR)
		      AND chainTime >= 0 THEN
          EVAL Thread.Fork(NEW(Lumen, dest := dest,
                             up := cd.whatChanged = VBT.Modifier.MouseL,
                             killGen := killGen, quick := TRUE))
        END
      END
    ELSE
      KillHigh(high);
      prev := ch;
      IF (cd.clickType = VBT.ClickType.LastUp) AND (dragSource # NIL) THEN
        IF (prev = dragSource) AND NOT cd.cp.gone THEN prev := dest END;
        IF (prev = NIL) OR (prev = dragSource) THEN
          dragSource := NIL;
          RETURN
        END;
        TRY
          IF attachable(dragSource, prev) THEN
            IF Real(dragSource) THEN
              log := dragSource.below;
              Attach(dragSource, prev);
              AddLog(dragSource, log, prev, TRUE);
              VBTRep.Redisplay();
            END;
            WHILE play(prev, next) DO
              VBT.Sync(ch);
              IF prev = next THEN
                Flip(prev, NOT prev.faceUp);
                AddLog(prev, NIL, NIL, FALSE);
              ELSE
                log := prev.below;
                Attach(prev, next);
                AddLog(prev, log, next, FALSE);
                VBTRep.Redisplay()
              END;
            END;
          END
        EXCEPT
          BadDeal =>
        END
      END;
      dragSource := NIL
    END;
    MouseSplit.Mouse(ch, cd)
  END Mouse;

PROCEDURE NotAttachable ( <*UNUSED*>a, b: T): BOOLEAN =
  BEGIN
    RETURN FALSE
  END NotAttachable;

PROCEDURE NoPlay ( <*UNUSED*>VAR a, b: T): BOOLEAN =
  BEGIN
    RETURN FALSE
  END NoPlay;

PROCEDURE NoMove ( <*UNUSED*>a: T): T =
  BEGIN
    RETURN NIL
  END NoMove;

PROCEDURE Detach (c: T) =
  BEGIN
    IF c.below = c THEN RETURN END;
    c.below.above := c.above;
    c.above.below := c.below;
    c.above := c;
    c.below := c
  END Detach;

PROCEDURE AttachOne (c, p: T) =
  (* Put c on top of p *)
  VAR
    dom  : Rect.T;
    below: T;
  BEGIN
    Detach(c);
    dom := ZSplit.GetDomain(p);
    below := Bottom(p);
    IF (below.value # Value.Min) AND Real(p) THEN
      IF p.faceUp THEN
        dom := Rect.MoveV(dom, Overlap)
      ELSE
        dom := Rect.MoveV(dom, OverlapDown)
      END
    END;
    ZSplit.Move(c, dom);
    ZSplit.Lift(c);
    c.above := p.above;
    p.above := c;
    c.below := p;
    c.above.below := c
  END AttachOne;

PROCEDURE Attach (c, p: T) RAISES {BadDeal} =
  VAR above: T;
  BEGIN
    IF p.above = c OR p = c THEN RETURN END;
    IF Real(p.above) AND (p.above # p) OR Top(p) = Top(c) THEN
      RAISE BadDeal
    END;
    WHILE (p # c) AND Real(c) DO
      above := c.above;
      AttachOne(c, p);
      p := c;
      c := above
    END
  END Attach;

PROCEDURE InitializeStandardDeck (VAR deck: StandardDeck; zSplit: ZSplit.T) =
  VAR i: INTEGER;
  BEGIN
    i := 0;
    FOR val := Value.Ace TO Value.King DO
      FOR st := Family.Spades TO Family.Clubs DO
        deck[i] := New(val, st, Point.Origin, zSplit);
        INC(i)
      END
    END
  END InitializeStandardDeck;

PROCEDURE Flip (c: T; up: BOOLEAN) =
  VAR old: BOOLEAN;
  BEGIN
    old := c.faceUp;
    c.faceUp := up;
    IF old # up THEN FlipCard(Filter.Child(c), up) END
  END Flip;

PROCEDURE FlipCard (c: Card; up: BOOLEAN) =
  VAR old: BOOLEAN;
  BEGIN
    old := c.faceUp;
    c.faceUp := up;
    IF old # up THEN VBT.Mark(c) END
  END FlipCard;

PROCEDURE NewCard (value: Value; suit: Family; faceUp := TRUE): Card =
  BEGIN
    RETURN NEW(Card, value := value, family := suit, faceUp := faceUp)
  END NewCard;

PROCEDURE New (         val   : Value;
                        st    : Family;
               READONLY loc   : Point.T;
                        zSplit: ZSplit.T;
                        faced : BOOLEAN    := TRUE): T =
  VAR res := NEW(T, value := val, family := st, faceUp := faced);
  BEGIN
    IF NOT Real(res) THEN
      EVAL HighlightVBT.T.init(res, NIL, feltSwap, Pixmap.Solid)
    ELSIF st = Family.Hearts OR st = Family.Diamonds THEN
      EVAL HighlightVBT.T.init(res, NIL, redSwap, Pixmap.Solid)
    ELSE
      EVAL HighlightVBT.T.init(res, NIL, blackSwap, Pixmap.Solid)
    END;
    res.above := res;
    res.below := res;
    Realize(res, loc, zSplit);
    RETURN res
  END New;

PROCEDURE Realize (c: T; READONLY loc: Point.T; zSplit: ZSplit.T) =
  <*FATAL Split.NotAChild *>
  BEGIN
    Split.Insert(c, NIL, NEW(Card, round := TRUE, family := c.family,
                             value := c.value, faceUp := c.faceUp));
    ZSplit.InsertAt(zSplit, c, loc)
  END Realize;

REVEAL
  Card = CardPublic BRANDED OBJECT
           round := FALSE;
         OVERRIDES
           repaint := Repaint;
           shape   := Shape
         END;

PROCEDURE Repaint (v: Card; READONLY bad: Region.T) =
  BEGIN
    IF v.round THEN
      PaintBg(v, bad.r);
      PaintBorder(v, bad.r);
      PaintTopCorners(v, bad.r);
      PaintBottomCorners(v, bad.r)
    ELSE
      PaintSquareBg(v, bad.r);
      PaintSquareBorder(v, bad.r)
    END;
    IF RealCard(v) AND v.faceUp THEN
      PaintRankAndSuit(v, bad.r);
      PaintCenter(v, bad.r)
    END
  END Repaint;

PROCEDURE PaintCenter (v: Card; READONLY clip: Rect.T) =
  VAR
    op, top: PaintOp.T;
    delta              := Rect.NorthWest(VBT.Domain(v));
    pix                := CardSuit.PipPix(v.family);
    xip                := CardSuit.PipXip(v.family);
  BEGIN
    IF v.family = Family.Hearts OR v.family = Family.Diamonds THEN
      op := red;
      top := transpRed
    ELSE
      op := black;
      top := transpBlack
    END;
    IF v.value >= Value.Jack THEN
      BorderCenter(v, op, top, clip, FaceCards.Pix(v.family, v.value), Mid,
                   delta, pix, xip, v.family, v.value)
    ELSIF v.value >= Value.Four THEN
      Center(v, op, clip, pix, Point.T{Col1, Row1}, delta);
      Center(v, op, clip, pix, Point.T{Col3, Row1}, delta);
      Center(v, op, clip, xip, Point.T{Col1, Row5}, delta);
      Center(v, op, clip, xip, Point.T{Col3, Row5}, delta);
      IF v.value >= Value.Nine THEN
        Center(v, op, clip, pix, Point.T{Col1, Row2}, delta);
        Center(v, op, clip, pix, Point.T{Col3, Row2}, delta);
        Center(v, op, clip, xip, Point.T{Col1, Row4}, delta);
        Center(v, op, clip, xip, Point.T{Col3, Row4}, delta);
        IF v.value >= Value.Ten THEN
          Center(v, op, clip, pix, Point.T{MidH, TenV1}, delta);
          Center(v, op, clip, xip, Point.T{MidH, TenV2}, delta);
        ELSIF v.value >= Value.Nine THEN
          Center(v, op, clip, pix, Mid, delta);
        END
      ELSIF v.value >= Value.Six THEN
        Center(v, op, clip, pix, Point.T{Col1, Row3}, delta);
        Center(v, op, clip, pix, Point.T{Col3, Row3}, delta);
        IF v.value >= Value.Seven THEN
          Center(v, op, clip, pix, Point.T{MidH, SevenV}, delta);
          IF v.value >= Value.Eight THEN
            Center(v, op, clip, xip, Point.T{MidH, EightV}, delta);
          END
        END;
      ELSIF v.value >= Value.Five THEN
        Center(v, op, clip, pix, Mid, delta)
      END
    ELSIF v.value >= Value.Deuce THEN
      Center(v, op, clip, pix, Point.T{MidH, Row1}, delta);
      Center(v, op, clip, xip, Point.T{MidH, Row5}, delta);
      IF v.value >= Value.Three THEN
        Center(v, op, clip, pix, Mid, delta)
      END
    ELSE
      Center(v, op, clip, CardSuit.AcePix(v.family), Mid, delta)
    END
  END PaintCenter;

PROCEDURE Center (         v           : Card;
                           op          : PaintOp.T;
                  READONLY clip        : Rect.T;
                           pm          : Pixmap.T;
                  READONLY midpt, delta: Point.T    ) =
  VAR
    dom := VBT.PixmapDomain(v, pm);
    nw  := Point.Sub(Point.Add(delta, midpt), Rect.Middle(dom));
  BEGIN
    VBT.PaintPixmap(v, clip, op, pm, nw)
  END Center;

PROCEDURE BorderCenter (         v           : Card;
                                 op, top     : PaintOp.T;
                        READONLY clip        : Rect.T;
                                 pm          : Pixmap.T;
                        READONLY midpt, delta: Point.T;
                                 pix, xip    : Pixmap.T;
                                 s           : Suit;
                                 r           : FaceCards.FaceRank) =
  VAR
    dom            := VBT.PixmapDomain(v, pm);
    nw             := Point.Sub(Point.Add(delta, midpt), Rect.Middle(dom));
    border: Rect.T;
    a: Rect.Partition;
  BEGIN
    VBT.PaintPixmap(v, clip, op, pm, nw);
    Center(v, top, clip, pix, FaceCards.PixCenter(s, r), nw);
    Center(v, top, clip, xip, FaceCards.XipCenter(s, r), nw);
    dom := Rect.Move(dom, nw);
    border := Rect.Inset(dom, -1);
    Rect.Factor(Rect.Meet(clip, border), dom, a, 0, 0);
    a[2] := a[4];
    VBT.PolyTint(v, SUBARRAY(a, 0, 4), op)
  END BorderCenter;

PROCEDURE PaintRankAndSuit (v: Card; READONLY clip: Rect.T) =
  VAR
    op   : PaintOp.T;
    delta            := Rect.NorthWest(VBT.Domain(v));
    pix  : Pixmap.T;
  BEGIN
    IF v.family = Family.Hearts OR v.family = Family.Diamonds THEN
      op := red
    ELSE
      op := black
    END;
    VBT.PaintPixmap(
      v, clip, op, CardRank.Pix(v.value), Point.Add(delta, RankNW));
    pix := CardRank.Xip(v.value);
    VBT.PaintPixmap(v, clip, op, pix,
                    Point.Sub(Point.Add(delta, KnarNW),
                              Rect.SouthEast(VBT.PixmapDomain(v, pix))));
    IF v.family = Family.Diamonds THEN INC(delta.h) END;
    VBT.PaintPixmap(
      v, clip, op, CardSuit.RankPix(v.family), Point.Add(delta, SuitNW));
    pix := CardSuit.RankXip(v.family);
    VBT.PaintPixmap(v, clip, op, pix,
                    Point.Sub(Point.Add(delta, TiusNW),
                              Rect.SouthEast(VBT.PixmapDomain(v, pix))));
  END PaintRankAndSuit;

PROCEDURE TranslateAndClip (READONLY a    : ARRAY OF Rect.T;
                            VAR      b    : ARRAY OF Rect.T;
                            READONLY delta: Point.T;
                            READONLY clip : Rect.T           ): INTEGER =
  VAR
    j         := 0;
    r: Rect.T;
  BEGIN
    FOR i := FIRST(a) TO LAST(a) DO
      r := Rect.Meet(clip, Rect.Move(a[i], delta));
      IF NOT Rect.IsEmpty(r) THEN b[j] := r; INC(j) END
    END;
    RETURN j
  END TranslateAndClip;

PROCEDURE PaintTopCorners (v: Card; READONLY clip: Rect.T) =
  VAR
    t       : T         := VBT.Parent(v);
    op1, op2: PaintOp.T;
    txt     : Pixmap.T;
    bottom              := Bottom(t);
    a1: ARRAY [0 .. LAST(TopCornerRectsA)] OF Rect.T;
    a2: ARRAY [0 .. LAST(TopCornerRectsB)] OF Rect.T;
    n1 := TranslateAndClip(
            TopCornerRectsA, a1, Rect.NorthWest(VBT.Domain(v)), clip);
    n2 := TranslateAndClip(
            TopCornerRectsB, a2, Rect.NorthWest(VBT.Domain(v)), clip);
  BEGIN
    IF bottom = t OR NOT Real(t.below) OR bottom.value = Value.Min THEN
      op1 := felt.op;
      op2 := felt.op;
      txt := felt.txt;
    ELSIF t.below.faceUp THEN
      op1 := PaintOp.Bg;
      op2 := PaintOp.Fg;
      txt := Pixmap.Solid
    ELSE
      op1 := back;
      op2 := PaintOp.Fg;
      txt := backPix;
    END;
    IF n1 > 0 THEN
      IF txt = Pixmap.Solid THEN
        VBT.PolyTint(v, SUBARRAY(a1, 0, n1), op1)
      ELSE
        VBT.PolyTexture(v, SUBARRAY(a1, 0, n1), op1, txt,
                        Rect.NorthWest(VBT.Domain(t.below)))
      END
    END;
    IF n2 > 0 THEN VBT.PolyTint(v, SUBARRAY(a2, 0, n2), op2) END
  END PaintTopCorners;

PROCEDURE PaintBottomCorners (v: Card; READONLY clip: Rect.T) =
  VAR
    a: ARRAY [0 .. LAST(BottomCornerRects)] OF Rect.T;
    n := TranslateAndClip(
           BottomCornerRects, a, Rect.NorthWest(VBT.Domain(v)), clip);
  BEGIN
    IF n > 0 THEN
      VBT.PolyTexture(v, SUBARRAY(a, 0, n), felt.op, felt.txt)
    END
  END PaintBottomCorners;

PROCEDURE PaintBorder (v: Card; READONLY clip: Rect.T) =
  VAR
    a: ARRAY [0 .. LAST(BorderRects)] OF Rect.T;
    n := TranslateAndClip(
           BorderRects, a, Rect.NorthWest(VBT.Domain(v)), clip);
  BEGIN
    IF n > 0 THEN VBT.PolyTint(v, SUBARRAY(a, 0, n), PaintOp.Fg) END
  END PaintBorder;

PROCEDURE PaintSquareBorder (v: Card; READONLY clip: Rect.T) =
  VAR
    a: ARRAY [0 .. LAST(SquareBorderRects)] OF Rect.T;
    n := TranslateAndClip(
           SquareBorderRects, a, Rect.NorthWest(VBT.Domain(v)), clip);
  BEGIN
    IF n > 0 THEN VBT.PolyTint(v, SUBARRAY(a, 0, n), PaintOp.Fg) END
  END PaintSquareBorder;

PROCEDURE ComputeBg(v: Card; VAR txt: Pixmap.T; VAR op: PaintOp.T) =
  BEGIN
    IF NOT RealCard(v) THEN
      txt := felt.txt;
      op := felt.op
    ELSIF v.faceUp THEN
      txt := Pixmap.Solid;
      op := PaintOp.Bg
    ELSE
      txt := backPix;
      op := back
    END
  END ComputeBg;

PROCEDURE PaintBg (v: Card; READONLY clip: Rect.T) =
  VAR
    op : PaintOp.T;
    txt: Pixmap.T;
    a  : ARRAY [0 .. LAST(BgRects)] OF Rect.T;
    n  : INTEGER;
  BEGIN
    ComputeBg(v, txt, op);
    n := TranslateAndClip(BgRects, a, Rect.NorthWest(VBT.Domain(v)), clip);
    IF n > 0 THEN VBT.PolyTexture(v, SUBARRAY(a, 0, n), op, txt) END
  END PaintBg;

PROCEDURE PaintSquareBg (v: Card; READONLY clip: Rect.T) =
  VAR
    op : PaintOp.T;
    txt: Pixmap.T;
    a  : ARRAY [0 .. LAST(SquareBgRects)] OF Rect.T;
    n  : INTEGER;
  BEGIN
    ComputeBg(v, txt, op);
    n := TranslateAndClip(
           SquareBgRects, a, Rect.NorthWest(VBT.Domain(v)), clip);
    IF n > 0 THEN VBT.PolyTexture(v, SUBARRAY(a, 0, n), op, txt) END
  END PaintSquareBg;

PROCEDURE EnableTracking (enable: BOOLEAN) =
  BEGIN
    tracking := enable
  END EnableTracking;

PROCEDURE EnableHighlight (enable: BOOLEAN; chain: INTEGER) =
  BEGIN
    highlight := enable;
    chainTime := chain
  END EnableHighlight;

PROCEDURE Shuffle (VAR deck: ARRAY OF T) =
  VAR
    j: INTEGER;
    c: T;
  BEGIN
    FOR i := 0 TO LAST(deck) DO
      j := default.integer(i, LAST(deck));
      c := deck[i];
      deck[i] := deck[j];
      deck[j] := c
    END
  END Shuffle;

PROCEDURE Top (c: T): T =
  BEGIN
    RETURN Bottom(c).below
  END Top;

PROCEDURE Bottom (c: T): T =
  VAR d := c.below;
  BEGIN
    IF NOT Real(c) THEN RETURN c END;
    WHILE Real(d) AND d # c DO d := d.below END;
    RETURN d
  END Bottom;

CONST
  SquareBorderRects = ARRAY OF
                        Rect.T{Rect.T{0, 1, 0, Height},
                               Rect.T{Width - 1, Width, 0, Height},
                               Rect.T{1, Width - 1, 0, 1},
                               Rect.T{1, Width - 1, Height - 1, Height}};
  BorderRects = ARRAY OF
                  Rect.T{
                  Rect.T{4, Width - 4, 0, 1},
                  Rect.T{4, Width - 4, Height - 1, Height},
                  Rect.T{0, 1, 4, Height - 4},
                  Rect.T{Width - 1, Width, 4, Height - 4},
                  Rect.T{2, 4, 1, 2}, Rect.T{Width - 4, Width - 2, 1, 2},
                  Rect.T{2, 4, Height - 2, Height - 1},
                  Rect.T{Width - 4, Width - 2, Height - 2, Height - 1},
                  Rect.T{1, 2, 2, 4}, Rect.T{1, 2, Height - 4, Height - 2},
                  Rect.T{Width - 2, Width - 1, 2, 4},
                  Rect.T{Width - 2, Width - 1, Height - 4, Height - 2}};


  SquareBgRects = ARRAY OF Rect.T{Rect.T{1, Width - 1, 1, Height - 1}};

  BgRects = ARRAY OF
              Rect.T{
              Rect.T{4, Width - 4, 1, 2}, Rect.T{2, Width - 2, 2, 4},
              Rect.T{1, Width - 1, 4, Height - 4},
              Rect.T{2, Width - 2, Height - 4, Height - 2},
              Rect.T{4, Width - 4, Height - 2, Height - 1}};

  TopCornerRectsA = ARRAY OF
                      Rect.T{Rect.T{1, 4, 0, 1},
                             Rect.T{Width - 4, Width - 1, 0, 1},
                             Rect.T{1, 2, 1, 2},
                             Rect.T{Width - 2, Width - 1, 1, 2}};
  TopCornerRectsB = ARRAY OF
                      Rect.T{
                      Rect.T{0, 1, 0, 4}, Rect.T{Width - 1, Width, 0, 4}};

  BottomCornerRects = ARRAY OF
                        Rect.T{
                        Rect.T{0, 4, Height - 1, Height},
                        Rect.T{Width - 4, Width, Height - 1, Height},
                        Rect.T{0, 2, Height - 2, Height - 1},
                        Rect.T{Width - 2, Width, Height - 2, Height - 1},
                        Rect.T{0, 1, Height - 4, Height - 2},
                        Rect.T{Width - 1, Width, Height - 4, Height - 2}};

VAR
  default := NEW(Random.Default).init();

BEGIN
  attachable := NotAttachable;
  play := NoPlay;
  obvious := NoMove;
  trivial := NoMove;
  stupid := NoMove;
  felt := TwoTone.New(PaintOp.Pair(PaintOp.Bg, realFelt), Gray.New4x4(6));
END Card.
