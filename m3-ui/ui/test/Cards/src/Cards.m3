(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman, Mark Manasse and Greg Nelson           *)
(* Last modified on Mon Feb 24 14:01:27 PST 1992 by muller   *)
(*      modified on Tue Nov 19  0:10:04 PST 1991 by gnelson  *)
(*      modified on Wed Sep 11 15:54:32 PDT 1991 by msm      *)
<*PRAGMA LL*>

MODULE Cards EXPORTS Main;

IMPORT PaintOp, RigidVBT, TextureVBT, BorderedVBT, VBT, ZSplit, Rect, 
    Point, Trestle, Filter, VBTClass, HighlightVBT, PlaidVBT, Pixmap,
    MouseSplit, EyesVBT, ButtonVBT, HVSplit, AnchorBtnVBT,
    TextVBT, Axis, Text, Split, MenuBtnVBT, BurmaShave, TrestleComm;

TYPE Card = BorderedVBT.T OBJECT
  METHODS
    init(cq: PaintOp.ColorQuad; type: CardType): Card := Init;
  OVERRIDES
    mouse := Mouse;
    position := Position;
  END;

TYPE CardType = {Solid, Plaid, Eye, Billboard};

PROCEDURE Init(card: Card; cq: PaintOp.ColorQuad; type: CardType): Card =
    VAR ch: VBT.T;
  BEGIN
    IF type = CardType.Plaid THEN
      ch := RigidVBT.FromHV(NEW(PlaidVBT.T), 35.0, 23.0)
    ELSIF type = CardType.Solid THEN
      ch := RigidVBT.FromHV(
        TextureVBT.New(cq.bg, Pixmap.Solid, TRUE), 23.0, 35.0)
    ELSIF type = CardType.Billboard THEN
      ch := RigidVBT.FromHV(BurmaShave.New(), 23.0, 15.0)
    ELSE
      ch := RigidVBT.FromHV(NEW(EyesVBT.T), 23.0, 30.0)
    END;
    EVAL BorderedVBT.T.init(card, ch, op := cq.fg);
    RETURN card
  END Init;

TYPE Parent = ZSplit.T OBJECT
    dragChild: Card := NIL;
    rect := Rect.Empty;
    pt: Point.T
  END;

PROCEDURE Mouse(ch: Card; READONLY cd: VBT.MouseRec) =
  VAR p : Parent := VBT.Parent(ch);
  BEGIN
    (* BorderedVBT.T.mouse *) MouseSplit.Mouse(ch, cd);
    IF cd.clickType = VBT.ClickType.FirstDown AND 
       cd.whatChanged = VBT.Modifier.MouseL THEN
      p.dragChild := ch;
      p.rect := VBT.Domain(ch);
      p.pt := cd.cp.pt;
      ZSplit.Lift(ch);
      VBT.SetCage(ch, VBT.CageFromPosition(cd.cp));
        HighlightVBT.SetRect(p, p.rect, 3)
    ELSIF p.dragChild = ch THEN
      IF cd.clickType = VBT.ClickType.LastUp THEN
        ZSplit.Move(ch, p.rect)
      END;
      p.dragChild := NIL;
      HighlightVBT.SetRect(p, Rect.Empty, 0)
    END
  END Mouse;

PROCEDURE Position(ch: Card; READONLY cd: VBT.PositionRec) =
  VAR p : Parent := VBT.Parent(ch);
  BEGIN
    (* BorderedVBT.T.position *) MouseSplit.Position(ch, cd);
    IF p # NIL AND p.dragChild = ch THEN
      VBT.SetCage(ch, VBT.CageFromPosition(cd.cp, TRUE));
      IF NOT cd.cp.offScreen AND Rect.Member(cd.cp.pt, VBT.Domain(p)) THEN
        p.rect := Rect.Add(p.rect, Point.Sub(cd.cp.pt, p.pt));
        p.pt := cd.cp.pt;
        HighlightVBT.SetRect(p, p.rect, 3)
      END
    END;
  END Position;

TYPE ClrRec = RECORD cq: PaintOp.ColorQuad; name: TEXT END;

VAR clr := ARRAY [0..7] OF ClrRec {
  NewCR(0.0, 0.0, 0.0, "black"),
  NewCR(1.0, 1.0, 1.0, "white"),
  NewCR(0.0, 0.0, 1.0, "blue"),
  NewCR(0.0, 1.0, 0.0, "green"),
  NewCR(1.0, 0.0, 0.0, "red"),
  NewCR(0.0, 1.0, 1.0, "cyan"),
  NewCR(1.0, 1.0, 0.0, "yellow"),
  NewCR(1.0, 0.0, 1.0, "magenta")};

PROCEDURE NewCR(r, g, b: REAL; name: TEXT) : ClrRec = 
  BEGIN
    RETURN ClrRec{
      PaintOp.MakeColorQuad(PaintOp.FromRGB(r,g,b), PaintOp.Fg), 
      name}
  END NewCR;

PROCEDURE DoNewChild(b: ButtonVBT.T; <*UNUSED*> READONLY cd: VBT.MouseRec) =
  VAR name := TextVBT.Get(Filter.Child(b)); card: Card; BEGIN
    IF Text.Equal(name, "plaid") THEN
      card := NEW(Card).init(PaintOp.bgFg, CardType.Plaid)
    ELSIF Text.Equal(name, "eye") THEN
      card := NEW(Card).init(PaintOp.bgFg, CardType.Eye)
    ELSIF Text.Equal(name, "shave") THEN
      card := NEW(Card).init(PaintOp.bgFg, CardType.Billboard)
    ELSE
      FOR i := FIRST(clr) TO LAST(clr) DO
        IF Text.Equal(name, clr[i].name) THEN
          card := NEW(Card).init(clr[i].cq, CardType.Solid);
          EXIT
        END
      END
    END;
    ZSplit.InsertAt(zSplit, card, 
      Point.Add(Rect.NorthWest(VBT.Domain(zSplit)),
                Point.T{75, 5}))
  END DoNewChild;
    
PROCEDURE DoErase(<*UNUSED*>b: ButtonVBT.T; <*UNUSED*>READONLY cd: VBT.MouseRec) =
  <* FATAL Split.NotAChild *>
  VAR ch := Split.Succ(zSplit, NIL); BEGIN
    WHILE ch # NIL DO
      VAR chP := ch; BEGIN
        ch := Split.Succ(zSplit, ch);
        IF ISTYPE(chP, Card) THEN
          Split.Delete(zSplit, chP)
        END
      END
    END
  END DoErase;

PROCEDURE Menu1(): HVSplit.T = 
  VAR res := HVSplit.New(Axis.T.Ver); BEGIN
    FOR i := FIRST(clr) TO LAST(clr) DO
      Split.AddChild(res,
        MenuBtnVBT.TextItem(clr[i].name, DoNewChild))
    END;
    Split.AddChild(res, 
        MenuBtnVBT.TextItem("plaid", DoNewChild));
    Split.AddChild(res, 
        MenuBtnVBT.TextItem("eye", DoNewChild));
    Split.AddChild(res, 
        MenuBtnVBT.TextItem("shave", DoNewChild));
    Split.AddChild(res, 
        MenuBtnVBT.TextItem("erase", DoErase));
    RETURN res
  END Menu1;

VAR zSplit := NEW(Parent);

    menuBar := ButtonVBT.MenuBar(
      AnchorBtnVBT.New(
        TextVBT.New("new"),
        BorderedVBT.New(Menu1())));

    main := HVSplit.Cons(Axis.T.Ver,
      menuBar,
      HighlightVBT.New(zSplit));

<*FATAL TrestleComm.Failure*>

BEGIN
  EVAL ZSplit.T.init(zSplit, TextureVBT.New(PaintOp.Bg));
  Trestle.Install(main);
  Trestle.AwaitDelete(main);
END Cards.
