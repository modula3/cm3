MODULE Scroll EXPORTS Main;

IMPORT Trestle, VBT, TextVBT, ScrollerVBT, Axis, PaintOp, Font;
IMPORT HVSplit, RigidVBT, FlexVBT;
IMPORT Latin1Key, Text, Fmt;

PROCEDURE Scroller (min, max, step: INTEGER): ScrollerVBT.T =
  VAR v := NEW(ScrollerVBT.T, callback := Degrees);
  BEGIN
    RETURN (v.init(Axis.T.Ver, min, max, quad, step));
  END Scroller;

PROCEDURE Degrees (v: ScrollerVBT.T; READONLY cd: VBT.MouseRec) =
  VAR
    deg := ScrollerVBT.Get(scr1);
    min := ScrollerVBT.Get(scr2);
    sec := ScrollerVBT.Get(scr3);
    cir := Text.FromChar(VAL(Latin1Key.degree, CHAR));
    text := Fmt.Int(deg) & cir & " " & Fmt.Int(min) & "' " & Fmt.Int(sec)
              & "\"";
  BEGIN
    TextVBT.Put(display, text);
  END Degrees;

CONST
  hMin    = 10.0;                (* size in millimeters *)
  vMin    = 50.0;                (* size in millimeters *)
  times14 = "-*-times-*-r-*-*-14-*-*-*-*-*-*-*";
VAR
  font    := Font.FromName(ARRAY OF TEXT{times14});
  display := TextVBT.New("", fnt := font);
  quad    := PaintOp.MakeColorQuad(PaintOp.Bg, PaintOp.Fg);
  scr1    := Scroller(-180, 180, 15);
  scr2    := Scroller(0, 59, 5);
  scr3    := Scroller(0, 59, 5);
  hor     := FlexVBT.Shape{FlexVBT.StretchyRange, FlexVBT.FixedRange};
  deg     := FlexVBT.New(TextVBT.New("deg", fnt := font), hor);
  min     := FlexVBT.New(TextVBT.New("min", fnt := font), hor);
  sec     := FlexVBT.New(TextVBT.New("sec", fnt := font), hor);
  col1 := RigidVBT.FromHV(HVSplit.Cons(Axis.T.Ver, deg, scr1), hMin, vMin);
  col2 := RigidVBT.FromHV(HVSplit.Cons(Axis.T.Ver, min, scr2), hMin, vMin);
  col3 := RigidVBT.FromHV(HVSplit.Cons(Axis.T.Ver, sec, scr3), hMin, vMin);
  scs  := HVSplit.Cons(Axis.T.Hor, col1, col2, col3);
  main := HVSplit.Cons(Axis.T.Ver, scs, display);
BEGIN
  ScrollerVBT.Put(scr1, 0);
  ScrollerVBT.Put(scr2, 0);
  ScrollerVBT.Put(scr3, 0);
  Trestle.Install(main);
  Trestle.AwaitDelete(main)
END Scroll.
