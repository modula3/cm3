MODULE Overlap EXPORTS Main;

IMPORT Trestle, ZChassisVBT, TextVBT, BorderedVBT, ZSplit, TextureVBT,
       PaintOp;
IMPORT Rect, Font, RigidVBT, ZChildVBT;

CONST
  min     = 100.0;               (* size in millimeters *)
  max     = 999.0;               (* size in millimeters *)
  h       = 100;                 (* width in pixels *)
  v       = 100;                 (* height in pixels *)
  times14 = "-*-times-*-r-*-*-14-*-*-*-*-*-*-*";

VAR
  font := Font.FromName(ARRAY OF TEXT{times14});
  ch1  := BorderedVBT.New(TextVBT.New("Hi Mom", fnt := font));
  zc1  := NEW(ZChassisVBT.T).init(ch1, TextVBT.New("Mom"));
  ch2  := BorderedVBT.New(TextVBT.New("Hi Dad", fnt := font));
  zc2  := NEW(ZChassisVBT.T).init(ch2, TextVBT.New("Dad"));
  main := ZSplit.New(RigidVBT.FromHV(
                       TextureVBT.New(PaintOp.Bg), min, min, max, max));
  dom := Rect.FromSize(h, v);
BEGIN
  ZSplit.Insert(main, zc1, dom);
  ZSplit.Insert(main, zc2, dom);
  Trestle.Install(main);
  Trestle.AwaitDelete(main)
END Overlap.
