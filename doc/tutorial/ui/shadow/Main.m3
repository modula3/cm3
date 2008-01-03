MODULE Main;
IMPORT Trestle, VBT, ShadowedVBT, ButtonVBT, TextVBT, RigidVBT;
IMPORT HVSplit, Axis, Shadow, PaintOp, Region;
FROM Colors IMPORT grey, dgrey, lgrey;

TYPE
  TintVBT = VBT.Leaf OBJECT
              tint: PaintOp.T := PaintOp.Bg;
            OVERRIDES
              repaint := Repaint;
            END;

PROCEDURE Repaint (v: TintVBT; READONLY rgn: Region.T) RAISES {} =
  BEGIN
    VBT.PaintTint(v, rgn.r, v.tint);
  END Repaint;

PROCEDURE NoAction (v: ButtonVBT.T; READONLY cd: VBT.MouseRec) =
  BEGIN (* Do nothing *)
  END NoAction;

PROCEDURE Button (text: TEXT): ButtonVBT.T =
  BEGIN
    RETURN (ButtonVBT.New(TextVBT.New(text), NoAction));
  END Button;

CONST
  flex  = RigidVBT.SizeRange{lo := 0.0, pref := 8.0, hi := 9999.0};
  shape = RigidVBT.Shape{flex, flex};

VAR
  sh := Shadow.New(size := 7.0, bg := grey, light := lgrey, dark := dgrey);
  flat  := ShadowedVBT.New(Button("FlatStyle"), sh, Shadow.Style.Flat);
  raise := ShadowedVBT.New(Button("RaisedStyle"), sh, Shadow.Style.Raised);
  lower := ShadowedVBT.New(
             Button("LoweredStyle"), sh, Shadow.Style.Lowered);
  ridge := ShadowedVBT.New(Button("RidgedStyle"), sh, Shadow.Style.Ridged);
  chisel := ShadowedVBT.New(
              Button("ChiseledSTyle"), sh, Shadow.Style.Chiseled);

  l := NEW(RigidVBT.T).init(NEW(TintVBT, tint := grey), shape);
  m := NEW(RigidVBT.T).init(NEW(TintVBT, tint := grey), shape);
  u := NEW(RigidVBT.T).init(NEW(TintVBT, tint := grey), shape);
  v := NEW(RigidVBT.T).init(NEW(TintVBT, tint := grey), shape);
  w := NEW(RigidVBT.T).init(NEW(TintVBT, tint := grey), shape);
  x := NEW(RigidVBT.T).init(NEW(TintVBT, tint := grey), shape);
  y := NEW(RigidVBT.T).init(NEW(TintVBT, tint := grey), shape);
  z := NEW(RigidVBT.T).init(NEW(TintVBT, tint := grey), shape);

  ch := ARRAY [1 .. 11] OF
          VBT.T{u, flat, v, raise, w, lower, x, ridge, y, chisel, z};
  main := HVSplit.Cons(Axis.T.Hor, l, HVSplit.ConsArray(Axis.T.Ver, ch), m);

BEGIN
  Trestle.Install(main);
  Trestle.AwaitDelete(main);
END Main.

