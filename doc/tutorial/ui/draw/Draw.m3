MODULE Draw EXPORTS Main;
IMPORT Trestle, VBT, TextVBT, HVSplit, RigidVBT, BorderedVBT;
IMPORT Region, Axis, PaintOp, Path, Point, Rect;
FROM Colors IMPORT red, white, blue;

TYPE
  TexasVBT = VBT.Leaf OBJECT
               path: Path.T := NIL;
             METHODS
               init (): TexasVBT := Init
             OVERRIDES
               repaint := Repaint;
             END;

PROCEDURE Init (v: TexasVBT): TexasVBT =
  BEGIN
    v.path := NEW(Path.T);
    Path.Reset(v.path);
    Path.MoveTo(v.path, Point.T{0, 0});
    Path.LineTo(v.path, Point.T{-18, -18});
    Path.LineTo(v.path, Point.T{-22, -14});
    Path.LineTo(v.path, Point.T{-40, -32});
    Path.LineTo(v.path, Point.T{-24, -32});
    Path.LineTo(v.path, Point.T{-24, -52});
    Path.LineTo(v.path, Point.T{2, -52});
    Path.LineTo(v.path, Point.T{2, -42});
    Path.LineTo(v.path, Point.T{12, -42});
    Path.CurveTo(v.path, Point.T{12, -12}, Point.T{0, -30}, Point.T{0, 0});
    RETURN (v);
  END Init;

PROCEDURE Repaint (v: TexasVBT; READONLY rgn: Region.T) =
  PROCEDURE Pair (READONLY row, col: INTEGER) =
    VAR
      dom := VBT.Domain(v);
      mid := Rect.Middle(dom);
      x   := row * 44 + col * 52 + 10;
      y   := -row * 62 - col * 10;
      or1 := Point.MoveHV(mid, x, y);
      or2 := Point.MoveHV(mid, x + 52, y - 10);
    BEGIN
      VBT.Fill(v, dom, Path.Translate(v.path, or1), op := red);
      VBT.Stroke(
        v, dom, Path.Translate(v.path, or2), width := 2, op := blue);
    END Pair;
  BEGIN
    VBT.PaintRegion(v, rgn, white);
    FOR row := -2 TO 1 DO Pair(row, -2); Pair(row, 0); Pair(row, 2); END;
  END Repaint;
VAR
  main := BorderedVBT.New(
            RigidVBT.FromHV(NEW(TexasVBT).init(), 50.0, 50.0));
BEGIN
  Trestle.Install(main);
  Trestle.AwaitDelete(main);
END Draw.
