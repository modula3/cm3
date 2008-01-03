MODULE Colors;

IMPORT Color, ColorName, PaintOp;

PROCEDURE FromName (name: TEXT): PaintOp.T =
  BEGIN
    RETURN FromRec(ColorName.ToRGB(name))
  END FromName;

PROCEDURE FromRec (color: Color.T): PaintOp.T =
  BEGIN
    RETURN PaintOp.FromRGB(color.r, color.g, color.b)
  END FromRec;

BEGIN
  white := FromName("white");
  royal := FromName("royal blue");
  navy := FromName("navy");
  red := FromName("red");
  lred := FromName("light red");
  dred := FromName("dark  red");
  blue := FromName("blue");
  lblue := FromName("light blue");
  dblue := FromName("dark  blue");
  grey := FromName("slate grey");
  lgrey := FromName("light slate grey");
  dgrey := FromName("dark  slate grey");
END Colors.
