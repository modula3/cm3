GENERIC MODULE PolarFmtLex(R,RF);
(*Copyright (c) 1996, m3na project

Abstract: Complex numbers in polar coordinates

12/13/95  Harry George    Initial version
1/27/96   Harry George    Converted to m3na format
2/3/96    Harry George    Added trancendentals
2/17/96   Harry George    Converted from Objects to ADT's
3/16/96   Warren Smith    Improved routines, and new routines.
                          The ones with beginning caps are wds's
*)

FROM FmtLexSupport IMPORT Precedence, Parenthesize;

<*UNUSED*> CONST Module = "PolarFmtLex.";

(*----------------*)
PROCEDURE Fmt (READONLY x : T; READONLY style := FmtStyle{}) : TEXT =
VAR
  t:TEXT;
BEGIN
  t:="Polar{radius:=" & RF.Fmt(x.radius,style.elemStyle) & ","
         & "angle:="  & RF.Fmt(x.angle, style.elemStyle) & "}";
  RETURN t;
END Fmt;

PROCEDURE Tex (READONLY x : T; READONLY style := TexStyle{}; within := Precedence.sum) : TEXT =
VAR
  t:TEXT;
BEGIN
  IF R.IsZero(x.radius) OR R.IsZero(x.angle) THEN
    t:=RF.Tex(x.radius,style.elemStyle,within);
  ELSE
    t:=Parenthesize(RF.Tex(x.radius,style.elemStyle,Precedence.product) & " \\cdot e^{" &
                    RF.Tex(x.angle, style.elemStyle,Precedence.sum) & " i}",
                    Precedence.product,within);
  END;
  RETURN t;
END Tex;

(*==========================*)
BEGIN
END PolarFmtLex.
