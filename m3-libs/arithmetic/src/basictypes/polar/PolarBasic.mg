GENERIC MODULE PolarBasic(R, RT, C, CT);
(* Arithmetic for Modula-3, see doc for details

   Abstract: Complex numbers in polar coordinates *)

FROM Arithmetic IMPORT Error;

<* UNUSED *>
CONST
  Module = "PolarBasic.";


PROCEDURE IsZero (READONLY x: T; ): BOOLEAN =
  BEGIN
    RETURN R.IsZero(x.radius);
  END IsZero;


PROCEDURE Equal (READONLY x, y: T; ): BOOLEAN =
  BEGIN
    RETURN R.Equal(x.radius, y.radius) AND R.Equal(x.angle, y.angle);
  END Equal;


PROCEDURE FromComplex (READONLY c: C.T; ): T =
  VAR x: T;
  BEGIN
    x.radius := CT.Abs(c);
    x.angle := RT.ArcTan2(c.im, c.re);
    RETURN x;
  END FromComplex;


PROCEDURE ToComplex (READONLY x: T; ): C.T =
  BEGIN
    RETURN
      C.Scale(C.T{re := RT.Cos(x.angle), im := RT.Sin(x.angle)}, x.radius);
  END ToComplex;

PROCEDURE NormalizeAngle (READONLY x: T; ): T =
  VAR angle := x.angle;
  BEGIN
    (*---normalize to -pi..+pi---*)
    (* if it was normalized before, the loops should run at most one
       cycle *)
    WHILE R.Compare(angle, RT.Pi) > 0 DO
      angle := R.Sub(angle, RT.TwoPi);
    END;
    WHILE R.Compare(angle, R.Neg(RT.Pi)) < 0 DO
      angle := R.Add(angle, RT.TwoPi);
    END;
    RETURN T{x.radius, angle};
  END NormalizeAngle;

PROCEDURE Mul (READONLY x, y: T; ): T =
  VAR z: T;
  BEGIN
    z.radius := R.Mul(x.radius, y.radius);
    z.angle := R.Add(x.angle, y.angle);
    RETURN NormalizeAngle(z);
  END Mul;

PROCEDURE Div (READONLY x, y: T; ): T RAISES {Error} =
  VAR z: T;
  BEGIN
    z.radius := R.Div(x.radius, y.radius);
    z.angle := R.Sub(x.angle, y.angle);
    RETURN NormalizeAngle(z);
  END Div;


BEGIN
END PolarBasic.
