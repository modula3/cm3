(* $Id: Main.m3,v 1.1 2001/11/28 03:30:07 mika Exp $ *)

MODULE Main;
IMPORT BigRational AS Rational;
IMPORT Debug;

VAR
  one := Rational.NewSimple(1,1);
  two := Rational.NewSimple(2,1);
  third := Rational.NewSimple(1,3);
  twelfth := Rational.NewSimple(1,12);
  x := Rational.Add(third,twelfth);
  mtwelfth := Rational.Mul(twelfth,Rational.NewSimple(-1,1));
  y := Rational.Add(x,mtwelfth);
  z := one;
BEGIN
  Debug.Out(Rational.Format(two));
  Debug.Out(Rational.Format(one));
  Debug.Out(Rational.Format(x));
  Debug.Out(Rational.Format(y));

  FOR i := 1 TO 99 DO z := Rational.Mul(twelfth,z) END;
  Debug.Out(Rational.Format(z));
END Main.
