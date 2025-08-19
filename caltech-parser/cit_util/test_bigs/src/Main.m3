(* $Id: Main.m3,v 1.1 2001/11/28 03:30:06 mika Exp $ *)

MODULE Main;
IMPORT BigInt;
IMPORT Debug;

VAR
  one := BigInt.New(1);
  two := BigInt.New(2);
  onethousandone := BigInt.New(1001);
  twoSixteen := BigInt.New(65536);
  twoThirtyTwo := BigInt.Mul(twoSixteen,twoSixteen);
  twoSixtyFour := BigInt.Mul(twoThirtyTwo,twoSixteen);
  million := BigInt.New(1000*1000);
  trillion := BigInt.Mul(million,million);
  aBigNumber := one;
BEGIN
(*  Debug.Out(BigInt.Format(one));
  Debug.Out(BigInt.Format(two));
  Debug.Out(BigInt.Format(onethousandone));
*)
  FOR i := 0 TO 100 DO 
    aBigNumber := BigInt.Mul(aBigNumber,trillion)
  END;

  Debug.Out(BigInt.Format(twoSixteen));
  Debug.Out(BigInt.Format(twoThirtyTwo));
  Debug.Out(BigInt.Format(twoSixtyFour));
  Debug.Out(BigInt.Format(BigInt.Div(BigInt.Mul(BigInt.New(22),aBigNumber),BigInt.New(7))))
END Main.
