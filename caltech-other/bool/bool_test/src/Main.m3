(* $Id$ *)

MODULE Main;
IMPORT BoolInteger,Bool;
IMPORT Debug,Fmt;

VAR
  a := NEW(BoolInteger.T).init(2);
  b,c := NEW(BoolInteger.T).init(200);
BEGIN
  Debug.Out("IsConstant(Zero) = " & Fmt.Bool(BoolInteger.Zero.isConstant()));
  Debug.Out("a.getMinValue() = " & Fmt.Int(a.getMinValue()));
  Debug.Out("a.getMaxValue() = " & Fmt.Int(a.getMaxValue()));
  Debug.Out("(a+a).getMaxValue() = " & 
    Fmt.Int(BoolInteger.Add(a,a).getMaxValue()));
  Debug.Out("( (b = 2 AND c = 5) => (b+c = 7) ) = Bool.True(): " &
    Fmt.Bool(Bool.Implies( Bool.And(b.isEqual(2),c.isEqual(5)), 
                  BoolInteger.Add(b,c).isEqual(7) ) = Bool.True()));
  Debug.Out("(b+c).getMaxValue() = " & Fmt.Int(BoolInteger.Add(b,c).getMaxValue()));
  Debug.Out("(b-c).getMinValue() = " & Fmt.Int(BoolInteger.Sub(b,c).getMinValue()));
  Debug.Out("((b = 1 AND c = 5) AND ( (b = 1 AND c = 5) => (b+c = 7) )) = Bool.False(): " &
    Fmt.Bool(Bool.And(Bool.And(b.isEqual(1),c.isEqual(5)),Bool.Implies( Bool.And(b.isEqual(1),c.isEqual(5)), 
                  BoolInteger.Add(b,c).isEqual(7) )) = Bool.False()));

  Debug.Out("(b = 200) = Bool.False() : " &
    Fmt.Bool(b.isEqual(200) = Bool.False()));
  Debug.Out("(b = 200) = Bool.True() : " &
    Fmt.Bool(b.isEqual(200) = Bool.True()));
  Debug.Out("(b = 201) = Bool.False() : " &
    Fmt.Bool(b.isEqual(201) = Bool.False()));

  Debug.Out(
      Fmt.Bool((BoolInteger.Mul(BoolInteger.Constant(3),BoolInteger.Constant(8))= BoolInteger.Constant(24)) )
  );

END Main.
