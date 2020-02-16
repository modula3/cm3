(* $Id$ *)

MODULE BoolInteger;
IMPORT Bool,Word;
IMPORT Fmt, BoolFormatter;
IMPORT TextWr, Wr, Thread;

REVEAL 
  Private = Public BRANDED Brand & "Private" OBJECT
  OVERRIDES
    getMinValue := GetMinValue;
    getMaxValue := GetMaxValue;
    isEqual := IsEqual;
    format := Format;
  END;

PROCEDURE Format(a : Private; bf : BoolFormatter.T) : TEXT =
  <* FATAL Thread.Alerted, Wr.Failure *>
  VAR
    wr := NEW(TextWr.T).init();
  BEGIN
    FOR i := 0 TO a.repbits() DO
      Wr.PutText(wr,"bit "); 
      Wr.PutText(wr,Fmt.Int(i)); 
      Wr.PutText(wr," : ");
      Wr.PutText(wr,bf.fmt(a.extract(i)));
      Wr.PutChar(wr,'\n')
    END;
    RETURN TextWr.ToText(wr)
  END Format;

(* for generics *)
PROCEDURE Equal(a, b : T) : BOOLEAN = BEGIN RETURN a = b END Equal;

(* inefficient but simple first impl. *)
PROCEDURE IsEqual(a : Public; n : INTEGER) : Bool.T = 
  BEGIN RETURN Equals(a, Constant(n)) END IsEqual;

PROCEDURE NotEquals(a, b : T) : Bool.T = 
  BEGIN RETURN Bool.Not(Equals(a,b)) END NotEquals;

PROCEDURE GetMaxValue(a : Public) : INTEGER =
  VAR
    top,bot : INTEGER;
  BEGIN
    top := Word.Shift(1,a.repbits());
    bot := -top;
    WHILE top # bot DO
      VAR
        mid := (top + bot) DIV 2;
      BEGIN
        IF GreaterThan(a,Constant(mid)) = Bool.False() THEN
          top := mid
        ELSE
          bot := mid + 1
        END
      END
    END;
    <* ASSERT top = bot AND 
              GreaterThan(a,Constant(top)) = Bool.False() AND
              Equals(a,Constant(top)) # Bool.False()
     *>
    RETURN top
  END GetMaxValue;

PROCEDURE GetMinValue(a : Public) : INTEGER =
  BEGIN RETURN -Neg(a).getMaxValue() END GetMinValue;

PROCEDURE Sub(a, b : T) : T =
  VAR
    bminus := Neg(b);
  BEGIN
    RETURN Add(a,bminus)
  END Sub;

PROCEDURE Abs(a : T) : T =
  VAR
    pos := GreaterThanOrEqual(a,Zero);
  BEGIN
    RETURN Choose(pos,a,Neg(a))
  END Abs;

PROCEDURE GreaterThan(a, b : T) : Bool.T =
  BEGIN RETURN LessThanZero(Sub(b,a)) END GreaterThan;

PROCEDURE LessThan(a, b : T) : Bool.T =
  BEGIN RETURN LessThanZero(Sub(a,b)) END LessThan;

PROCEDURE GreaterThanOrEqual(a, b : T) : Bool.T =
  BEGIN 
    RETURN Bool.Or(LessThanZero(Sub(b,a)),Equals(a,b)) 
  END GreaterThanOrEqual;

PROCEDURE LessThanOrEqual(a, b : T) : Bool.T =
  BEGIN 
    RETURN Bool.Or(LessThanZero(Sub(a,b)),Equals(a,b)) 
  END LessThanOrEqual;


PROCEDURE BitwiseAnd(a, b : T) : T =
  BEGIN RETURN BitwiseOp(a,b,Bool.And) END BitwiseAnd;

PROCEDURE BitwiseOr(a, b : T) : T =
  BEGIN RETURN BitwiseOp(a,b,Bool.Or) END BitwiseOr;

PROCEDURE BitwiseXor(a, b : T) : T =
  BEGIN RETURN BitwiseOp(a,b,Bool.Xor) END BitwiseXor;

PROCEDURE BitwiseNot(a : T) : T =
  BEGIN RETURN BitwiseOp(a,MinusOne,Bool.Xor) END BitwiseNot;


PROCEDURE UnsignedShift(a : T; sa : INTEGER) : T =
  BEGIN
    IF sa < 0 THEN 
      RETURN UnsignedShiftRight(a, -sa) 
    ELSE 
      RETURN ShiftLeft(a,sa)
    END
  END UnsignedShift;

PROCEDURE SignedShift(a : T; sa : INTEGER) : T =
  BEGIN
    IF sa < 0 THEN 
      RETURN SignedShiftRight(a, -sa) 
    ELSE 
      RETURN ShiftLeft(a,sa)
    END
  END SignedShift;

PROCEDURE UnsignedShiftV(a : T; sa : T) : T =
  VAR
    res : T;
  BEGIN
    res := Zero; (* should never get this *)
    FOR i := sa.getMinValue() TO sa.getMaxValue() DO
      res := Choose(sa.isEqual(i),UnsignedShift(a,i),res)
    END;
    RETURN res
  END UnsignedShiftV;

PROCEDURE SignedShiftV(a : T; sa : T) : T =
  VAR
    res : T;
  BEGIN
    res := Zero; (* should never get this *)
    FOR i := sa.getMinValue() TO sa.getMaxValue() DO
      res := Choose(sa.isEqual(i),SignedShift(a,i),res)
    END;
    RETURN res
  END SignedShiftV;

PROCEDURE Sign(a : T) : Bool.T =
  BEGIN RETURN a.extract(a.repbits() - 1) END Sign;

PROCEDURE LessThanZero(a : T) : Bool.T =
  BEGIN RETURN Sign(a) END LessThanZero;

PROCEDURE Mul(a, b : T) : T =
  VAR
    absa := Abs(a);
    absb := Abs(b);

    sgna := Sign(a);
    sgnb := Sign(b);

    sgnr := Bool.Xor(sgna,sgnb);
    r := Zero;
  BEGIN
    FOR i := 0 TO absa.repbits() - 1 DO
      VAR
        term := Choose(absa.extract(i),ShiftLeft(absb,i),Zero);
      BEGIN
        r := Add(r,term)
      END
    END;
    RETURN Choose(sgnr,Neg(r),r)
  END Mul;

BEGIN 
  (* can't do anything here because we need to set things up in 
     BoolIntegerImpl first *)
END BoolInteger.
