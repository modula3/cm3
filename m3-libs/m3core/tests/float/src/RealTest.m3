(* Copyright (C) 1992, Xerox                                                 *)
(* All rights reserved.                                                      *)

(* Last modified on Mon Oct 12 14:24:26 PDT 1992 by muller                   *)
(*      modified on Fri Feb 28 21:25:03 PST 1992 by stolfi                   *)
(*      modified on Wed Sep 25 00:33:01 1991 by goldberg@xerox.parc.com      *)

(* test for Float.i3, FloatMode.i3, Real.i3 interfaces *)


MODULE RealTest;

IMPORT  Thread, Wr, RealFloat, FloatMode, Fmt, Text, Real;
FROM FloatMode IMPORT RoundingMode, Flag, Behavior;
FROM RealFloat IMPORT IEEEClass;
FROM Stdio IMPORT stderr; 

<*FATAL ANY*>

PROCEDURE ICheck(name: TEXT; n, m: INTEGER) =
  BEGIN
    IF n = m THEN
      Wr.PutText (stderr, Fmt.F ("   %s test OK\n", name));
    ELSE
      Wr.PutText (stderr, Fmt.F ("** %s test not OK: %s should be %s\n",
                                 name, Fmt.Int(n), Fmt.Int(m)));
    END;      
  END ICheck;

PROCEDURE BCheck(name: TEXT; n, m: BOOLEAN) =
  BEGIN
    IF n = m THEN
      Wr.PutText (stderr, Fmt.F ("   %s test OK\n", name));
    ELSE
      Wr.PutText (stderr, Fmt.F ("** %s test not OK: %s should be %s\n", 
                                 name, Fmt.Bool(n), Fmt.Bool(m)));
    END;      
  END BCheck;

PROCEDURE Check(name: TEXT; n, m: REAL) =
  BEGIN
    IF n = m THEN
      Wr.PutText (stderr, "   " & name & " test OK\n");
    ELSE
      Wr.PutText (stderr, "** " & name & " test not OK: "
                            & Fmt.Real (n) & " should be "
                            & Fmt.Real (m) & "\n");
    END;      
  END Check;

PROCEDURE TestFloat()=
  VAR
    arg, x, y: REAL;
    minusZero, NaN: REAL;
    n: INTEGER;
    one := 1.0;
    zero := 0.0;
    ten := 10.0;
  BEGIN
    minusZero := -zero;
    BCheck("1.0/0.0 # 1.0/(-0.0)", 1.0/zero = 1.0/minusZero, FALSE);
    BCheck("1.0/0.0 = 1.0/(- (-0.0))", 1.0/zero = 1.0/(-minusZero), TRUE);

    NaN := minusZero/minusZero;

    (* Logb, Scalb *)
    arg := Real.MinPosNormal;
    n := RealFloat.ILogb (arg);
    ICheck("ILogb (MinPosNormal)", n, -126);

    arg := arg/8.0;
    n := RealFloat.ILogb (arg);
    ICheck("Ilogb (MinPosNormal / 8.0)", n, -129);

    x := RealFloat.Logb(arg);
    Check("Logb  (MinPosNormal / 8.0)", x, -126.0);

    arg := RealFloat.Scalb(1.1, 100);
    n := RealFloat.ILogb(arg);
    ICheck("Ilogb (Scalb (1.1, 100))", n, 100);

    x := RealFloat.Logb(arg);
    Check("Logb (Scalb (1.1, 100))", x, 100.0);

    (* NextAfter *)
    x := RealFloat.NextAfter(0.0, 10.0);
    Check("NextAfter (0.0, 10.0)", x, Real.MinPos);

    x := RealFloat.NextAfter(-1.0, -2.0);
    Check("NextAfter (-1.0, -2.0)", -1.0 - x, RealFloat.Scalb(1.0, -23));

    (* CopySign *)
    x := RealFloat.CopySign(1.0, minusZero);
    Check("CopySign (1.0, -0.0)", x, -1.0);

    x := RealFloat.CopySign(-1.0, 0.0);
    Check("CopySign (-1.0, 0.0)", x, 1.0);

    (* Class,  class functions *)
    x := one/ten;
    BCheck("Class (one/ten)", RealFloat.Class(x) = IEEEClass.Normal, TRUE);

    x := Real.MinPos/ten;
    BCheck("Class (MinPos/ten)", RealFloat.Class(x) = IEEEClass.Zero, TRUE);

    x := Real.MinPosNormal/ten;
    BCheck("Class (MinPosNormal/ten)",
           RealFloat.Class(x) = IEEEClass.Denormal, TRUE);
    BCheck("Finite (MinPosNormal/ten)", RealFloat.Finite(x), TRUE);

    x := Real.MaxFinite*ten;
    BCheck("Class (MaxFinite*ten)", 
	   RealFloat.Class(x) = IEEEClass.Infinity, TRUE);
    BCheck("Finite (MaxFinite*ten)", RealFloat.Finite(x), FALSE);
    BCheck("IsNaN (MaxFinite*ten)", RealFloat.IsNaN(x), FALSE);

    x := zero/zero;
    BCheck("Class (zero/zero)", RealFloat.Class(x) = IEEEClass.QuietNaN, TRUE);
    BCheck("Finite (zero/zero)", RealFloat.Finite(x), FALSE);
    BCheck("IsNaN (zero/zero)", RealFloat.IsNaN(x), TRUE);

    ICheck("Sign (zero)", RealFloat.Sign(zero), 0);
    ICheck("Sign (minusZero)", RealFloat.Sign(minusZero), 1);

    (* Differs, Unordered *)
    BCheck("Unordered (zero, NaN)", RealFloat.Unordered(zero, NaN), TRUE);
    BCheck("Unordered (zero, zero)", RealFloat.Unordered(zero, zero), FALSE);
    BCheck("Unordered (NaN, NaN)", RealFloat.Unordered(NaN, NaN), TRUE);

    BCheck("Differs (zero, NaN)", RealFloat.Differs(zero, NaN), FALSE);
    BCheck("Differs (zero, zero)", RealFloat.Differs(zero, zero), FALSE);
    BCheck("Differs (zero, one)", RealFloat.Differs(zero, one), TRUE);
    BCheck("Differs (NaN, NaN)", RealFloat.Differs(NaN, NaN), FALSE);

    (* Sqrt *)
    y := RealFloat.Scalb(1234.0, 14);
    arg := y*y; (* exact *)
    x := RealFloat.Sqrt(arg);
    Check("Sqrt (y*y)", x, y);
    Check("Sqrt (minusZero)", minusZero, minusZero);

  END TestFloat;

PROCEDURE TestReal() =
  VAR x, y: REAL;
  BEGIN
    x := Real.MaxFinite;
    y := x + x/RealFloat.Scalb(1.0, 23);
    BCheck("MaxFinite  1st", RealFloat.Class(x) = IEEEClass.Normal, TRUE);
    BCheck("MaxFinite  2nd", RealFloat.Class(y) = IEEEClass.Infinity, TRUE);
    Check("MaxFinite  3rd", y - x, y);

    x := Real.MinPosNormal;
    y := x/RealFloat.Scalb(1.0, 23);
    Check("MinPosNormal  1st", y, Real.MinPos);
    BCheck("MinPosNormal  2nd", y = 0.0, FALSE);
    y := y/2.0;
    BCheck("MinPosNormal  3rd", y = 0.0, TRUE);

    y := x - x/RealFloat.Scalb(1.0, 23);
    BCheck("MinPosNormal  4th", RealFloat.Class(x) = IEEEClass.Normal, TRUE);
    BCheck("MinPosNormal  5th", RealFloat.Class(y) = IEEEClass.Denormal, TRUE);
  END TestReal;

PROCEDURE TestFloatMode() =
  PROCEDURE SetRndMode(md: RoundingMode; x, y: REAL): REAL =
    VAR
      md1: RoundingMode;
    BEGIN
      TRY
	FloatMode.SetRounding (md);
	md1 := FloatMode.GetRounding ();
	BCheck("GetRound  1st", md = md1, TRUE);
	RETURN(x/y);
      EXCEPT
	FloatMode.Failure => RETURN(0.0);
      END;
    END SetRndMode;
  VAR
    xPlus, xMinus, xZero, xNear: REAL;
    eleven := 11.0;
    twentyOne := 21.0;
    ten := 10.0;
    one := 1.0;
    zero := 0.0;
    two := 2.0;
  BEGIN
    (* rounding modes *)
    xPlus := SetRndMode(RoundingMode.PlusInfinity, eleven, ten);
    xMinus := SetRndMode(RoundingMode.MinusInfinity, eleven, ten);
    xZero := SetRndMode(RoundingMode.Zero, eleven, ten);
    xNear := SetRndMode(RoundingMode.Nearest, eleven, ten);

    BCheck("SetRound  1st", xMinus < xPlus, TRUE);
    BCheck("SetRound  2nd", xMinus = xZero, TRUE);
    BCheck("SetRound  3rd", xPlus = xNear, TRUE);

    xPlus := SetRndMode(RoundingMode.PlusInfinity, -twentyOne, ten);
    xMinus := SetRndMode(RoundingMode.MinusInfinity, -twentyOne, ten);
    xZero := SetRndMode(RoundingMode.Zero, -twentyOne, ten);
    xNear := SetRndMode(RoundingMode.Nearest, -twentyOne, ten);

    BCheck("SetRound  4th", xMinus < xPlus, TRUE);
    BCheck("SetRound  5th", xPlus = xZero, TRUE);
    BCheck("SetRound  6th", xZero = xNear, TRUE);

    xNear := SetRndMode(RoundingMode.Vax, -twentyOne, ten);
    Check("SetRound  7th", xNear, 0.0);

    PROCEDURE GetFlags(a, b: REAL; op: TEXT): SET OF Flag = 
      VAR c: REAL;
	  s: SET OF Flag;
      BEGIN
	IF Text.Equal(op, "/") THEN
	  c := a/b;
	ELSE
	  c := a*b;
	END;
	s := FloatMode.SetFlags(FloatMode.NoFlags);
	RETURN(s);
      END GetFlags;
    VAR
      fl: SET OF Flag;
    TYPE Set = SET OF Flag;
    BEGIN
      EVAL(FloatMode.SetFlags(FloatMode.NoFlags));
      fl := FloatMode.GetFlags();
      BCheck("SetFlags  1st", fl = Set{Flag.Denormalized}, TRUE);

      fl := GetFlags(one, zero, "/");
      BCheck("SetFlags  2nd", fl = Set{Flag.DivByZero,Flag.Denormalized}, TRUE);

      fl := GetFlags(one, ten, "/");
      BCheck("SetFlags  3rd", fl = Set{Flag.Inexact,Flag.Denormalized}, TRUE);

      fl := GetFlags(Real.MinPosNormal, two, "/");
      BCheck("SetFlags  4th", fl = FloatMode.NoFlags, TRUE);

      fl := GetFlags(Real.MinPosNormal, ten, "/");
      BCheck("SetFlags  5th", fl = Set{Flag.Inexact, Flag.Underflow}, TRUE);

      fl := GetFlags(Real.MinPos, two, "/");
      BCheck("SetFlags  6th", fl = Set{Flag.Inexact, Flag.Underflow}, TRUE);

      fl := GetFlags(Real.MaxFinite, two, "*");
      BCheck("SetFlags  7th", fl = Set{Flag.Overflow, Flag.Inexact}, TRUE);

      fl := GetFlags(zero, zero, "/");
      BCheck("SetFlags  8th", fl = Set{Flag.Invalid}, TRUE);

      EVAL(FloatMode.SetFlags(FloatMode.NoFlags));
      EVAL FloatMode.SetFlags(Set{Flag.Invalid, Flag.DivByZero});
      fl := FloatMode.GetFlags();
      BCheck("SetFlags  9th", fl = Set{Flag.Invalid, Flag.DivByZero}, TRUE);
    END;

    PROCEDURE SetBehave(flg: Flag; msg: TEXT; u, v: REAL; op: TEXT) =
      VAR
	b: Behavior;
	x: REAL;
      BEGIN
	b := FloatMode.GetBehavior(flg);
	BCheck("Old behavior, " & msg, b = Behavior.SetFlag, TRUE);

	FloatMode.SetBehavior(flg, Behavior.Trap);
	b := FloatMode.GetBehavior(flg);
	BCheck("New behavior, " & msg, b = Behavior.Trap, TRUE);
	TRY
	  x := zero/zero;
	  x := -1.0;
	  IF Text.Equal(op, "/") THEN
	    x := u/v;
	  ELSE
	    x := u*v;
	  END;
	  Wr.PutText (stderr, "** no trap\n");
	EXCEPT
	  FloatMode.Trap(fl) =>
	    BCheck("right flag ?", fl = flg, TRUE);
	    Check("right value ?", x, -1.0);
	END;
	FloatMode.SetBehavior(flg, Behavior.SetFlag);
      END SetBehave;

    BEGIN
      SetBehave(Flag.DivByZero, "DivByZero", one, zero, "/");
      SetBehave(Flag.Overflow, "Overflow", Real.MaxFinite, 10.0, "*");
      SetBehave(Flag.Underflow, "Underflow", Real.MinPosNormal, 2.0, "/");
    END;

    (* test IntDivZero *)
    VAR
      b: Behavior;
      x: REAL;
      r, s: INTEGER;
    BEGIN
      b := FloatMode.GetBehavior(Flag.IntDivByZero);
      BCheck("IntDivByZero, default behaviour", b = Behavior.Trap, TRUE);

      TRY
	BEGIN
	  x := one/zero;
	  x := -1.0;
	  s := 0;
	  r := 10;
	  r := r DIV s;
	  Wr.PutText (stderr, "** no trap\n");
	END;
      EXCEPT
	FloatMode.Trap(fl) =>
	  BCheck("right flag ?", fl = Flag.IntDivByZero, TRUE);
	  Check("right value ?", x, -1.0);
      END;
    END;
  END TestFloatMode;

PROCEDURE Test() =
  BEGIN
    TestFloat();  (* Float.i3 *)
    TestReal();  (* Real.i3 *)
    TestFloatMode();  (* FloatMode.i3 *)
  END Test;

PROCEDURE X(cl: Thread.Closure <* NOWARN *>): REFANY RAISES {} =
  VAR
    fl: SET OF Flag;
  BEGIN
    EVAL FloatMode.SetFlags(SET OF Flag{Flag.Overflow});
    fl := FloatMode.GetFlags();
    BCheck("Fork 2nd", fl = SET OF Flag{Flag.Overflow}, TRUE);
    RETURN(NIL);
  END X;

PROCEDURE TestThreads() =
  VAR
    fl: SET OF Flag;
    th: Thread.T;
  BEGIN
    EVAL FloatMode.SetFlags(SET OF Flag{Flag.Inexact});
    fl := FloatMode.GetFlags();
    BCheck("Fork  1st", fl = SET OF Flag{Flag.Inexact}, TRUE);
    th := Thread.Fork(NEW(Thread.Closure, apply := X));
    EVAL Thread.Join(th);
    fl := FloatMode.GetFlags();
    BCheck("Fork  3rd", fl = SET OF Flag{Flag.Inexact}, TRUE);
  END TestThreads;

BEGIN
 (* start code *)
END RealTest.
