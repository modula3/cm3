(* Copyright (C) 1992, Xerox                                                 *)
(* All rights reserved.                                                      *)

(* Last modified on Mon Oct 12 14:22:44 PDT 1992 by muller                   *)
(*      modified on Wed Sep 25 00:33:01 1991 by goldberg@xerox.parc.com      *)

MODULE LongTest;

IMPORT LongFloat, FloatMode, Fmt, Text, LongReal, Wr;
FROM FloatMode IMPORT RoundingMode, Flag, Behavior;
FROM LongFloat IMPORT IEEEClass;
FROM Stdio IMPORT stderr;

<* FATAL ANY *>

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

PROCEDURE Check(name: TEXT; n, m: LONGREAL) =
  BEGIN
    IF n = m THEN
      Wr.PutText (stderr, Fmt.F ("   %s test OK\n", name));
    ELSE
      Wr.PutText (stderr, Fmt.F ("** %s test not OK: %s should be %s\n",
                                 name, Fmt.LongReal(n), Fmt.LongReal(m)));
    END;      
  END Check;

PROCEDURE TestFloat()=
  VAR
    arg, x, y: LONGREAL;
    minusZero, NaN: LONGREAL;
    n: INTEGER;
    one := 1.0d0;
    zero := 0.0d0;
    ten := 10.0d0;
  BEGIN
    minusZero := -zero;
    BCheck("1.0/0.0 # 1.0/(-0.0)", 1.0d0/zero = 1.0d0/minusZero, FALSE);
    BCheck("1.0/0.0 = 1.0/(- (-0.0))", 1.0d0/zero = 1.0d0/(-minusZero), TRUE);
    NaN := minusZero/minusZero;

    (* Logb, Scalb *)
    arg := LongReal.MinPosNormal;
    n := LongFloat.ILogb(arg);
    ICheck("ILogb (MinPosNormal)", n, -1022);

    arg := arg/8.0d0;
    n := LongFloat.ILogb(arg);
    ICheck("Ilogb (MinPosNormal / 8.0)", n, -1025);

    x := LongFloat.Logb(arg);
    Check("Logb (MinPosNormal / 8.0)", x, -1022.0d0);

    arg := LongFloat.Scalb(1.1d0, 100);
    n := LongFloat.ILogb(arg);
    ICheck("Ilogb  (Scalb (1.1, 100))", n, 100);

    x := LongFloat.Logb(arg);
    Check("Logb  (Scalb (1.1, 100))", x, 100.0d0);

    (* NextAfter *)
    x := LongFloat.NextAfter(0.0d0, 10.0d0);
    Check("NextAfter (0.0, 10.0)", x, LongReal.MinPos);

    x := LongFloat.NextAfter(-1.0d0, -2.0d0);
    Check("NextAfter (-1.0, -2.0)", -1.0d0 - x, LongFloat.Scalb(1.0d0, -52));

    (* CopySign *)
    x := LongFloat.CopySign(1.0d0, minusZero);
    Check("CopySign (1.0, -0.0)", x, -1.0d0);

    x := LongFloat.CopySign(-1.0d0, 0.0d0);
    Check("CopySign (-1.0, 0.0)", x, 1.0d0);

    (* Class,  class functions *)
    x := one/ten;
    BCheck("Class (one/ten)", LongFloat.Class(x) = IEEEClass.Normal, TRUE);

    x := LongReal.MinPos/ten;
    BCheck("Class (MinPos/ten)", LongFloat.Class(x) = IEEEClass.Zero, TRUE);

    x := LongReal.MinPosNormal/ten;
    BCheck("Class (MinPosNormal/ten)", 
           LongFloat.Class(x) = IEEEClass.Denormal, TRUE);
    BCheck("Finite (MinPosNormal/ten)", LongFloat.Finite(x), TRUE);

    x := LongReal.MaxFinite*ten;
    BCheck("Class  MaxFinite*ten",
            LongFloat.Class(x) = IEEEClass.Infinity, TRUE);
    BCheck("Finite MaxFinite*ten", LongFloat.Finite(x), FALSE);
    BCheck("IsNaN  MaxFinite*ten", LongFloat.IsNaN(x), FALSE);

    x := zero/zero;
    BCheck("Class zero/zero", LongFloat.Class(x) = IEEEClass.QuietNaN, TRUE);
    BCheck("Finite zero/zero", LongFloat.Finite(x), FALSE);
    BCheck("IsNaN zero/zero", LongFloat.IsNaN(x), TRUE);

    ICheck("Sign (zero)", LongFloat.Sign(zero), 0);
    ICheck("Sign (minusZero)", LongFloat.Sign(minusZero), 1);

    (* Differs, Unordered *)
    BCheck("Unordered (zero, NaN)", LongFloat.Unordered(zero, NaN), TRUE);
    BCheck("Unordered (zero, zero)", LongFloat.Unordered(zero, zero), FALSE);
    BCheck("Unordered (NaN, NaN)", LongFloat.Unordered(NaN, NaN), TRUE);

    BCheck("Differs  (zero, NaN)", LongFloat.Differs(zero, NaN), FALSE);
    BCheck("Differs  (zero, zero)", LongFloat.Differs(zero, zero), FALSE);
    BCheck("Differs  (zero, one)", LongFloat.Differs(zero, one), TRUE);
    BCheck("Differs  (NaN, NaN)", LongFloat.Differs(NaN, NaN), FALSE);

    (* Sqrt *)
    y := LongFloat.Scalb(1234.0d0, 14);
    arg := y*y; (* exact *)
    x := LongFloat.Sqrt(arg);
    Check("Sqrt  (y*y)", x, y);
    Check("Sqrt  (minusZero)", minusZero, minusZero);

  END TestFloat;

PROCEDURE TestReal() =
  VAR x, y: LONGREAL;
  BEGIN
    x := LongReal.MaxFinite;
    y := x + x/LongFloat.Scalb(1.0d0, 52);
    BCheck("MaxFinite  1st", LongFloat.Class(x) = IEEEClass.Normal, TRUE);
    BCheck("MaxFinite  2nd", LongFloat.Class(y) = IEEEClass.Infinity, TRUE);
    Check("MaxFinite  3rd", y - x, y);

    x := LongReal.MinPosNormal;
    y := x/LongFloat.Scalb(1.0d0, 52);
    Check("MinPosNormal  1st", y, LongReal.MinPos);
    BCheck("MinPosNormal  2nd", y = 0.0d0, FALSE);
    y := y/2.0d0;
    BCheck("MinPosNormal  3rd", y = 0.0d0, TRUE);

    y := x - x/LongFloat.Scalb(1.0d0, 52);
    BCheck("MinPosNormal  4th", LongFloat.Class(x) = IEEEClass.Normal, TRUE);
    BCheck("MinPosNormal  5th", LongFloat.Class(y) = IEEEClass.Denormal, TRUE);
  END TestReal;

PROCEDURE TestFloatMode() =
  PROCEDURE SetRndMode(md: RoundingMode; x, y: LONGREAL): LONGREAL =
    VAR
      md1: RoundingMode;
    BEGIN
      TRY
	FloatMode.SetRounding (md);
	md1 := FloatMode.GetRounding ();
	BCheck("GetRound  1st", md = md1, TRUE);
	RETURN(x/y);
      EXCEPT
	FloatMode.Failure => RETURN(0.0d0);
      END;
    END SetRndMode;
  VAR
    xPlus, xMinus, xZero, xNear: LONGREAL;
    eleven := 11.0d0;
    twentyOne := 21.0d0;
    ten := 10.0d0;
    one := 1.0d0;
    zero := 0.0d0;
    two := 2.0d0;
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
    BCheck("SetRound  6th", xMinus = xNear, TRUE);

    xNear := SetRndMode(RoundingMode.Vax, -twentyOne, ten);
    Check("SetRound  7th", xNear, 0.0d0);

    PROCEDURE GetFlags(a, b: LONGREAL; op: TEXT): SET OF Flag = 
      VAR c: LONGREAL;
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
      BCheck("SetFlags  1st", fl = FloatMode.NoFlags, TRUE);

      fl := GetFlags(one, zero, "/");
      BCheck("SetFlags  2nd", fl = Set{Flag.DivByZero}, TRUE);

      fl := GetFlags(one, ten, "/");
      BCheck("SetFlags  3rd", fl = Set{Flag.Inexact}, TRUE);

      fl := GetFlags(LongReal.MinPosNormal, two, "/");
      BCheck("SetFlags  4th", fl = FloatMode.NoFlags, TRUE);

      fl := GetFlags(LongReal.MinPosNormal, ten, "/");
      BCheck("SetFlags  5th", fl = Set{Flag.Inexact, Flag.Underflow}, TRUE);

      fl := GetFlags(LongReal.MinPos, two, "/");
      BCheck("SetFlags  6th", fl = Set{Flag.Inexact, Flag.Underflow}, TRUE);

      fl := GetFlags(LongReal.MaxFinite, two, "*");
      BCheck("SetFlags  7th", fl = Set{Flag.Overflow, Flag.Inexact}, TRUE);

      fl := GetFlags(zero, zero, "/");
      BCheck("SetFlags  8th", fl = Set{Flag.Invalid}, TRUE);

      EVAL(FloatMode.SetFlags(FloatMode.NoFlags));
      EVAL FloatMode.SetFlags(Set{Flag.Invalid, Flag.DivByZero});
      fl := FloatMode.GetFlags();
      BCheck("SetFlags  9th", fl = Set{Flag.Invalid, Flag.DivByZero}, TRUE);
    END;

    PROCEDURE SetBehave(flg: Flag; msg: TEXT; u, v: LONGREAL; op: TEXT) =
      VAR
	b: Behavior;
	x: LONGREAL;
      BEGIN
	b := FloatMode.GetBehavior(flg);
	BCheck("Old behavior, " & msg, b = Behavior.SetFlag, TRUE);

	FloatMode.SetBehavior(flg, Behavior.Trap);
	b := FloatMode.GetBehavior(flg);
	BCheck("New behavior, " & msg, b = Behavior.Trap, TRUE);
	TRY
	  x := zero/zero;
	  x := -1.0d0;
	  IF Text.Equal(op, "/") THEN
	    x := u/v;
	  ELSE
	    x := u*v;
	  END;
	  Wr.PutText (stderr, Fmt.F ("** no trap\n"));
	EXCEPT
	  FloatMode.Trap(fl) =>
	    BCheck("right flag ?", fl = flg, TRUE);
	    Check("right value ?", x, -1.0d0);
	END;
	FloatMode.SetBehavior(flg, Behavior.SetFlag);
      END SetBehave;

    BEGIN
      SetBehave(Flag.DivByZero, "DivByZero", one, zero, "/");
      SetBehave(Flag.Overflow, "Overflow", LongReal.MaxFinite, 10.0d0, "*");
      SetBehave(Flag.Underflow, "Underflow", LongReal.MinPosNormal, 2.0d0, "/");
    END;

    (* test IntDivZero *)
    VAR
      b: Behavior;
      x: LONGREAL;
      r, s: INTEGER;
    BEGIN
(*      b := FloatMode.GetBehavior(Flag.IntDivByZero);
      BCheck("IntDivByZero, default behaviour", b = Behavior.Trap, TRUE); *)

      TRY
	BEGIN
	  x := one/zero;
	  x := -1.0d0;
	  s := 0;
	  r := 10;
	  r := r DIV s;
	  Wr.PutText (stderr, Fmt.F ("** no trap\n"));
	END;
      EXCEPT
	FloatMode.Trap(fl) =>
	  (*BCheck("right flag ?", fl = Flag.IntDivByZero, TRUE);*)
	  Check("right value", x, -1.0d0);
      END;
    END;
  END TestFloatMode;

PROCEDURE Test() =
  BEGIN
    TestFloat();  (* Float.i3 *)
    TestReal();  (* Real.i3 *)
    TestFloatMode();  (* FloatMode.i3 *)
  END Test;

BEGIN
 (* start code *)
END LongTest.
