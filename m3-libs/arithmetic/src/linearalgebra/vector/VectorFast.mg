GENERIC MODULE VectorFast(R, V, VR);
(*Copyright (c) 1996, m3na project *)
FROM NADefinitions IMPORT Error, Err;

(*VR is needed to reveal the structure of V.T*)

<*UNUSED*>
CONST Module = "VectorFast.";

(*-----------------*)
<*INLINE*>
PROCEDURE AssertEqualSize (READONLY x, y: T) RAISES {Error} =
  BEGIN
    IF NUMBER(x) # NUMBER(y) THEN RAISE Error(Err.bad_size); END;
  END AssertEqualSize;

(*---------------------*)
PROCEDURE IsZero (READONLY x: T): BOOLEAN =
  BEGIN
    FOR i := FIRST(x) TO LAST(x) DO
      IF x[i] # R.Zero THEN RETURN FALSE; END
    END;
    RETURN TRUE;
  END IsZero;

(*---------------------*)
PROCEDURE Equal (READONLY x, y: T): BOOLEAN RAISES {Error} =
  BEGIN
    AssertEqualSize(x, y);
    FOR i := FIRST(x) TO LAST(x) DO
      IF x[i] # y[i] THEN RETURN FALSE; END
    END;
    RETURN TRUE;
  END Equal;

(*-----------------*)
PROCEDURE Add (READONLY x, y: T): V.T RAISES {Error} =
  VAR
    z := NEW(V.T, NUMBER(x));
  BEGIN
    AssertEqualSize(x, y);
    FOR i := FIRST(x) TO LAST(x) DO z[i] := x[i] + y[i]; END;
    RETURN z;
  END Add;

(*-----------------*)
PROCEDURE Sub (READONLY x, y: T): V.T RAISES {Error} =
  VAR 
    z := NEW(V.T, NUMBER(x));
  BEGIN
    AssertEqualSize(x, y);
    FOR i := FIRST(x) TO LAST(x) DO z[i] := x[i] - y[i]; END;
    RETURN z;
  END Sub;

(*---------------------*)
PROCEDURE Neg (READONLY x: T): V.T =
  VAR z := NEW(V.T, NUMBER(x));
  BEGIN
    FOR i := FIRST(x) TO LAST(x) DO z[i] := -x[i]; END;
    RETURN z;
  END Neg;


(*-----------------*)
PROCEDURE Scale (READONLY x: T; y: R.T): V.T =
  VAR z := NEW(V.T, NUMBER(x));
  BEGIN
    FOR i := FIRST(x) TO LAST(x) DO z[i] := x[i] * y; END;
    RETURN z;
  END Scale;


(*-----------------*)
PROCEDURE Inner (READONLY x, y: T): R.T RAISES {Error} =
  VAR 
    sum := R.Zero;
  BEGIN
    AssertEqualSize(x, y);
    FOR i := FIRST(x) TO LAST(x) DO sum := sum + x[i] * y[i]; END;
    RETURN sum;
  END Inner;

(*-----------------*)
(*
PROCEDURE Cross(
                x,y:T):T RAISES {Error}=
BEGIN
  RAISE Error(Err.not_implemented);
END Cross;
*)

PROCEDURE Sum (READONLY x: T): R.T =
  VAR sum := R.Zero;
  BEGIN
    FOR i := FIRST(x) TO LAST(x) DO sum := sum + x[i]; END;
    RETURN sum;
  END Sum;

PROCEDURE Max (READONLY x: T): R.T =
  VAR max := R.NegInf;
  BEGIN
    FOR i := FIRST(x) TO LAST(x) DO max := MAX(max, x[i]); END;
    RETURN max;
  END Max;

PROCEDURE Min (READONLY x: T): R.T =
  VAR min := R.PosInf;
  BEGIN
    FOR i := FIRST(x) TO LAST(x) DO min := MIN(min, x[i]); END;
    RETURN min;
  END Min;


(*-----------------*)
PROCEDURE ArithSeq (num: CARDINAL; from: R.T; by: R.T): V.T =
  VAR x := NEW(V.T, num);
  BEGIN
    IF num > 0 THEN
      x[0] := from;
      FOR j := 1 TO num - 1 DO from := from + by; x[j] := from; END;
    END;
    RETURN x;
  END ArithSeq;

(*-----------------*)
PROCEDURE GeomSeq (num: CARDINAL; from: R.T; by: R.T): V.T =
  VAR x := NEW(V.T, num);
  BEGIN
    IF num > 0 THEN
      x[0] := from;
      FOR j := 1 TO num - 1 DO from := from * by; x[j] := from; END;
    END;
    RETURN x;
  END GeomSeq;


(*-----------------*)
BEGIN
END VectorFast.
