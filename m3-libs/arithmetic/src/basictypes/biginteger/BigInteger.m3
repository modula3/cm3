MODULE BigInteger;
(* Arithmetic for Modula-3, see doc for details

   Abstract: Integers of arbitrary size

   Daniel Beer *)

IMPORT Arithmetic AS Arith;
IMPORT BigIntegerRep AS Rep;

<* UNUSED *>
CONST
  Module = "BigInteger.";
(*==========================*)

<* UNUSED *>
PROCEDURE FastCopy (READONLY x: T): T =
  VAR y := T{NEW(Value, NUMBER(x.data^)), x.size, x.sign};

  BEGIN
    y.data^ := x.data^;
    RETURN y;
  END FastCopy;

PROCEDURE Copy (READONLY x: T): T =
  VAR y := T{NEW(Value, x.size), x.size, x.sign};

  BEGIN
    y.data^ := SUBARRAY(x.data^, 0, NUMBER(y.data^));
    RETURN y;
  END Copy;


PROCEDURE FromInteger (x: INTEGER): T =
  VAR data: Value;
  BEGIN
    IF x = 0 THEN
      (*we cannot return Zero because we use FromInteger to initialize
         Zero*)
      RETURN T{NIL, 0, FALSE};
    ELSE
      data := NEW(Value, 1);
      data[0] := ABS(x);
      RETURN T{data, 1, x < 0};
    END;
  END FromInteger;



(*signed arithmetic*)

PROCEDURE Add (READONLY x, y: T): T =
  VAR z: T;

  BEGIN
    IF x.sign = y.sign THEN
      z := Rep.AddU(x, y);
      z.sign := x.sign;
    ELSE
      CASE Rep.CompareU(x, y) OF
      | 1 => z := Rep.SubU(x, y); z.sign := x.sign;
      | -1 => z := Rep.SubU(y, x); z.sign := y.sign;
      | 0 => z := Zero;
      END;
    END;
    RETURN z;
  END Add;

PROCEDURE Sub (READONLY x, y: T): T =
  VAR z: T;

  BEGIN
    IF x.sign # y.sign THEN
      z := Rep.AddU(x, y);
      z.sign := x.sign;
    ELSE
      CASE Rep.CompareU(x, y) OF
      | 1 => z := Rep.SubU(x, y); z.sign := x.sign;
      | -1 => z := Rep.SubU(y, x); z.sign := NOT x.sign;
      | 0 => z := Zero;
      END;
    END;
    RETURN z;
  END Sub;

PROCEDURE Neg (READONLY x: T): T =
  VAR y := x;
  BEGIN
    y.sign := NOT x.sign;
    RETURN y;
  END Neg;

PROCEDURE Conj (READONLY x: T): T =
  BEGIN
    RETURN x;
  END Conj;


PROCEDURE IsZero (READONLY x: T): BOOLEAN =
  BEGIN
    RETURN x.size = 0;
    (*RETURN x.size=0 OR x.size=1 AND x.data[0]=0;*)
  END IsZero;

PROCEDURE Compare (READONLY x, y: T): [-1 .. 1] =
  BEGIN
    IF x.sign # y.sign THEN
      IF x.sign THEN RETURN -1 ELSE RETURN 1 END
    ELSE
      IF x.sign THEN
        RETURN Rep.CompareU(y, x)
      ELSE
        RETURN Rep.CompareU(x, y)
      END
    END;
  END Compare;

PROCEDURE Equal (READONLY x, y: T): BOOLEAN =
  BEGIN
    IF x.sign # y.sign OR x.size # y.size THEN
      RETURN FALSE
    ELSE
      FOR j := x.size - 1 TO 0 BY -1 DO
        IF x.data[j] # y.data[j] THEN RETURN FALSE END;
      END;
      RETURN TRUE;
    END;
  END Equal;




PROCEDURE Mul (READONLY x, y: T): T =
  VAR z: T;

  BEGIN
    z := Rep.MulU(x, y);
    z.sign := x.sign # y.sign;
    RETURN z;
  END Mul;



PROCEDURE Div (READONLY x, y: T): T RAISES {Arith.Error} =
  VAR qr := Rep.DivModU(x, y);

  BEGIN
    qr.quot.sign := x.sign # y.sign;
    (*IF NOT Equal(r,Zero) THEN*)
    IF qr.rem.size # 0 THEN
      RAISE Arith.Error(NEW(Arith.ErrorIndivisible).init());
    END;
    RETURN qr.quot;
  END Div;

PROCEDURE Rec (READONLY x: T): T RAISES {Arith.Error} =
  BEGIN
    IF IsZero(x) THEN
      RAISE Arith.Error(NEW(Arith.ErrorDivisionByZero).init());
    ELSIF Equal(x, One) THEN
      RETURN One;
    ELSE
      RAISE Arith.Error(NEW(Arith.ErrorIndivisible).init());
    END;
  END Rec;

(*Is this correct?*)
PROCEDURE DivMod (READONLY x, y: T): QuotRem RAISES {Arith.Error} =
  VAR qr := Rep.DivModU(x, y);

  BEGIN
    qr.rem.sign := y.sign;
    qr.quot.sign := x.sign # y.sign;
    IF qr.quot.sign AND NOT IsZero(qr.rem) THEN (*means x.sign#y.sign*)
      qr.rem := Rep.SubU(y, qr.rem);
      IF IsZero(qr.quot) THEN
        IF qr.quot.sign THEN qr.quot := One; ELSE qr.quot := MinusOne; END;
      ELSE
        qr.quot := Rep.SubU(qr.quot, One);
      END;
    END;
    RETURN qr;
  END DivMod;

PROCEDURE Mod (READONLY x, y: T): T RAISES {Arith.Error} =
  BEGIN
    RETURN DivMod(x, y).rem;
  END Mod;

(*==========================*)
BEGIN
  Zero := FromInteger(0);
  One := FromInteger(1);
  Two := FromInteger(2);
  MinusOne := FromInteger(-1);

  (* billion := FromInteger (1000000000); *)
END BigInteger.
