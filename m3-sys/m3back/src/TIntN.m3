MODULE TIntN; (* TInt but with specified precision, in bytes *)

IMPORT Fmt, TInt;
FROM Target IMPORT Int;

PROCEDURE FromTInt (READONLY x: Int;  n: CARDINAL;  VAR r: T): BOOLEAN =
  BEGIN
    r.n := n;
    RETURN TInt.Extend(x, n, r.x);
  END FromTInt;

PROCEDURE ToTInt (READONLY r: T): Int =
  VAR x: Int;
  BEGIN
    EVAL TInt.Extend(r.x, r.n, x);
    RETURN x;
  END ToTInt;

PROCEDURE FromInt (a: INTEGER;  n: CARDINAL;  VAR r: T): BOOLEAN =
  VAR x: Int;
  BEGIN
    RETURN TInt.FromInt(a, x) AND FromTInt(x, n, r);
  END FromInt;

PROCEDURE ToInt (READONLY r: T;  VAR x: INTEGER): BOOLEAN =
  BEGIN
    RETURN TInt.ToInt(ToTInt(r), x);
  END ToInt;

PROCEDURE Add (READONLY a, b: T;  VAR r: T): BOOLEAN =
  VAR x: Int;
  BEGIN
    RETURN TInt.Add(ToTInt(a), ToTInt(b), x) AND FromTInt(x, MIN(a.n, b.n), r);
  END Add;

PROCEDURE Subtract (READONLY a, b: T;  VAR r: T): BOOLEAN =
  VAR x: Int;
  BEGIN
    RETURN TInt.Subtract(ToTInt(a), ToTInt(b), x) AND FromTInt(x, MIN(a.n, b.n), r);
  END Subtract;

PROCEDURE Negate (READONLY a: T;  VAR r: T): BOOLEAN =
  BEGIN
    RETURN Subtract(Zero, a, r);
  END Negate;
  
PROCEDURE Abs (READONLY a: T;  VAR r: T): BOOLEAN =
  BEGIN
    IF GE(a, Zero) THEN
      r := a;
      RETURN TRUE;
    END;
    RETURN Negate(a, r);
  END Abs;

PROCEDURE Multiply (READONLY a, b: T;  VAR r: T): BOOLEAN =
  VAR x: Int;
  BEGIN
    RETURN TInt.Multiply(ToTInt(a), ToTInt(b), x) AND FromTInt(x, MIN(a.n, b.n), r);
  END Multiply;

PROCEDURE Div (READONLY num, den: T;  VAR q: T): BOOLEAN =
  VAR x: Int;
  BEGIN
    RETURN TInt.Div(ToTInt(num), ToTInt(den), x) AND FromTInt(x, MIN(num.n, den.n), q);
  END Div;

PROCEDURE Mod (READONLY num, den: T;  VAR r: T): BOOLEAN =
  VAR x: Int;
  BEGIN
    RETURN TInt.Mod(ToTInt(num), ToTInt(den), x) AND FromTInt(x, MIN(num.n, den.n), r);
  END Mod;

PROCEDURE EQ (READONLY a, b: T): BOOLEAN =
  BEGIN
    RETURN TInt.EQ(ToTInt(a), ToTInt(b));
  END EQ;

PROCEDURE LT (READONLY a, b: T): BOOLEAN =
  BEGIN
    RETURN TInt.LT(ToTInt(a), ToTInt(b));
  END LT;

PROCEDURE LE (READONLY a, b: T): BOOLEAN =
  BEGIN
    RETURN TInt.LE(ToTInt(a), ToTInt(b));
  END LE;

PROCEDURE NE (READONLY a, b: T): BOOLEAN =
  BEGIN
    RETURN TInt.NE(ToTInt(a), ToTInt(b));
  END NE;

PROCEDURE GT (READONLY a, b: T): BOOLEAN =
  BEGIN
    RETURN TInt.GT(ToTInt(a), ToTInt(b));
  END GT;

PROCEDURE GE (READONLY a, b: T): BOOLEAN = 
  BEGIN
    RETURN TInt.GE(ToTInt(a), ToTInt(b));
  END GE;

PROCEDURE ToText (READONLY r: T): TEXT =
  BEGIN
    RETURN TInt.ToText(ToTInt(r));
  END ToText;

PROCEDURE ToChars (READONLY r: T;  VAR buf: ARRAY OF CHAR): INTEGER =
  BEGIN
    RETURN TInt.ToChars(ToTInt(r), buf);
  END ToChars;

PROCEDURE ToDiagnosticText(a: T): TEXT =
  BEGIN
    RETURN "n:" & Fmt.Unsigned(a.n) & ",x:" & TargetIntToDiagnosticText(a.x);
  END ToDiagnosticText;

PROCEDURE TargetIntToDiagnosticText(a: Int): TEXT =
  VAR t := "";
  BEGIN
    FOR i := FIRST(a) TO LAST(a) DO
      t := t & Fmt.Unsigned(a[i]);
      IF i # LAST(a) THEN
        t := t & ",";
      END;
    END;
    RETURN t;
  END TargetIntToDiagnosticText;

BEGIN
END TIntN.
