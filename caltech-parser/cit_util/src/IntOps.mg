GENERIC MODULE IntOps(Int, TextIntTbl);
IMPORT Text;
IMPORT Scan AS yScan;
IMPORT Random;
IMPORT FloatMode, Lex;
IMPORT Pathname;
IMPORT FileRd;
IMPORT Rd;
IMPORT FileWr;
IMPORT Wr;
IMPORT OSError;
IMPORT Thread;

<* FATAL FloatMode.Trap, Lex.Error, Rd.Failure, Wr.Failure, Thread.Alerted, OSError.E *>

PROCEDURE ExtendedGCD(a, b: T; VAR aCoeff, bCoeff: T): T =
  BEGIN
    IF Compare(a,Int.Zero) = -1 THEN
      VAR
        g := ExtendedGCD(b,Negate(a),bCoeff,aCoeff);
      BEGIN
        aCoeff := Negate(aCoeff);
        RETURN g;
      END;
    END;
    IF Compare(b,Int.Zero) = -1 THEN
      RETURN ExtendedGCD(b,a,bCoeff,aCoeff);
    END;
    CASE Compare(a,b) OF
    | -1 => RETURN EGCD(a,b,aCoeff,bCoeff);
    | 0 => aCoeff:=Int.One; bCoeff:=Int.Zero; RETURN a;
    | 1 => RETURN EGCD(b,a,bCoeff,aCoeff);
    END;
  END ExtendedGCD;

(* assumes 0<=a<ABS(b) *)
PROCEDURE EGCD(a,b: T; VAR aCoeff,bCoeff: T): T =
  BEGIN
    IF Equal(a, Int.Zero) THEN
      aCoeff := Int.Zero;
      bCoeff := Int.One;
      RETURN b;
    END;
    VAR
      r,q,g,rCoeff: T;
    BEGIN
      (* r := Mod(b, a);
         q := Div(b, a); *)
      Int.Divide(b, a, q, r);
      g := EGCD(r, a, rCoeff, aCoeff);
      bCoeff := rCoeff;
      aCoeff := Sub(aCoeff,Mul(rCoeff,q));
      RETURN g;
      (* g = aCoeff*a + rCoeff*r
           = aCoeff*a + rCoeff*(b - q*a)
           = (aCoeff-q*rCoeff)*a + rCoeff*b *)
    END;
  END EGCD;

PROCEDURE Random1(source: Random.T; largerThan: T) : T =
  VAR
    chunkBase := New(16_100000);
    r := New(source.integer(0, 16_100000));
  BEGIN
    IF Compare(largerThan, chunkBase) = -1 THEN
      RETURN r;
    END;
    RETURN Add(r,Mul(chunkBase,Random1(source, Div(largerThan,chunkBase))));
  END Random1;

PROCEDURE Rand(source: Random.T; lessThan: T) : T =
  BEGIN
    RETURN Mod(Random1(source, Mul(lessThan, New(99999))), lessThan);
  END Rand;

PROCEDURE Scan(t: TEXT; base : CARDINAL := 10) : T =
  VAR
    chunkDigits := 5;
    sb:=base*base;
    chunkBase := New(sb*sb*base);
    l := Text.Length(t);
  BEGIN
    IF l <= chunkDigits THEN
      RETURN New(yScan.Int(t, base));
    END;
    IF Text.GetChar(t, 0) = '-' THEN
      RETURN Negate(Scan(Text.Sub(t, 1), base));
    END;
    DEC(l, chunkDigits);
    RETURN Add(Mul(chunkBase, Scan(Text.Sub(t, 0, l),base)),
               New(yScan.Int(Text.Sub(t, l),base)));
  END Scan;

PROCEDURE Exp(base, exp: T): T =
  BEGIN
    RETURN MultiExp(base, exp, Int.Zero, FALSE);
  END Exp;

PROCEDURE ModExp(base, exp, mod: T): T =
  <* FATAL NoneExists *>
  BEGIN
    IF Sign(exp) = -1 THEN
      RETURN ModExp(ModInverse(base, mod), Negate(exp), mod);
    END;
    RETURN MultiExp(base, exp, mod, TRUE);
  END ModExp;

PROCEDURE MultiExp(base, exp, mod: T; useMod: BOOLEAN): T =
  BEGIN
    CASE Sign(exp) OF
    | 0 => RETURN Int.One;
    | -1 => <* ASSERT FALSE *>
    | 1 =>
      VAR
        t: T;
      BEGIN
        IF Odd(exp) THEN
          t := Mul(base, MultiExp(base, Pred(exp), mod, useMod));
        ELSE
          t := Square(MultiExp(base, Half(exp), mod, useMod));
        END;
        IF useMod THEN
          t := Mod(t, mod);
        END;
        RETURN t;
      END;
    END;
  END MultiExp;

PROCEDURE ModInverse(a, mod: T): T RAISES {NoneExists} =
  VAR
    aCoeff, bCoeff, g: T;
  BEGIN
    g := ExtendedGCD(a,mod,aCoeff,bCoeff);
    IF NOT Equal(g, Int.One) THEN RAISE NoneExists; END;
    RETURN Mod(aCoeff, mod);
  END ModInverse;

PROCEDURE Read(fn: Pathname.T): TextIntTbl.T =
  VAR
    rd := FileRd.Open(fn);
    t: TEXT;
    i: INTEGER;
    tbl := NEW(TextIntTbl.Default).init();
  BEGIN
    TRY
      LOOP
        t := Rd.GetLine(rd);
        i := Text.FindChar(t, '=');
        EVAL tbl.put(Text.Sub(t,0,i), Scan(Text.Sub(t,i+1)));
      END;
    EXCEPT Rd.EndOfFile =>
    END;
    Rd.Close(rd);
    RETURN tbl;
  END Read;

PROCEDURE Write(fn: Pathname.T; tbl: TextIntTbl.T) =
  VAR
    iter := tbl.iterate();
    key: TEXT;
    val: T;
    wr := FileWr.Open(fn);
  BEGIN
    WHILE iter.next(key, val) DO
      Wr.PutText(wr, key & "=" & Int.Format(val) & "\n");
    END;
    Wr.Close(wr);
  END Write;

PROCEDURE ProbablyPrime(p: T): BOOLEAN =
  BEGIN RETURN Equal(ModExp(New(17), Pred(p), p), Int.One) AND
               Equal(ModExp(New(19), Pred(p), p), Int.One); END ProbablyPrime;
PROCEDURE Negate(a: T): T =
  BEGIN RETURN Mul(New(-1), a); END Negate;
PROCEDURE Sub(a, b: T): T =
  BEGIN RETURN Add(a, Negate(b)); END Sub;
PROCEDURE Square(a: T): T = BEGIN RETURN Mul(a,a); END Square;
PROCEDURE Odd(a: T): BOOLEAN =
  BEGIN RETURN Equal(Mod(a, New(2)),Int.One); END Odd;
PROCEDURE Pred(x: T): T = BEGIN RETURN Add(x, New(-1)); END Pred;
PROCEDURE Succ(x: T): T = BEGIN RETURN Add(x, Int.One); END Succ;
PROCEDURE Half(x: T): T = BEGIN RETURN Div(x, New(2)); END Half;
PROCEDURE GCD(a, b: T): T =
  VAR ac,bc: T; BEGIN RETURN ExtendedGCD(a,b,ac,bc); END GCD;
PROCEDURE IsOne(x: T): BOOLEAN = BEGIN RETURN Equal(x, Int.One); END IsOne;
PROCEDURE One(): T = BEGIN RETURN Int.One; END One;
PROCEDURE Zero(): T = BEGIN RETURN Int.Zero; END Zero;
PROCEDURE RelPrime(a,b: T): BOOLEAN=BEGIN RETURN IsOne(GCD(a,b));END RelPrime;
PROCEDURE Old(a: T): INTEGER = BEGIN RETURN ROUND(Int.ToLongReal(a)); END Old;
PROCEDURE Log2i(a:T):INTEGER=BEGIN RETURN Text.Length(Format(a,2))-1;END Log2i;
PROCEDURE Exp2i(i: INTEGER): T = BEGIN RETURN Exp(New(2),New(i)); END Exp2i;
PROCEDURE Log2(a: T): T = BEGIN RETURN New(Log2i(a)); END Log2;
PROCEDURE ModMul(a,b,mod: T): T = BEGIN RETURN Mod(Mul(a,b),mod); END ModMul;
PROCEDURE ModDiv(a,b,mod: T): T = <* FATAL NoneExists *>
  BEGIN RETURN ModMul(a, ModInverse(b, mod), mod); END ModDiv;


BEGIN END IntOps.
