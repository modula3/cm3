(* $Id$ *)
GENERIC MODULE Rational(BaseInt);
IMPORT Word;

PROCEDURE CheckInvariant(READONLY x : T) : T =
  BEGIN
    (* in lowest terms *)
    <* ASSERT NOT BaseInt.Equal(BaseInt.Mod(x.n,x.d), BaseInt.New(0)) OR
                  BaseInt.Equal(x.d,BaseInt.New(1)) *>

    (* check that denominator actually is a nat'l number. *)
    <* ASSERT BaseInt.Compare(x.d, BaseInt.New(0)) = 1 *>

    RETURN x
  END CheckInvariant;
    
PROCEDURE NewInt(READONLY a : BaseInt.T) : T =
  BEGIN RETURN CheckInvariant(T { a, BaseInt.New(1) }) END NewInt;

PROCEDURE New(READONLY n : BaseInt.T; READONLY d : BaseInt.Natural) : T =
  BEGIN RETURN CheckInvariant(LowestTerms(T { n, d })) END New;

PROCEDURE NewSimple(n : INTEGER; d : [1..LAST(CARDINAL)]) : T =
  BEGIN RETURN New(BaseInt.New(n),BaseInt.New(d)) END NewSimple;

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN = 
  BEGIN RETURN Compare(a,b) = 0 END Equal;

PROCEDURE Compare(READONLY a, b : T) : CompRet =
  VAR
    aa := BaseInt.Mul(a.n,b.d);
    bb := BaseInt.Mul(b.n,a.d);
  BEGIN
    RETURN BaseInt.Compare(aa,bb) 
  END Compare;

PROCEDURE LowestTerms(READONLY a : T) : T =
  VAR
    gcd := BaseGCD(BaseInt.Abs(a.n),a.d);
  BEGIN
    RETURN CheckInvariant(T { BaseInt.Div(a.n,gcd), BaseInt.Div(a.d,gcd) })
  END LowestTerms;

PROCEDURE Mul(READONLY a, b : T) : T =
  VAR
    res := T { BaseInt.Mul(a.n,b.n), BaseInt.Mul(a.d,b.d) };
  BEGIN
    RETURN CheckInvariant(LowestTerms( res ))
  END Mul;

PROCEDURE Add(READONLY a, b : T) : T =
  VAR
    lcm := BaseLCM(a.d,b.d);
    am := BaseInt.Div(lcm,a.d);
    bm := BaseInt.Div(lcm,b.d);
    an := BaseInt.Mul(am,a.n);
    bn := BaseInt.Mul(bm,b.n);
    res := T { BaseInt.Add(an,bn), lcm };
  BEGIN
    RETURN CheckInvariant(LowestTerms( res ))
  END Add;

PROCEDURE Sub(READONLY a, b : T) : T =
  BEGIN RETURN Add(a, Mul(NewInt(BaseInt.New(-1)),b)) END Sub;

PROCEDURE Div(READONLY a, b : T) : T =
  BEGIN RETURN Mul(a, Reciprocal(b)) END Div;

PROCEDURE Reciprocal(READONLY a : T) : T =
  BEGIN 
    RETURN CheckInvariant(T { BaseInt.Mul(BaseInt.New(BaseInt.Sign(a.n)),a.d), BaseInt.Abs(a.n) } )
  END Reciprocal;

VAR
  Bzero := BaseInt.New(0);

PROCEDURE BaseGCD(a, b : BaseInt.T) : BaseInt.T =
  VAR
    c : CompRet;
  BEGIN
    IF BaseInt.Equal(a,Bzero) OR BaseInt.Equal(b,Bzero) THEN 
      RETURN BaseInt.Max(a,b)
    END;
      
    c := BaseInt.Compare(a, b);
    CASE c OF
      1 => RETURN BaseGCD(BaseInt.Mod(a,b),b)
    |
      -1 => RETURN BaseGCD(a,BaseInt.Mod(b,a))
    |
      0 => RETURN a (* really unnecessary to test for this... *)
    END
  END BaseGCD;

PROCEDURE BaseLCM(a, b : BaseInt.T) : BaseInt.T =
  VAR
    gcd := BaseGCD(a,b);
    ad := BaseInt.Div(a,gcd);
    bd := BaseInt.Div(b,gcd);
  BEGIN
    RETURN BaseInt.Mul(BaseInt.Mul(ad,bd),gcd)
  END BaseLCM;

PROCEDURE Format(a : T; base : CARDINAL) : TEXT =
  BEGIN
    RETURN BaseInt.Format(a.n,base) & " / " & BaseInt.Format(a.d,base)
  END Format;

PROCEDURE Hash(a : T) : Word.T = 
  BEGIN RETURN Word.Plus(BaseInt.Hash(a.n),BaseInt.Hash(a.d)) END Hash;

PROCEDURE ToLongReal(a : T) : LONGREAL =
  BEGIN RETURN BaseInt.ToLongReal(a.n)/BaseInt.ToLongReal(a.d) END ToLongReal;

BEGIN END Rational.
