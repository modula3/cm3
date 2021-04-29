UNSAFE MODULE Mpfr;
IMPORT MpfrP AS P;
FROM Ctypes IMPORT char_star;
IMPORT Math;
IMPORT Debug;
FROM Fmt IMPORT F, Int;
IMPORT Text;
IMPORT WeakRef;

REVEAL
  T = BRANDED Brand OBJECT
    val : P.MpfrPtrT;
    (* this must be an UNTRACED REF so that the GC doesnt move it around *)
  END;

PROCEDURE New(prec : CARDINAL) : T =
  VAR
    val := P.alloc();
  BEGIN
    P.init2(L(val), prec);
    WITH new = NEW(T, val := val) DO
      EVAL WeakRef.FromRef(new, CleanUp);
      RETURN new
    END
  END New;

PROCEDURE CleanUp(<*UNUSED*>READONLY w : WeakRef.T; r : REFANY) =
  BEGIN
    WITH this = NARROW(r, T) DO
      P.free(this.val)
    END
  END CleanUp;

PROCEDURE I2T(int : INTEGER) : Ternary =
  BEGIN
    IF    int < 0 THEN RETURN -1
    ELSIF int > 0 THEN RETURN +1
    ELSE  RETURN 0
    END
  END I2T;

PROCEDURE L(p : P.MpfrPtrT) : P.MpfrPtrT =
  BEGIN
    RETURN P.deref(p)
  END L;

PROCEDURE RM2C(rm : RM) : INTEGER =
  BEGIN
    (* we could easily make this better by querying the C layer in the 
       initialization of the module *)
    IF rm = RM.NA THEN RETURN -1 ELSE RETURN ORD(rm) END
  END RM2C;
  
PROCEDURE Set     (tgt, v : T;            rnd := RM.N) : Ternary =
  BEGIN RETURN I2T(P.set(L(tgt.val), L(v.val), RM2C(rnd))) END Set;

PROCEDURE SetLR   (tgt : T; v : LONGREAL; rnd := RM.N) : Ternary =
  BEGIN RETURN I2T(P.set_d(L(tgt.val), v, RM2C(rnd))) END SetLR;
  
PROCEDURE SetInt  (tgt : T; v : INTEGER;  rnd := RM.N) : Ternary =
  BEGIN RETURN I2T(P.set_si(L(tgt.val), v, RM2C(rnd))) END SetInt;

PROCEDURE GetLR   (from : T; rnd := RM.N) : LONGREAL =
  BEGIN RETURN P.get_d(L(from.val), RM2C(rnd)) END GetLR;
  
PROCEDURE Swap(a, b : T) =
  BEGIN P.swap(L(a.val), L(b.val)) END Swap;

PROCEDURE Compare(a, b : T) : Ternary =
  BEGIN RETURN I2T(P.cmp(L(a.val), L(b.val))) END Compare;

PROCEDURE Ndigits(base, prec : CARDINAL) : CARDINAL =
  BEGIN
    RETURN CEILING(FLOAT(prec, LONGREAL) *
                   Math.log(2.0d0) / Math.log(FLOAT(base,LONGREAL)))
  END Ndigits;

PROCEDURE Format(v : T; base : PrintBase := 10; rnd := RM.N) : TEXT =
  VAR
    prec :=  P.get_prec(L(v.val));
    n := Ndigits(base, prec);
    siz := n + 2;
    buf := NEW(REF ARRAY OF CHAR, siz);
    exp : P.Exp;
    mlen : CARDINAL;
    edit := TRUE;
  BEGIN
    WITH str = LOOPHOLE(ADR(buf[0]), char_star) DO
      EVAL P.get_str(str, ADR(exp), base, siz, L(v.val), RM2C(rnd));

      mlen := siz;
      FOR i := FIRST(buf^) TO LAST(buf^) DO
        IF buf[i] = '\000' THEN
          mlen := i;
          EXIT
        END;
        IF buf[i] = '@' THEN
          edit := FALSE
        END
      END;

      IF edit THEN
        IF buf[0] = '-' THEN
          RETURN F("-%s.%se%s",
                   Text.FromChars(SUBARRAY(buf^, 1, 1)),
                   Text.FromChars(SUBARRAY(buf^, 2, mlen - 2)),
                   Int(exp - 1))
        ELSE
          RETURN F("%s.%se%s",
                   Text.FromChars(SUBARRAY(buf^, 0, 1)),
                   Text.FromChars(SUBARRAY(buf^, 1, mlen - 1)),
                   Int(exp - 1))
        END          
      ELSE
        RETURN Text.FromChars(SUBARRAY(buf^, 0, mlen))
      END;

    END
  END Format;

PROCEDURE FormatInt(v : T; base : PrintBase := 10; rnd := RM.N) : TEXT =
  VAR
    prec :=  P.get_prec(L(v.val));
    n := Ndigits(base, prec);
    siz := n + 2;
    buf := NEW(REF ARRAY OF CHAR, siz);
    exp : P.Exp;
    mlen : CARDINAL;
    edit := TRUE;
  BEGIN
    WITH str = LOOPHOLE(ADR(buf[0]), char_star) DO
      EVAL P.get_str(str, ADR(exp), base, siz, L(v.val), RM2C(rnd));

      mlen := siz;
      FOR i := FIRST(buf^) TO LAST(buf^) DO
        IF buf[i] = '\000' THEN
          mlen := i;
          EXIT
        END;
        IF buf[i] = '@' THEN
          edit := FALSE
        END
      END;

      IF edit AND exp <= mlen - 1 THEN
        IF buf[0] = '-' THEN
          RETURN F("-%s",
                   Text.FromChars(SUBARRAY(buf^, 1, exp)))
        ELSE
          RETURN F("%s",
                   Text.FromChars(SUBARRAY(buf^, 0, exp)))
        END
      ELSE
        RETURN Format(v, base, rnd) (* give up, use sci. not. *)
      END;

    END
  END FormatInt;

PROCEDURE I2B(int : INTEGER) : BOOLEAN =
  BEGIN RETURN int # 0 END I2B;

PROCEDURE NanP    (v : T) : BOOLEAN =
  BEGIN RETURN I2B(P.nan_p(L(v.val))) END NanP;
  
PROCEDURE InfP    (v : T) : BOOLEAN =
  BEGIN RETURN I2B(P.inf_p(L(v.val))) END InfP;
  
PROCEDURE NumberP (v : T) : BOOLEAN =
  BEGIN RETURN I2B(P.number_p(L(v.val))) END NumberP;
  
PROCEDURE ZeroP   (v : T) : BOOLEAN =
  BEGIN RETURN I2B(P.zero_p(L(v.val))) END ZeroP;
  
PROCEDURE RegularP(v : T) : BOOLEAN =
  BEGIN RETURN I2B(P.regular_p(L(v.val))) END RegularP;

  (**********************************************************************)
  
PROCEDURE Sign    (v : T) : Ternary =
  BEGIN
    WITH int = P.cmp_ui(L(v.val), 0),
         res = I2T(int) DO
      (*
      Debug.Out(F("Mpfr.Sign(%s): int = %s ; res = %s",
                  Format(v),
                  Int(int),
                  Int(res)));
      *)
      RETURN res
    END
  END Sign;

  (**********************************************************************)

PROCEDURE GreaterP     (v, w : T) : BOOLEAN =
  BEGIN RETURN I2B(P.greater_p(L(v.val), L(w.val))) END GreaterP;
  
PROCEDURE GreaterEqualP(v, w : T) : BOOLEAN =
  BEGIN RETURN I2B(P.greaterequal_p(L(v.val), L(w.val))) END GreaterEqualP;
  
PROCEDURE LessP        (v, w : T) : BOOLEAN =
  BEGIN RETURN I2B(P.less_p(L(v.val), L(w.val))) END LessP;
  
PROCEDURE LessEqualP   (v, w : T) : BOOLEAN =
  BEGIN RETURN I2B(P.lessequal_p(L(v.val), L(w.val))) END LessEqualP;
  
PROCEDURE EqualP       (v, w : T) : BOOLEAN =
  BEGIN RETURN I2B(P.equal_p(L(v.val), L(w.val))) END EqualP;

PROCEDURE LessGreaterP (v, w : T) : BOOLEAN =
  BEGIN RETURN I2B(P.lessgreater_p(L(v.val), L(w.val))) END LessGreaterP;
  
  (**********************************************************************)

PROCEDURE Add(tgt, a, b : T; rnd := RM.N) : Ternary =
  BEGIN RETURN I2T(P.add(L(tgt.val), L(a.val), L(b.val), RM2C(rnd))) END Add;
  
PROCEDURE Sub(tgt, a, b : T; rnd := RM.N) : Ternary =
  BEGIN RETURN I2T(P.sub(L(tgt.val), L(a.val), L(b.val), RM2C(rnd))) END Sub;

PROCEDURE Mul(tgt, a, b : T; rnd := RM.N) : Ternary =
  BEGIN RETURN I2T(P.mul(L(tgt.val), L(a.val), L(b.val), RM2C(rnd))) END Mul;

PROCEDURE Div(tgt, a, b : T; rnd := RM.N) : Ternary =
  BEGIN RETURN I2T(P.div(L(tgt.val), L(a.val), L(b.val), RM2C(rnd))) END Div;

PROCEDURE Pow(tgt, a, b : T; rnd := RM.N) : Ternary =
  BEGIN RETURN I2T(P.pow(L(tgt.val), L(a.val), L(b.val), RM2C(rnd))) END Pow;

  (**********************************************************************)
  
PROCEDURE Sqrt(tgt, a : T; rnd := RM.N) : Ternary =
  BEGIN RETURN I2T(P.sqrt(L(tgt.val), L(a.val), RM2C(rnd))) END Sqrt;
  
PROCEDURE Neg(tgt, a : T; rnd := RM.N) : Ternary =
  BEGIN RETURN I2T(P.neg(L(tgt.val), L(a.val), RM2C(rnd))) END Neg;

PROCEDURE Abs(tgt, a : T; rnd := RM.N) : Ternary =
  BEGIN RETURN I2T(P.abs(L(tgt.val), L(a.val), RM2C(rnd))) END Abs;

PROCEDURE Log(tgt, a : T; rnd := RM.N) : Ternary =
  BEGIN RETURN I2T(P.log(L(tgt.val), L(a.val), RM2C(rnd))) END Log;
  
PROCEDURE Exp(tgt, a : T; rnd := RM.N) : Ternary =
  BEGIN RETURN I2T(P.exp(L(tgt.val), L(a.val), RM2C(rnd))) END Exp;

PROCEDURE Cos(tgt, a : T; rnd := RM.N) : Ternary =
  BEGIN RETURN I2T(P.cos(L(tgt.val), L(a.val), RM2C(rnd))) END Cos;

PROCEDURE Sin(tgt, a : T; rnd := RM.N) : Ternary =
  BEGIN RETURN I2T(P.sin(L(tgt.val), L(a.val), RM2C(rnd))) END Sin;

PROCEDURE Tan(tgt, a : T; rnd := RM.N) : Ternary =
  BEGIN RETURN I2T(P.tan(L(tgt.val), L(a.val), RM2C(rnd))) END Tan;

PROCEDURE Gamma(tgt, a : T; rnd := RM.N) : Ternary =
  BEGIN RETURN I2T(P.gamma(L(tgt.val), L(a.val), RM2C(rnd))) END Gamma;

  (**********************************************************************)

PROCEDURE ConstLog2(tgt : T; rnd := RM.N) : Ternary =
  BEGIN RETURN I2T(P.const_log2(L(tgt.val), RM2C(rnd))) END ConstLog2;

PROCEDURE ConstPi(tgt : T; rnd := RM.N) : Ternary= 
  BEGIN RETURN I2T(P.const_pi(L(tgt.val), RM2C(rnd))) END ConstPi;

VAR
  expected : CARDINAL;
BEGIN
  CASE BITSIZE(INTEGER) OF
    32 => expected := P.PrecFormat32
  |
    64 => expected := P.PrecFormat64
  ELSE
    <*ASSERT FALSE*>
  END;
  WITH got      = P.GetPrecFormat() DO
    IF got # expected THEN
      Debug.Error(F("P.GetPrecFormat = %s, expected %s!",
                    Int(got), Int(expected)))
    END
  END
END Mpfr.
