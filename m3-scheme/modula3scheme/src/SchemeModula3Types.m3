(* $Id$ *)

MODULE SchemeModula3Types;
FROM SchemeUtils IMPORT Stringify;
IMPORT Scheme;
IMPORT SchemeObject, SchemeString, SchemeBoolean, SchemeLongReal, SchemeChar;
IMPORT SchemeInt, Mpz, Mpfr, SchemeMpfr, SchemeRational, SchemeDual;
IMPORT SchemePrimitive;
FROM Scheme IMPORT E, Object;
IMPORT SchemeProcedure, SchemeSymbol;
FROM SchemeUtils IMPORT First, Rest;

PROCEDURE ToScheme_MUTEX(m : MUTEX) : SchemeObject.T =
  BEGIN RETURN m END ToScheme_MUTEX;

PROCEDURE ToScheme_TEXT(t : TEXT) : SchemeObject.T =
  BEGIN RETURN SchemeString.FromText(t) END ToScheme_TEXT;

PROCEDURE ToScheme_UNTRACED_ROOT(u : UNTRACED ROOT) : SchemeObject.T =
  BEGIN RETURN NEW(UntracedRoot, u := u) END ToScheme_UNTRACED_ROOT;

PROCEDURE ToScheme_ROOT(r : ROOT)          : SchemeObject.T =
  BEGIN RETURN r END ToScheme_ROOT;

PROCEDURE ToScheme_ADDRESS(a : ADDRESS) : SchemeObject.T =
  BEGIN RETURN NEW(Address, a := a) END ToScheme_ADDRESS;

PROCEDURE ToScheme_REFANY(r : REFANY) : SchemeObject.T =
  BEGIN RETURN r END ToScheme_REFANY;

PROCEDURE ToScheme_EXTENDED(x : EXTENDED) : SchemeObject.T =
  BEGIN RETURN SchemeLongReal.FromLR(FLOAT(x, LONGREAL)) END ToScheme_EXTENDED;

PROCEDURE ToScheme_REAL(r : REAL) : SchemeObject.T =
  BEGIN RETURN SchemeLongReal.FromLR(FLOAT(r, LONGREAL)) END ToScheme_REAL;

PROCEDURE ToScheme_LONGREAL(l : LONGREAL) : SchemeObject.T =
  BEGIN RETURN SchemeLongReal.FromLR(l) END ToScheme_LONGREAL;

PROCEDURE ToScheme_CHAR(c : CHAR) : SchemeObject.T =
  BEGIN RETURN SchemeChar.Character(c) END ToScheme_CHAR;

PROCEDURE ToScheme_BOOLEAN(b : BOOLEAN) : SchemeObject.T =
  BEGIN RETURN SchemeBoolean.Truth(b) END ToScheme_BOOLEAN;

PROCEDURE ToScheme_CARDINAL(c : CARDINAL) : SchemeObject.T =
  BEGIN RETURN SchemeInt.FromI(c) END ToScheme_CARDINAL;

PROCEDURE ToScheme_INTEGER(i : INTEGER) : SchemeObject.T =
  BEGIN RETURN SchemeInt.FromI(i) END ToScheme_INTEGER;

PROCEDURE ToScheme_Mpz_T(m : Mpz.T) : SchemeObject.T =
  BEGIN RETURN SchemeInt.MpzToScheme(m) END ToScheme_Mpz_T;

(**********************************************************************)

PROCEDURE ToModula_MUTEX(m : SchemeObject.T) : MUTEX RAISES { Scheme.E } =
  BEGIN
    IF NOT ISTYPE(m, MUTEX) THEN
      RAISE Scheme.E("expected a Modula-3 MUTEX: " & Stringify(m))
    END;
    RETURN m
  END ToModula_MUTEX;

PROCEDURE ToModula_TEXT(t : SchemeObject.T) : TEXT RAISES { Scheme.E } =
  BEGIN RETURN SchemeString.ToText(t) END ToModula_TEXT;

PROCEDURE ToModula_UNTRACED_ROOT(u : SchemeObject.T) : UNTRACED ROOT RAISES { Scheme.E } =
    BEGIN
    IF NOT ISTYPE(u, UntracedRoot) THEN
      RAISE Scheme.E("expected an UntracedRoot: " & Stringify(u))
    END;
    RETURN NARROW(u,UntracedRoot).u
  END ToModula_UNTRACED_ROOT; 

PROCEDURE ToModula_ROOT(r : SchemeObject.T) : ROOT RAISES { Scheme.E } =
  BEGIN
    IF NOT ISTYPE(r, ROOT) THEN
      RAISE Scheme.E("expected a Modula-3 ROOT: " & Stringify(r))
    END;
    RETURN r
  END ToModula_ROOT;
  

PROCEDURE ToModula_ADDRESS(u : SchemeObject.T) : ADDRESS RAISES { Scheme.E } =
  BEGIN
    IF NOT ISTYPE(u, Address) THEN
      RAISE Scheme.E("expected an Address: " & Stringify(u))
    END;
    RETURN NARROW(u,Address).a
  END ToModula_ADDRESS;

PROCEDURE ToModula_REFANY(r : SchemeObject.T) : REFANY RAISES { }  =
  BEGIN RETURN r END ToModula_REFANY;

PROCEDURE ToModula_EXTENDED(x : SchemeObject.T) : EXTENDED RAISES { Scheme.E }=
  BEGIN RETURN FLOAT(SchemeLongReal.FromO(x), EXTENDED) END ToModula_EXTENDED;

PROCEDURE ToModula_REAL(r : SchemeObject.T) : REAL RAISES { Scheme.E } =
  BEGIN RETURN FLOAT(SchemeLongReal.FromO(r), REAL) END ToModula_REAL;

PROCEDURE ToModula_LONGREAL(l : SchemeObject.T) : LONGREAL RAISES { Scheme.E }=
  BEGIN RETURN SchemeLongReal.FromO(l) END ToModula_LONGREAL;

PROCEDURE ToModula_CHAR(c : SchemeObject.T) : CHAR RAISES { Scheme.E } =
  BEGIN RETURN SchemeChar.Char(c) END ToModula_CHAR;

PROCEDURE ToModula_BOOLEAN(b : SchemeObject.T) : BOOLEAN RAISES { } =
  BEGIN RETURN SchemeBoolean.TruthO(b) END ToModula_BOOLEAN;

PROCEDURE ToModula_CARDINAL(c : SchemeObject.T) : CARDINAL RAISES { Scheme.E }=
  BEGIN
    TYPECASE c OF
      SchemeInt.T(ri) =>
        IF ri^ < 0 THEN
          RAISE Scheme.E("CARDINAL out of range : " & Stringify(c))
        END;
        RETURN ri^
    | Mpz.T(m) =>
      IF Mpz.fits_slong_p(m) # 0 THEN
        WITH i = Mpz.get_si(m) DO
          IF i < 0 THEN
            RAISE Scheme.E("CARDINAL out of range : " & Stringify(c))
          END;
          RETURN i
        END
      ELSE
        RAISE Scheme.E("CARDINAL out of range : " & Stringify(c))
      END
    ELSE
      WITH flt = SchemeLongReal.FromO(c) DO
        IF flt < FLOAT(FIRST(CARDINAL),LONGREAL) OR
           flt > FLOAT(LAST(CARDINAL),LONGREAL) THEN
          RAISE Scheme.E("CARDINAL out of range : " & Stringify(c))
        END;
        WITH int = ROUND(flt) DO
          IF FLOAT(int,LONGREAL) # flt THEN
            RAISE Scheme.E("Not an integer : " & Stringify(c))
          END;
          RETURN int
        END
      END
    END
  END ToModula_CARDINAL;

PROCEDURE ToModula_INTEGER(c : SchemeObject.T) : INTEGER RAISES { Scheme.E }=
  BEGIN
    TYPECASE c OF
      SchemeInt.T(ri) => RETURN ri^
    | Mpz.T(m) =>
      IF Mpz.fits_slong_p(m) # 0 THEN
        RETURN Mpz.get_si(m)
      ELSE
        RAISE Scheme.E("INTEGER out of range : " & Stringify(c))
      END
    ELSE
      WITH flt = SchemeLongReal.FromO(c) DO
        IF flt < FLOAT(FIRST(INTEGER),LONGREAL) OR
           flt > FLOAT(LAST(INTEGER),LONGREAL) THEN
          RAISE Scheme.E("INTEGER out of range : " & Stringify(c))
        END;
        WITH int = ROUND(flt) DO
          IF FLOAT(int,LONGREAL) # flt THEN
            RAISE Scheme.E("Not an integer : " & Stringify(c))
          END;
          RETURN int
        END
      END
    END
  END ToModula_INTEGER;

PROCEDURE ToModula_Mpz_T(x : SchemeObject.T) : Mpz.T RAISES { Scheme.E } =
  BEGIN
    TYPECASE x OF
      SchemeInt.T(ri) => RETURN Mpz.NewInt(ri^)
    | Mpz.T(m) => RETURN m
    ELSE
      WITH flt = SchemeLongReal.FromO(x) DO
        WITH int = ROUND(flt) DO
          IF FLOAT(int, LONGREAL) # flt THEN
            RAISE Scheme.E("Not an integer : " & Stringify(x))
          END;
          RETURN Mpz.NewInt(int)
        END
      END
    END
  END ToModula_Mpz_T;

PROCEDURE ToScheme_Mpfr_T(m : Mpfr.T) : SchemeObject.T =
  BEGIN
    RETURN SchemeMpfr.FromMpfr(m)
  END ToScheme_Mpfr_T;

PROCEDURE ToModula_Mpfr_T(x : SchemeObject.T) : Mpfr.T RAISES { Scheme.E } =
  BEGIN
    TYPECASE x OF
      SchemeMpfr.T(sm) => RETURN sm.val
    | SchemeLongReal.T(lr) =>
      (* convert LONGREAL to Mpfr with 53-bit precision *)
      VAR m := Mpfr.New(53); BEGIN
        EVAL Mpfr.SetLR(m, lr^);
        RETURN m
      END
    | SchemeInt.T(ri) =>
      VAR m := Mpfr.New(64); BEGIN
        EVAL Mpfr.SetInt(m, ri^);
        RETURN m
      END
    | Mpz.T(mz) =>
      (* convert via LONGREAL — loses precision for very large numbers *)
      VAR m := Mpfr.New(64); BEGIN
        EVAL Mpfr.SetLR(m, Mpz.get_d(mz));
        RETURN m
      END
    | SchemeRational.T(r) =>
      (* compute num/den as Mpfr *)
      VAR prec : CARDINAL := 128;
          mNum := Mpfr.New(prec);
          mDen := Mpfr.New(prec);
          result := Mpfr.New(prec);
      BEGIN
        EVAL Mpfr.SetLR(mNum, Mpz.get_d(r.num));
        EVAL Mpfr.SetLR(mDen, Mpz.get_d(r.den));
        EVAL Mpfr.Div(result, mNum, mDen);
        RETURN result
      END
    ELSE
      RAISE Scheme.E("expected a number for Mpfr.T: " & Stringify(x))
    END
  END ToModula_Mpfr_T;

PROCEDURE InteropRoundtripApply(<*UNUSED*>p : SchemeProcedure.T;
                                <*UNUSED*>interp : Scheme.T;
                                args : Object) : Object RAISES { E } =
  VAR
    sym := Scheme.SymbolCheck(First(args));
    val := First(Rest(args));
  BEGIN
    IF sym = SchemeSymbol.FromText("INTEGER") THEN
      RETURN ToScheme_INTEGER(ToModula_INTEGER(val))
    ELSIF sym = SchemeSymbol.FromText("CARDINAL") THEN
      RETURN ToScheme_CARDINAL(ToModula_CARDINAL(val))
    ELSIF sym = SchemeSymbol.FromText("LONGREAL") THEN
      RETURN ToScheme_LONGREAL(ToModula_LONGREAL(val))
    ELSIF sym = SchemeSymbol.FromText("REAL") THEN
      RETURN ToScheme_REAL(ToModula_REAL(val))
    ELSIF sym = SchemeSymbol.FromText("EXTENDED") THEN
      RETURN ToScheme_EXTENDED(ToModula_EXTENDED(val))
    ELSIF sym = SchemeSymbol.FromText("Mpz_T") THEN
      RETURN ToScheme_Mpz_T(ToModula_Mpz_T(val))
    ELSIF sym = SchemeSymbol.FromText("Mpfr_T") THEN
      RETURN ToScheme_Mpfr_T(ToModula_Mpfr_T(val))
    ELSIF sym = SchemeSymbol.FromText("TEXT") THEN
      RETURN ToScheme_TEXT(ToModula_TEXT(val))
    ELSIF sym = SchemeSymbol.FromText("BOOLEAN") THEN
      RETURN ToScheme_BOOLEAN(ToModula_BOOLEAN(val))
    ELSIF sym = SchemeSymbol.FromText("CHAR") THEN
      RETURN ToScheme_CHAR(ToModula_CHAR(val))
    ELSIF sym = SchemeSymbol.FromText("SchemeDual_T") THEN
      IF NOT ISTYPE(val, SchemeDual.T) THEN
        RAISE Scheme.E("expected a SchemeDual.T: " & Stringify(val))
      END;
      RETURN val
    ELSIF sym = SchemeSymbol.FromText("REFANY") THEN
      RETURN ToScheme_REFANY(ToModula_REFANY(val))
    ELSE
      RAISE Scheme.E("interop-roundtrip: unknown type: " & Stringify(sym))
    END
  END InteropRoundtripApply;

PROCEDURE Extend(prims : SchemePrimitive.ExtDefiner)  : SchemePrimitive.ExtDefiner =
  BEGIN
    prims.addPrim("scheme-modula-conversion-mode", NEW(SchemeProcedure.T,
                                                       apply := ConversionModeApply),
                  1, 1);
    prims.addPrim("interop-roundtrip", NEW(SchemeProcedure.T,
                                           apply := InteropRoundtripApply),
                  2, 2);
    RETURN prims
  END Extend;

PROCEDURE ConversionModeApply(<*UNUSED*>p : SchemeProcedure.T; 
                              <*UNUSED*>interp : Scheme.T; 
                              args : Object) : Object RAISES { E } =
  VAR
    s := Scheme.SymbolCheck(First(args));
  BEGIN
    FOR i := FIRST(Name) TO LAST(Name) DO
      IF s = SchemeSymbol.FromText(Name[i]) THEN
        CASE ProcMode[i] OF
          Mode.Reference => RETURN SchemeSymbol.FromText("Reference")
        |
          Mode.Concrete =>  RETURN SchemeSymbol.FromText("Concrete")
        END
      END
    END;

    RAISE Scheme.E("Cant find type to convert : " & Stringify(s))
  END ConversionModeApply;

BEGIN END SchemeModula3Types.
