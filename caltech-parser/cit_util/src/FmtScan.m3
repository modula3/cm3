MODULE FmtScan;
IMPORT FloatMode;
IMPORT Fmt;
IMPORT Lex;
IMPORT Scan;

(* ints *)

PROCEDURE Int(base: INTEGER) : IntT =
  BEGIN
    RETURN NEW(IntPrivate, base:=base);
  END Int;

TYPE
  IntPrivate = IntT OBJECT
    base: CARDINAL;
  OVERRIDES
    fmt := FmtInt;
    scan := ScanInt;
  END;

PROCEDURE FmtInt(self: IntPrivate; i: INTEGER): TEXT =
  BEGIN
    RETURN Fmt.Int(i, self.base);
  END FmtInt;

PROCEDURE ScanInt(self: IntPrivate; t: TEXT): INTEGER RAISES {Error} =
  BEGIN
    TRY
      RETURN Scan.Int(t, self.base);
    EXCEPT Lex.Error, FloatMode.Trap =>
      RAISE Error;
    END;
  END ScanInt;


(* bools *)

PROCEDURE Bool() : BoolT =
  BEGIN
    RETURN NEW(BoolT, fmt := FmtBool, scan := ScanBool);
  END Bool;

PROCEDURE FmtBool(<*UNUSED*>self: BoolT; i: BOOLEAN): TEXT =
  BEGIN
    RETURN Fmt.Bool(i);
  END FmtBool;

PROCEDURE ScanBool(<*UNUSED*>self: BoolT; t: TEXT): BOOLEAN
  RAISES {Error} =
  BEGIN
    TRY
      RETURN Scan.Bool(t);
    EXCEPT Lex.Error =>
      RAISE Error;
    END;
  END ScanBool;


(* longreals *)

PROCEDURE LongReal(digits: CARDINAL; style: Fmt.Style) : LongRealT =
  BEGIN
    RETURN NEW(LongRealPrivate, digits:=digits, style:=style);
  END LongReal;

TYPE
  LongRealPrivate = LongRealT OBJECT
    digits: CARDINAL;
    style: Fmt.Style;
  OVERRIDES
    fmt := FmtLongReal;
    scan := ScanLongReal;
  END;

PROCEDURE FmtLongReal(self: LongRealPrivate; i: LONGREAL): TEXT =
  BEGIN
    RETURN Fmt.LongReal(i, self.style, self.digits);
  END FmtLongReal;

PROCEDURE ScanLongReal(<*UNUSED*>self: LongRealPrivate; t: TEXT): LONGREAL
  RAISES {Error} =
  BEGIN
    TRY
      RETURN Scan.LongReal(t);
    EXCEPT Lex.Error, FloatMode.Trap =>
      RAISE Error;
    END;
  END ScanLongReal;


BEGIN
END FmtScan.
