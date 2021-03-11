MODULE FmtScanVar;
IMPORT FloatMode;
IMPORT Fmt;
IMPORT Lex;
IMPORT Scan;




(*****************************************************************************
 *                                                                           *
 *                                   INTEGER                                 *
 *                                                                           *
 *****************************************************************************)

PROCEDURE Int(var: REF INTEGER; base, lo, hi: INTEGER) : T =
  BEGIN
    RETURN NEW(IntPrivate, var:=var, base:=base, lo:=lo, hi:=hi);
  END Int;

TYPE
  IntPrivate = T OBJECT
    var: REF INTEGER;
    base: CARDINAL;
    lo,hi: INTEGER;
  OVERRIDES
    fmt := FmtInt;
    scan := ScanInt;
  END;

PROCEDURE FmtInt(self: IntPrivate): TEXT =
  BEGIN
    RETURN Fmt.Int(self.var^, self.base);
  END FmtInt;

PROCEDURE ScanInt(self: IntPrivate; t: TEXT) RAISES {Error} =
  VAR
    res: INTEGER;
  BEGIN
    TRY
      res := Scan.Int(t, self.base);
    EXCEPT Lex.Error, FloatMode.Trap =>
      RAISE Error("integer expected.");
    END;
    IF (res < self.lo) OR (res > self.hi) THEN
      RAISE Error("expected value in range [" & Fmt.Int(self.lo) & "," &
            Fmt.Int(self.hi) & "]");
    END;
    self.var^ := res;
  END ScanInt;




(*****************************************************************************
 *                                                                           *
 *                                   BOOLEAN                                 *
 *                                                                           *
 *****************************************************************************)


PROCEDURE Bool(var: REF BOOLEAN) : T =
  BEGIN
    RETURN NEW(BoolPrivate, var:=var);
  END Bool;

TYPE
  BoolPrivate = T OBJECT
    var: REF BOOLEAN;
  OVERRIDES
    fmt := FmtBool;
    scan := ScanBool;
  END;

PROCEDURE FmtBool(self: BoolPrivate): TEXT =
  BEGIN
    RETURN Fmt.Bool(self.var^);
  END FmtBool;

PROCEDURE ScanBool(self: BoolPrivate; t: TEXT) RAISES {Error} =
  BEGIN
    TRY
      self.var^ := Scan.Bool(t);
    EXCEPT Lex.Error =>
      RAISE Error("boolean expected.");
    END;
  END ScanBool;




(*****************************************************************************
 *                                                                           *
 *                                  LONGREAL                                 *
 *                                                                           *
 *****************************************************************************)


PROCEDURE LongReal(var: REF LONGREAL; digits: CARDINAL; style: Fmt.Style) : T =
  BEGIN
    RETURN NEW(LongRealPrivate, var:=var, digits:=digits, style:=style);
  END LongReal;

TYPE
  LongRealPrivate = T OBJECT
    var: REF LONGREAL;
    digits: CARDINAL;
    style: Fmt.Style;
  OVERRIDES
    fmt := FmtLongReal;
    scan := ScanLongReal;
  END;

PROCEDURE FmtLongReal(self: LongRealPrivate): TEXT =
  BEGIN
    RETURN Fmt.LongReal(self.var^, self.style, self.digits);
  END FmtLongReal;

PROCEDURE ScanLongReal(self: LongRealPrivate; t: TEXT)
  RAISES {Error} =
  BEGIN
    TRY
      self.var^ := Scan.LongReal(t);
    EXCEPT Lex.Error, FloatMode.Trap =>
      RAISE Error("number expected.");
    END;
  END ScanLongReal;


BEGIN
END FmtScanVar.
