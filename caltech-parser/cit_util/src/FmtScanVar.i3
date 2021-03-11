INTERFACE FmtScanVar;
IMPORT Fmt;

TYPE
  T = OBJECT METHODS
    fmt(): TEXT;
    scan(t: TEXT) RAISES {Error};
  END;

EXCEPTION
  Error(TEXT);

PROCEDURE Int(var: REF INTEGER; base, lo, hi: INTEGER): T;
PROCEDURE Bool(var: REF BOOLEAN): T;
PROCEDURE LongReal(var: REF LONGREAL; digits: CARDINAL; style: Fmt.Style): T;


END FmtScanVar.
