INTERFACE ScanList;
IMPORT FloatMode;
IMPORT IntList;
IMPORT Lex;
IMPORT LongrealList;
IMPORT TextList;

PROCEDURE Int(l: TextList.T; defaultBase: [2..16] := 10): IntList.T
  RAISES {Lex.Error, FloatMode.Trap};

PROCEDURE LongReal(l: TextList.T): LongrealList.T
  RAISES {Lex.Error, FloatMode.Trap};

END ScanList.
