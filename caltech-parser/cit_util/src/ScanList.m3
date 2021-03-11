MODULE ScanList;
IMPORT FloatMode;
IMPORT IntList;
IMPORT Lex;
IMPORT LongrealList;
IMPORT Scan;
IMPORT TextList;

PROCEDURE Int(l: TextList.T; defaultBase: [2..16] := 10): IntList.T 
  RAISES {Lex.Error, FloatMode.Trap} =
  VAR
    res: IntList.T := NIL;
  BEGIN
    WHILE l # NIL DO
      res := IntList.Cons(Scan.Int(l.head, defaultBase), res);
      l := l.tail;
    END;
    RETURN IntList.ReverseD(res);
  END Int;

PROCEDURE LongReal(l: TextList.T): LongrealList.T  
  RAISES {Lex.Error, FloatMode.Trap} =
  VAR
    res: LongrealList.T := NIL;
  BEGIN
    WHILE l # NIL DO
      res := LongrealList.Cons(Scan.LongReal(l.head), res);
      l := l.tail;
    END;
    RETURN LongrealList.ReverseD(res);
  END LongReal;

BEGIN
END ScanList.
