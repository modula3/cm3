
(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

MODULE ObliqPrinter;
IMPORT SynWr, Obliq, ObTree, ObPrintTree, ObValue, ObPrintValue, ObLib;

  VAR setupDone := FALSE;

  PROCEDURE PackageSetup() =
  BEGIN
    Obliq.PackageSetup();
    IF NOT setupDone THEN
      setupDone := TRUE;
      ObPrintTree.Setup();
      ObPrintValue.Setup();
    END;
  END PackageSetup;

  PROCEDURE PrintTerm(term: ObTree.Term; libEnv: ObLib.Env; 
    swr: SynWr.T:=NIL; depth:=10) =
  BEGIN
    IF swr=NIL THEN swr := SynWr.out END;
    ObPrintTree.PrintTerm(swr, term, libEnv, NIL, depth);
  END PrintTerm;

  PROCEDURE PrintVal(val: ObValue.Val; libEnv: ObLib.Env;
     swr: SynWr.T:=NIL; depth:=10) =
  BEGIN
    IF swr=NIL THEN swr := SynWr.out END;
    ObPrintValue.PrintVal(swr, val, libEnv, NIL, depth);
  END PrintVal;

  PROCEDURE PrintText(text: TEXT; swr: SynWr.T:=NIL) =
  BEGIN
    IF swr=NIL THEN swr := SynWr.out END;
    SynWr.Text(swr, text);
  END PrintText;

  PROCEDURE PrintFlush(swr: SynWr.T:=NIL) =
  BEGIN
    IF swr=NIL THEN swr := SynWr.out END;
    SynWr.Flush(swr);
  END PrintFlush;

BEGIN
END ObliqPrinter.
