
(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

MODULE ObliqPrinter;
IMPORT SynWr, Obliq, ObTree, ObPrintTree, ObValue, ObPrintValue, ObLib;

  VAR setupDone := FALSE;

  PROCEDURE PackageSetup(console: SynWr.T) =
  BEGIN
    Obliq.PackageSetup(console);
    IF NOT setupDone THEN
      setupDone := TRUE;
      ObPrintTree.Setup();
      ObPrintValue.Setup();
    END;
  END PackageSetup;

  PROCEDURE PrintTerm(term: ObTree.Term; libEnv: ObLib.Env; 
                      swr: SynWr.T; depth:=10) =
  BEGIN
    ObPrintTree.PrintTerm(swr, term, libEnv, NIL, depth);
  END PrintTerm;

  PROCEDURE PrintVal(val: ObValue.Val; libEnv: ObLib.Env;
                     swr: SynWr.T; depth:=10) =
  BEGIN
    ObPrintValue.PrintVal(swr, val, libEnv, NIL, depth);
  END PrintVal;

  PROCEDURE PrintText(text: TEXT; swr: SynWr.T) =
  BEGIN
    SynWr.Text(swr, text);
  END PrintText;

  PROCEDURE PrintFlush(swr: SynWr.T) =
  BEGIN
    SynWr.Flush(swr);
  END PrintFlush;

BEGIN
END ObliqPrinter.
