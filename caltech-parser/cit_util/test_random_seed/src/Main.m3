(* $Id: Main.m3,v 1.1 2003/08/05 18:14:12 mika Exp $ *)

(* Random-seed demo program *)

MODULE Main;
IMPORT CITRandom, IO, Scan; 
IMPORT Params, Fmt;

IMPORT FloatMode, Lex;

<* FATAL Lex.Error, FloatMode.Trap *>

VAR
  seed : INTEGER;
  r : CITRandom.T;
BEGIN
  IF Params.Count = 1 THEN
    (* no parameters, just make a new seed *)
    r := NEW(CITRandom.T).init(fixed := FALSE);
    IO.Put("Seed is " & Fmt.Int(r.seed()) & "\n")
  ELSE
    r := NEW(CITRandom.T).init(fixed := TRUE, seed := Scan.Int(Params.Get(1)))
  END;

  IO.Put("Value : " & Fmt.Int(r.integer()) & "\n");
  IO.Put("Value : " & Fmt.Int(r.integer()) & "\n");
  IO.Put("Value : " & Fmt.Int(r.integer()) & "\n");
  IO.Put("Value : " & Fmt.Int(r.integer()) & "\n")

END Main.
