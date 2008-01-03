(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: WYSIWYGify.m3,v 1.2 2001-09-19 15:30:31 wagner Exp $ *)

MODULE WYSIWYGify;
IMPORT Fmt;
IMPORT Scan;
IMPORT FloatMode;
IMPORT Lex;

<* FATAL FloatMode.Trap, Lex.Error *>

CONST
  MakePSBiggerByFactor = 1.3;
  (* determined by repeated eyeballing *)

PROCEDURE FormatTextSizeForPS(a: REAL): TEXT =
  VAR
    result := a * MakePSBiggerByFactor;
  BEGIN
    IF result > 50.0 THEN
      RETURN Fmt.Int(ROUND(result));
    ELSE
      RETURN Fmt.Real(result, prec := 2);
    END;
  END FormatTextSizeForPS;

PROCEDURE ScanTextSizeFromPS(t: TEXT): REAL =
  BEGIN
    RETURN Scan.Real(t) / MakePSBiggerByFactor;
  END ScanTextSizeFromPS;

BEGIN
END WYSIWYGify. 
