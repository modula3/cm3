MODULE PhysicalUnitFmtLex;
(* Arithmetic for Modula-3, see doc for details *)

IMPORT PhysicalUnit AS U;
(*IMPORT Rd, Wr, TextWr, Thread;*)
IMPORT Fmt AS F;
(*IMPORT Lex AS L;*)
(*IMPORT FloatMode;*)

<* UNUSED *>
CONST
  Module = "PhysicalUnitFmtLex.";

PROCEDURE Fmt (unit: T; ): TEXT =
  VAR
    it             := unit.iterate();
    dim: INTEGER;
    exp: U.ExpType;
    res: TEXT      := "{";
  BEGIN
    WHILE it.next(dim, exp) DO
      res := res & "(" & F.Int(dim) & "," & F.Int(exp) & ")";
    END;
    RETURN res & "}";
  END Fmt;

(*
PROCEDURE Lex (rd: Rd.T; READONLY style: LexStyle; ): T
  RAISES {L.Error, FloatMode.Trap, Rd.Failure, Thread.Alerted} =
  BEGIN
  END Lex;
*)

BEGIN
END PhysicalUnitFmtLex.
