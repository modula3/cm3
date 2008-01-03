INTERFACE PhysicalUnitFmtLex;
(* Arithmetic for Modula-3, see doc for details *)

(*
Abstract: Formatter for unit vectors.
          Mainly for debugging purposes.
*)

IMPORT PhysicalUnit AS U;


TYPE T = U.T;

PROCEDURE Fmt (unit: T; ): TEXT;

(*
TYPE
  LexStyle = RECORD
               sep       := ' ';
               elemStyle := RF.LexStyle{};
             END;

PROCEDURE Lex (rd: Rd.T; READONLY style := LexStyle{}; ): T
  RAISES {L.Error, FloatMode.Trap, Rd.Failure, Thread.Alerted};
*)
END PhysicalUnitFmtLex.
