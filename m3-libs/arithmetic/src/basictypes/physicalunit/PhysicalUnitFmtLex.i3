INTERFACE PhysicalUnitFmtLex;
(*Copyright (c) 1996, m3na project*)

(*
Abstract: Formatter for unit vectors.
          Mainly for debugging purposes.
*)

IMPORT PhysicalUnit AS U;


TYPE T = U.T;

PROCEDURE Fmt (unit: T): TEXT;

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
