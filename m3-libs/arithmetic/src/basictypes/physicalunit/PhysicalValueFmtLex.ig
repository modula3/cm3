GENERIC INTERFACE PhysicalValueFmtLex(CF, PV, DB);
(*Copyright (c) 1996, m3na project*)

TYPE T = PV.T;

TYPE
  FmtStyle = RECORD
               unitDataBase: DB.T;
               elemStyle            := CF.FmtStyle{};
             END;

PROCEDURE Fmt (READONLY x: T; READONLY style: FmtStyle): TEXT;

(*
TYPE
  LexStyle = RECORD
               sep       := ' ';
               elemStyle := RF.LexStyle{};
             END;

PROCEDURE Lex (rd: Rd.T; READONLY style := LexStyle{}; ): T
  RAISES {L.Error, FloatMode.Trap, Rd.Failure, Thread.Alerted};
*)
END PhysicalValueFmtLex.
