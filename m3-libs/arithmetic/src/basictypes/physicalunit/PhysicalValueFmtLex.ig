GENERIC INTERFACE PhysicalValueFmtLex(RF,PV,DB);

(*==========================*)
TYPE
  T = PV.T;
  FmtStyle = RECORD unitDataBase : DB.T; elemStyle := RF.FmtStyle{}; END;

(*
PROCEDURE Lex(str:TEXT):T RAISES {Error};
*)

PROCEDURE Fmt (READONLY x : T; READONLY style : FmtStyle) : TEXT;

(*==========================*)
END PhysicalValueFmtLex.
