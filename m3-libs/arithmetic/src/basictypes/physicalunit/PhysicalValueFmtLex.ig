GENERIC INTERFACE PhysicalValueFmtLex(CF,PV,DB);

(*==========================*)
TYPE
  T = PV.T;
  FmtStyle = RECORD unitDataBase : DB.T; elemStyle := CF.FmtStyle{}; END;

(*
PROCEDURE Lex(str:TEXT):T RAISES {Error};
*)

PROCEDURE Fmt (READONLY x : T; READONLY style : FmtStyle) : TEXT;

(*==========================*)
END PhysicalValueFmtLex.
