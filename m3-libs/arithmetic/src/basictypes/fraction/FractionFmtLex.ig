GENERIC INTERFACE FractionFmtLex(Fr,RF);
(*Copyright (c) 1996, m3na project
  
Abstract: Formatting and parsing fraction numbers

1/1/96  Harry George    Initial version
2/17/96 Harry George    Converted from Objects to ADT's
3/23/96 Harry George    Incorporated Warren Smith's implementations
*)

FROM xUtils IMPORT Error;

(*==========================*)
TYPE
  T = Fr.T;
  Base = [2..16];
  FmtStyle = RECORD elemStyle := RF.FmtStyle{}; END;

PROCEDURE Lex(str:TEXT):Fr.T RAISES {Error};
        (*reads after the "FRACTION{" in FRACTION{re:=<r>; im:=<r>},
        thru the "}"*)
PROCEDURE Fmt (READONLY x : T; READONLY style := FmtStyle{}) : TEXT;
        (*outputs as "FRACTION{re:=<r>; im:=<r>}"
        Uses simple F.Real if x.im=0.0.*)

(*==========================*)
END FractionFmtLex.
