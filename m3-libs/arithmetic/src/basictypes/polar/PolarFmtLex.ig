GENERIC INTERFACE PolarFmtLex(P,RF);
(*Copyright (c) 1996, m3na project
  
Abstract: Complex numbers in polar coordinates

1/1/96  Harry George    Initial version
2/17/96 Harry George    Converted from Objects to ADT's
3/23/96 Harry George    Incorporated Warren Smith's implementations
*)

FROM xUtils IMPORT Error;

(*==========================*)
TYPE
  T = P.T;
  FmtStyle = RECORD elemStyle := RF.FmtStyle{}; END;

PROCEDURE Fmt (READONLY x : T; READONLY style := FmtStyle{}) : TEXT;
        (*as "POLAR{radius:=<r>; angle:=<r>}"*)
        
(*==========================*)
END PolarFmtLex.
