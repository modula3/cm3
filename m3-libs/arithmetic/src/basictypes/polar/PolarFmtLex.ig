GENERIC INTERFACE PolarFmtLex(P);
(*Copyright (c) 1996, m3na project
  
Abstract: Complex numbers in polar coordinates

1/1/96  Harry George    Initial version
2/17/96 Harry George    Converted from Objects to ADT's
3/23/96 Harry George    Incorporated Warren Smith's implementations
*)

FROM xUtils IMPORT Error;
IMPORT Fmt AS F;

PROCEDURE Fmt(VALUE c:P.T;
        style:F.Style:=F.Style.Fix;
        prec:CARDINAL:=3):TEXT;
        (*as "POLAR{radius:=<r>; angle:=<r>}"*)
        
(*==========================*)
END PolarFmtLex.
