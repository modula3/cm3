MODULE LongRealFmtLex;
(*Copyright (c) 1996, m3na project

Abstract:

*)

IMPORT Fmt AS F;

(*----------------------*)
PROCEDURE Fmt (x : T; READONLY style := FmtStyle{}) : TEXT =
BEGIN
  RETURN F.LongReal (x, style.style, style.prec, style.literal);
END Fmt;

BEGIN
END LongRealFmtLex.
