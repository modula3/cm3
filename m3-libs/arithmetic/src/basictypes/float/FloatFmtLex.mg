GENERIC MODULE FloatFmtLex(FI);
(*Copyright (c) 1996, m3na project

Abstract:

*)

(*----------------------*)
PROCEDURE Fmt (x : T; READONLY style := FmtStyle{}) : TEXT =
BEGIN
  RETURN FI.Fmt (x, style.style, style.prec, style.literal);
END Fmt;

BEGIN
END FloatFmtLex.
