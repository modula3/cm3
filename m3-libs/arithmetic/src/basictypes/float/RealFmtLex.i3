INTERFACE RealFmtLex;
(*Copyright (c) 1996, m3na project

Abstract: Conform interface for formatting and parsing of REAL numbers

*)

IMPORT Fmt AS F, Lex AS L;

TYPE
  T = REAL;

CONST
  Lex = L.Real;
  Fmt = F.Real;

END RealFmtLex.
