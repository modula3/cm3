INTERFACE ExtendedFmtLex;
(*Copyright (c) 1996, m3na project

Abstract: Conform interface for formatting and parsing of EXTENDED numbers

*)

IMPORT Fmt AS F, Lex AS L;

CONST
  Fmt = F.Extended;
  Lex = L.Extended;

END ExtendedFmtLex.
