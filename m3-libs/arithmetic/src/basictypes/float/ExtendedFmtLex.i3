INTERFACE ExtendedFmtLex;
(*Copyright (c) 1996, m3na project

Abstract: Conform interface for formatting and parsing of EXTENDED numbers

*)

IMPORT Extended AS R, Fmt AS F, Lex AS L;

TYPE
  T = EXTENDED;
  FmtStyle = RECORD
               style   := F.Style.Auto;
               prec    :  CARDINAL := R.MaxSignifDigits - 1;
               literal := FALSE;
             END;

CONST
  Lex = L.Extended;

<*INLINE*>
PROCEDURE Fmt (x : T; READONLY style := FmtStyle{}) : TEXT;

END ExtendedFmtLex.
