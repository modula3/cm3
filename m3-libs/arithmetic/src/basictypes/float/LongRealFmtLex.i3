INTERFACE LongRealFmtLex;
(*Copyright (c) 1996, m3na project

Abstract: Conform interface for formatting and parsing of LONGREAL numbers

*)

IMPORT LongReal AS R, Fmt AS F, Lex AS L;

TYPE
  T = LONGREAL;
  FmtStyle = RECORD
               style   := F.Style.Auto;
               prec    :  CARDINAL := R.MaxSignifDigits - 1;
               literal := FALSE;
             END;

CONST
  Lex = L.LongReal;

<*INLINE*>
PROCEDURE Fmt (x : T; READONLY style := FmtStyle{}) : TEXT;

END LongRealFmtLex.
