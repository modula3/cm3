INTERFACE RealFmtLex;
(*Copyright (c) 1996, m3na project

Abstract: Conform interface for formatting and parsing of REAL numbers

*)

IMPORT Real AS R, Fmt AS F, Lex AS L;

TYPE
  T = REAL;
  FmtStyle = RECORD
               style   := F.Style.Auto;
               prec    :  CARDINAL := R.MaxSignifDigits - 1;
               literal := FALSE;
             END;

CONST
  Lex = L.Real;

<*INLINE*>
PROCEDURE Fmt (x : T; READONLY style := FmtStyle{}) : TEXT;

END RealFmtLex.
