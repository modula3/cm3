GENERIC INTERFACE FloatFmtLex(R);
(*Copyright (c) 1996, m3na project

Abstract: Generic interface for formatting and parsing of float numbers

*)

IMPORT Fmt AS F;
(*IMPORT Lex AS L;*)
FROM FmtLexSupport IMPORT Precedence;

TYPE
  T = R.T;
  FmtStyle = RECORD
               style   := F.Style.Auto;
               prec    :  CARDINAL := R.MaxSignifDigits - 3;
               literal := FALSE;
             END;
  TexStyle = RECORD
               style   := F.Style.Auto;
               prec    :  CARDINAL := R.MaxSignifDigits - 3;
             END;

(*
CONST
  Lex = L.LongReal;
*)

<*INLINE*>
PROCEDURE Fmt (x : T; READONLY style := FmtStyle{}) : TEXT;

PROCEDURE Tex (x : T; READONLY style := TexStyle{}; within := Precedence.sum) : TEXT;

END FloatFmtLex.
