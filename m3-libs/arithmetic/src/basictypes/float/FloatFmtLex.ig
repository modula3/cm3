GENERIC INTERFACE FloatFmtLex(R);
(*Copyright (c) 1996, m3na project*)

(* Abstract: Generic interface for formatting and parsing of float
   numbers *)

IMPORT Fmt AS F;
(*IMPORT Lex AS L;*)
FROM FmtLexSupport IMPORT Precedence;

TYPE T = R.T;

TYPE
  FmtStyle = RECORD
               style             := F.Style.Auto;
               prec   : CARDINAL := R.MaxSignifDigits - 3;
               literal           := FALSE;
             END;

<*INLINE*>
PROCEDURE Fmt (x: T; READONLY style := FmtStyle{}): TEXT;

TYPE
  TexStyle = RECORD
               style           := F.Style.Auto;
               prec : CARDINAL := R.MaxSignifDigits - 3;
             END;

PROCEDURE Tex (x: T; READONLY style := TexStyle{}; within := Precedence.sum):
  TEXT;

END FloatFmtLex.
