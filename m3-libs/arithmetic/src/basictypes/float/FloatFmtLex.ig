GENERIC INTERFACE FloatFmtLex(R);
(*Arithmetic for Modula-3, see doc for details*)

(* Abstract: Generic interface for formatting and parsing of float
   numbers *)

IMPORT Rd, Thread;
IMPORT Fmt AS F;
IMPORT Lex AS L;
IMPORT FloatMode;
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


TYPE LexStyle = RECORD END;

PROCEDURE Lex (rd: Rd.T; READONLY style := LexStyle{}; ): T
  RAISES {L.Error, FloatMode.Trap, Rd.Failure, Thread.Alerted};

END FloatFmtLex.
