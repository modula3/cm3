GENERIC INTERFACE PolynomialFmtLex(RF,P);
(*Copyright (c) 1996, m3na project

Abstract: Direct access to Polynomial functions

2/3/96   Harry George    Initial version
2/17/96  Harry George    Convert from OO to ADT
*)

IMPORT Wr,Thread;
FROM FmtLexSupport IMPORT Precedence;
(*==========================*)
TYPE
  T = P.T;
  FmtStyle = RECORD elemStyle := RF.FmtStyle{}; END;

  TexFlag    = {powerSum     (*present as coefficient vector or
                               as sum of powers of a given variable?*)
               ,simplePower  (*for powerSum: z and 1 instead of z^1 and z^0*)
               ,omitZero     (*for powerSum: omit zero terms*)
               ,reverse      (*high powers first*)
               };
  TexFlagSet = SET OF TexFlag;
  TexStyle = RECORD
               flags            := TexFlagSet{};
               var       : TEXT := "x";
               elemStyle        := RF.TexStyle{};
             END;

(*PROCEDURE Lex(str:TEXT):T;*)
PROCEDURE Fmt (x : T; READONLY style := FmtStyle{}) : TEXT RAISES {Thread.Alerted, Wr.Failure};
PROCEDURE Tex (x : T; READONLY style := TexStyle{}; within := Precedence.sum) : TEXT RAISES {Thread.Alerted, Wr.Failure};

(*==========================*)
END PolynomialFmtLex.
