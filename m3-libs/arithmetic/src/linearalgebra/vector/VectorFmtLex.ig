GENERIC INTERFACE VectorFmtLex(RF,V);
(*Copyright (c) 1996, m3na project

Abstract: vector formatting

2/17/96  Harry George    Convert from Objects to ADT's
*)
IMPORT Wr,Thread;
(*==========================*)
TYPE
  T = V.T;
  FmtStyle = RECORD
               width     : CARDINAL := 12;
               elemStyle            := RF.FmtStyle{};
             END;

  TexDirection = {horizontal, vertical};
  TexStyle = RECORD
               dir              := TexDirection.horizontal;
               sep       : TEXT := " \\quad ";
               elemStyle        := RF.TexStyle{};
             END;

(*
PROCEDURE Lex(str:TEXT):T;
*)
PROCEDURE Fmt (x : T; READONLY style := FmtStyle{}) : TEXT RAISES {Thread.Alerted, Wr.Failure};
PROCEDURE Tex (x : T; READONLY style := TexStyle{}) : TEXT RAISES {Thread.Alerted, Wr.Failure};

(*==========================*)
END VectorFmtLex.
