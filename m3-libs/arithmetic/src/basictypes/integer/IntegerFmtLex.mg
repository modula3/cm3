GENERIC MODULE IntegerFmtLex(I);
(*Copyright (c) 1996, m3na project

Abstract: Integers

2/17/96  Harry George    Initial version
*)
IMPORT Fmt AS F, Wr,TextWr,Thread;


<*UNUSED*>CONST Module = "IntegerFmtLex.";
(*==========================*)
(*----------------------*)
PROCEDURE FmtArray(READONLY a:ARRAY OF I.T;
                   base      :CARDINAL:=10;
                   cellwidth :CARDINAL:=4;
                   linewidth :CARDINAL:=60):TEXT RAISES {Thread.Alerted, Wr.Failure} =
VAR
  wr:=TextWr.New();
  n:=NUMBER(a); n1:=0; nn:=n-1;
  currwidth:=0;
BEGIN
  Wr.PutText(wr,"A" & Fmt(n) & "{");
  FOR i:=n1 TO nn DO
    Wr.PutText(wr,F.Pad(Fmt(a[i],base:=base),cellwidth)); 
    IF i#nn THEN Wr.PutText(wr,", "); END;
    INC(currwidth,cellwidth+2);
    IF currwidth>linewidth THEN
      Wr.PutText(wr,"\n   ");
      currwidth:=0;
    END;
  END;
  Wr.PutText(wr,"}\n");
  RETURN TextWr.ToText(wr);  
END FmtArray;

(*==========================*)
BEGIN
END IntegerFmtLex.
