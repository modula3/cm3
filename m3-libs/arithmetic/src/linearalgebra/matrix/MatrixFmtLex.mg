GENERIC MODULE MatrixFmtLex(Rf);
(*
Abstract:

6/6/87    hgeorge
          Initial version.

2/11/89   hgeorge
          To work with generic matrices.

11/20/94  Harry George
          Converted to Modula3 dynamic arrays.

12/18/95  Harry George
          ...and back to fully instantiated for REAL32.

1/27/96   Harry George
          Converted to OO format and R.T          

2/17/96   Harry George   ...and back to ADT format
*)

IMPORT Wr,TextWr,Fmt AS F,Thread;

<*UNUSED*>
CONST Module = "MatrixFmtLex.";

(*-----------------*)
(*
PROCEDURE Lex( 
               str:TEXT):T RAISES {Error}=
BEGIN
  RAISE Error(Err.not_implemented);
END Lex;
*)
(*-----------------*)
PROCEDURE Fmt( 
            mat:T; 
            style:=F.Style.Fix;
            prec:=2):TEXT RAISES {Thread.Alerted, Wr.Failure}= 
CONST width = 12;
VAR
  m:=NUMBER(mat^);    m1:=0; mm:=m-1;
  n:=NUMBER(mat[0]);  n1:=0; nn:=n-1;
  wr:=TextWr.New();
BEGIN
  Wr.PutText(wr,"M" & F.Int(m) & "x" & F.Int(n) & "{\n");
  FOR i:=m1 TO mm DO
    Wr.PutText(wr,"V" & F.Int(n) & "{");
    FOR j:= n1 TO nn DO
      Wr.PutText(wr,F.Pad(Rf.Fmt(mat[i,j],style,prec),width));
      IF j#nn THEN Wr.PutText(wr,", "); END;
    END;
    Wr.PutText(wr,"}");
    IF i#mm THEN Wr.PutText(wr,","); END;
    Wr.PutText(wr,"\n");
  END;
  Wr.PutText(wr,"}\n");
  RETURN TextWr.ToText(wr);
END Fmt;

(*-----------------*)
BEGIN
END MatrixFmtLex.
