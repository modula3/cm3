MODULE TestTex EXPORTS Test;
(*Copyright (c) 1996, m3na project
Abstract:  Tests for Tex module.

1/1/96    <name>   Initial version

*)

IMPORT FileWr, Wr, Process;
IMPORT Thread, OSError;
IMPORT TempFiles AS Tmp;

IMPORT LongRealBasic         AS R,
       LongRealComplexFast   AS C,
       LongRealPolarBasic    AS Polar,
       LongRealVectorFast    AS V,
       LongRealMatrixFast    AS M,
       LongRealFmtLex        AS RF,
       LongRealComplexFmtLex AS CF,
       LongRealPolarFmtLex   AS PolarF,
       LongRealVectorFmtLex  AS VF,
       LongRealMatrixFmtLex  AS MF;


(*=======================*)
CONST
  Module = "TestTex.";
(*----------------------*)
PROCEDURE TestTexVector():BOOLEAN=
CONST
  ftn = Module & "TestTexVector";
  filename = "test";
  y  = ARRAY OF R.T{1.3D0,-0.4D0,-0.2D0,3.6D0,-2.3D0};
  yc = ARRAY OF C.T{
         C.T{0.0D20,0.0D0},
         C.T{1.0D20,0.0D0},
         C.T{3.0D23,-4.0D23},
         C.T{-1.0D23,-1.0D23},
         C.T{0.0D0,2.5D0}
       };
VAR
  result:=TRUE;
  out := FileWr.Open(filename & ".tex");
  x := V.FromArray(ARRAY OF R.T{1.0D0,1.5D0,-0.3D0,0.7D0,-2.3D0});
  A := M.New(NUMBER(x^),NUMBER(x^));

<*FATAL OSError.E, Thread.Alerted, Wr.Failure *>
BEGIN
  Debug(1,ftn,"begin\n");

  Tmp.Note(filename&".tex");

  FOR i:=0 TO LAST(y) DO
    FOR j:=0 TO LAST(y) DO
      A[i,j] := y[ABS(i-j)];
    END;
  END;

  Wr.PutText(out,"\\documentclass[a4paper]{article}\n");
  Wr.PutText(out,"\\begin{document}\n");
  Wr.PutText(out,"$$\n");
  Wr.PutText(out,RF.Tex(V.Inner(x,M.MulV(A,x))) & "\n");
  Wr.PutText(out,"=" & VF.Tex(x,style:=VF.TexStyle{dir:=VF.TexDirection.horizontal}));
  Wr.PutText(out,"\\cdot" & MF.Tex(A));
  Wr.PutText(out,"\\cdot" & VF.Tex(x,style:=VF.TexStyle{dir:=VF.TexDirection.vertical}));
  Wr.PutText(out,"$$\n");
  Wr.PutText(out,"\\begin{eqnarray*}\n");
  FOR j:=0 TO LAST(yc) DO
    Wr.PutText(out,CF.Tex(yc[j])&"&=&");
    Wr.PutText(out,PolarF.Tex(Polar.FromComplex(yc[j]))&"\\\\\n");
  END;
  Wr.PutText(out,"\\end{eqnarray*}\n");
  Wr.PutText(out,"\\end{document}\n");

  Wr.Close(out);

  Tmp.Note(filename&".log");
  Tmp.Note(filename&".aux");
  Tmp.Note(filename&".dvi");
  EVAL Process.Wait(Process.Create ("latex", ARRAY OF TEXT{filename}));
  EVAL Process.Wait(Process.Create ("xdvi",  ARRAY OF TEXT{filename}));

  RETURN result;
END TestTexVector;
(*-------------------------*)
PROCEDURE TestTex():BOOLEAN=
CONST ftn = Module & "TestTex";
VAR result:=TRUE;
BEGIN
  NewLine(); EVAL TestTexVector();
  RETURN result;
END TestTex;
(*=======================*)
BEGIN
END TestTex.
