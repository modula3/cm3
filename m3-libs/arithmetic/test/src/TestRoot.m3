MODULE TestRoot EXPORTS Test;
(*Copyright (c) 1996, Harry George
Abstract: Test driver for Modula-3 rendition of
          Numerical Recipes in C, 1992.

1/2/96    Harry George   Initial version
1/28/96   Harry George   converted to m3na
2/17/96   Harry George   Converted to ADT format
*)
FROM xUtils IMPORT Error,Err;
IMPORT IO,Wr,Fmt,xReal64 AS R,xComplex AS C;
FROM xReal64 IMPORT REAL64,Array,Ftn;
IMPORT xRoot;
(*=======================*)
CONST
  Module = "TestRoot.";
(*----------------------*)
PROCEDURE TestABC():BOOLEAN=
CONST
  ftn = Module & "TestABC";
VAR
  result:=TRUE;
BEGIN
  Debug(1,ftn,"begin\n");

  RETURN result;   
END TestABC;
(*---------------------*)
VAR (*globally visible*)
  r1:=-10.0D0; r2:=2.0D0; r3:=10.0D0;
  
(*---------------------*)
PROCEDURE myfun(x:REAL64):REAL64=
BEGIN
  RETURN (x-r1)*(x-r2)*(x-r3);
END myfun;
(*---------------------*)
PROCEDURE myfun2(x:REAL64; VAR f,df:REAL64)=
BEGIN
  f:=(x-r1)*(x-r2)*(x-r3);
  df:=(x-r2)*(x-r3)+(x-r1)*(x-r3)+(x-r1)*(x-r2);
END myfun2;

(*=========================*)
(* Quadratics              *)
(*=========================*)
(*-----------------------*)
PROCEDURE TestQuadreal():BOOLEAN=
CONST
  ftn = Module & "TestQuadreal";
VAR
  result:=TRUE;
  alpha,beta,x1,x2:C.COMPLEX;
  a,b,c:REAL64;
BEGIN
  Debug(1,ftn,"begin\n");
  a:=1.0D0; b:=2.0D0; c:=-3.0D0;
  Msg("Solve a*x^2+b*x+c=0 for"
    & "\na=" & R.fmt(a)
    & "\nb=" & R.fmt(b)
    & "\nc=" & R.fmt(c)
    & "\n");
    
  xRoot.quadreal(a,b,c,alpha,beta,x1,x2);
  
  Msg("alpha=" & C.fmt(alpha)
   & " beta=" & C.fmt(beta)
   & "\n");
  Msg("x1=" & C.fmt(x1)
   & " x2=" & C.fmt(x2)
   & "\n");

  RETURN result;
END TestQuadreal;
(*-----------------------*)
PROCEDURE TestQuadcmplx():BOOLEAN=
CONST
  ftn = Module & "TestCmplx";
VAR
  result:=TRUE;
  alpha,beta,x1,x2:C.COMPLEX;
  a,b,c:C.COMPLEX;
BEGIN
  Debug(1,ftn,"begin\n");
  a:=C.COMPLEX{re:=1.0D0,im:=1.0D0};
  b:=C.COMPLEX{re:=2.0D0,im:=2.0D0};
  c:=C.COMPLEX{re:=3.0D0,im:=3.0D0};
  Msg("Solve a*x^2+b*x+c=0 for"
    & "\na=" & C.fmt(a)
    & "\nb=" & C.fmt(b)
    & "\nc=" & C.fmt(c)
    & "\n");
    
  xRoot.quadcmpx(a,b,c,alpha,beta,x1,x2);
  
  Msg("alpha=" & C.fmt(alpha)
   & " beta=" & C.fmt(beta)
   & "\n");
  Msg("x1=" & C.fmt(x1)
   & " x2=" & C.fmt(x2)
   & "\n");
      
  RETURN result;
END TestQuadcmplx;

(*=========================*)
(* NonLinears              *)
(*=========================*)
(*----------------------*)
PROCEDURE TestBracket_out():BOOLEAN=
CONST
  ftn = Module & "TestBracket_out";
  maxiter=10;
VAR
  result:=TRUE;
  x1,x2:REAL64;  
BEGIN
  Debug(1,ftn,"begin\n");
  Msg("true roots: r1=" & R.fmt(r1)
               & " r2=" & R.fmt(r2)
               & " r3=" & R.fmt(r3) 
               & " maxiter=" & Fmt.Int(maxiter)
               & "\n");
  FOR i:=1 TO 50 DO
    x1:=5.0D0*FLOAT(i,REAL64)-50.0D0; x2:=x1+1.0D0;
    Msg("start at x1=" & R.fmt(x1,style:=Fmt.Style.Fix,prec:=3) 
              & " x2=" & R.fmt(x2,style:=Fmt.Style.Fix,prec:=3));
    TRY
      IF xRoot.bracket_out(myfun,x1,x2,maxiter:=maxiter) THEN
         Msg(" end at x1=" & R.fmt(x1,style:=Fmt.Style.Fix,prec:=3) 
                  & " x2=" & R.fmt(x2,style:=Fmt.Style.Fix,prec:=3) 
                  & "\n");
      ELSE Msg(" not found\n");
      END;
    EXCEPT
    | Error(code) => 
    END;
  END;  
  RETURN result;   
END TestBracket_out;
(*----------------------*)
PROCEDURE TestBracket_in():BOOLEAN=
CONST
  ftn = Module & "TestBracket_in";
VAR
  result:=TRUE;
  x1,x2:REAL64;
  nb:CARDINAL:=5;
  xb1:=NEW(Array,nb);
  xb2:=NEW(Array,nb);
  n,nbtmp:CARDINAL;  
BEGIN
  Debug(1,ftn,"begin\n");
  Msg("true roots: r1=" & R.fmt(r1)
               & " r2=" & R.fmt(r2)
               & " r3=" & R.fmt(r3) 
               & "\n");
  x1:=-50.0D0; x2:=+50.0D0;
  Msg("start at x1=" & R.fmt(x1,style:=Fmt.Style.Fix,prec:=3) 
            & " x2=" & R.fmt(x2,style:=Fmt.Style.Fix,prec:=3)
            & " nb=" & Fmt.Int(nb)
            & "\n");
  FOR i:=10 TO 100 BY 10 DO
    n:=i; Msg("n=" & Fmt.Int(n) & "\n");
    nbtmp:=nb; (*so we don't overwrite nb*)
    TRY
      IF xRoot.bracket_in(func:=myfun,x1:=x1,x2:=x2,n:=n,
      xb1:=xb1,xb2:=xb2,nb:=nbtmp) THEN
         FOR j:=0 TO nbtmp-1 DO
         Msg(" found  x1=" & R.fmt(xb1[j],style:=Fmt.Style.Fix,prec:=3) 
                  & " x2=" & R.fmt(xb2[j],style:=Fmt.Style.Fix,prec:=3) 
                  & "\n");
         END;
      ELSE Msg(" not found\n");
      END;
    EXCEPT
    | Error(code) => 
    END;
  END;  
  RETURN result;   
END TestBracket_in;
(*----------------------*)
PROCEDURE TestBisect():BOOLEAN=
CONST
  ftn = Module & "TestBisect";
VAR
  result:=TRUE;
  x1,x2,tol,root:REAL64;
BEGIN
  Debug(1,ftn,"begin\n");
  Msg("true roots: r1=" & R.fmt(r1)
               & " r2=" & R.fmt(r2)
               & " r3=" & R.fmt(r3) 
               & "\n");
  x1:=-1.0D0; x2:=2.9D0; tol:=0.001D0;
  Msg("start at x1=" & R.fmt(x1,style:=Fmt.Style.Fix,prec:=3) 
            & " x2=" & R.fmt(x2,style:=Fmt.Style.Fix,prec:=3)
            & " tol=" & R.fmt(tol));
  root:=xRoot.bisect(myfun,x1,x2,tol);
  Msg(" found  root=" & R.fmt(root,style:=Fmt.Style.Fix,prec:=3) 
    & "\n");
  RETURN result;   
END TestBisect;
(*----------------------*)
PROCEDURE TestBrent():BOOLEAN=
CONST
  ftn = Module & "TestBrent";
VAR
  result:=TRUE;
  x1,x2,tol,root:REAL64;
BEGIN
  Debug(1,ftn,"begin\n");
  Msg("true roots: r1=" & R.fmt(r1)
               & " r2=" & R.fmt(r2)
               & " r3=" & R.fmt(r3) 
               & "\n");
  x1:=-12.0D0; x2:=1.0D0; tol:=0.001D0;
  Msg("start at x1=" & R.fmt(x1,style:=Fmt.Style.Fix,prec:=3) 
            & " x2=" & R.fmt(x2,style:=Fmt.Style.Fix,prec:=3)
            & " tol=" & R.fmt(tol));
  TRY
    root:=xRoot.brent(myfun,x1,x2,tol:=tol);
  EXCEPT
  | Error(code) =>
  ELSE
    Msg("other error\n");
  END;
  Msg(" found  root=" & R.fmt(root,style:=Fmt.Style.Fix,prec:=3) 
    & "\n");
  RETURN result;   
END TestBrent;
(*----------------------*)
PROCEDURE TestNewtraph():BOOLEAN=
CONST
  ftn = Module & "TestNewtraph";
VAR
  result:=TRUE;
  x1,x2,tol,root:REAL64;
  maxiter:CARDINAL;
BEGIN
  Debug(1,ftn,"begin\n");
  Msg("true roots: r1=" & R.fmt(r1)
               & " r2=" & R.fmt(r2)
               & " r3=" & R.fmt(r3) 
               & "\n");
  x1:=6.0D0; x2:=5.0D0; tol:=0.001D0; maxiter:=15;
FOR i:=0 TO 10 DO
  x2:=x2+1.1D0; 
  Msg("start at x1=" & R.fmt(x1,style:=Fmt.Style.Fix,prec:=3) 
            & " x2=" & R.fmt(x2,style:=Fmt.Style.Fix,prec:=3)
            & " tol=" & R.fmt(tol)
            & " maxiter=" & Fmt.Int(maxiter));
  TRY
    root:=xRoot.newtraph(myfun2,x1,x2,tol,maxiter);
    Msg(" found  root=" & R.fmt(root,style:=Fmt.Style.Fix,prec:=5) 
      & "\n");
  EXCEPT
  | Error(code) => CASE code OF
                      | Err.not_bracketed=>Msg(" not bracketed\n");
                      | Err.out_of_range=>Msg(" jumped out\n");
                      | Err.not_converging=>Msg(" not converging\n");
                      END;
  ELSE
    Msg(" other error\n");
  END;
END;
  RETURN result;   
END TestNewtraph;



(****************************
(*----------------------*)

PROCEDURE TestLaguer():BOOLEAN=
CONST
  ftn = Module & "TestLaguer";
  n=4; m=n-1;
VAR
  result:=TRUE;
  p:=NEW(na.cVector,n);
  x:C.COMPLEX;
BEGIN
  Debug(1,ftn,"begin\n");
  Msg("true roots: r1=" & R.fmt(r1)
               & " r2=" & R.fmt(r2)
               & " r3=" & R.fmt(r3) 
               & "\n");
  p^:=ARRAY [0..m] OF C.COMPLEX
      {C.COMPLEX(200.0,0.0), C.COMPLEX(-100.0,0.0),
       C.COMPLEX(-2.0,0.0),  C.COMPLEX(1.0,0.0)};
FOR i:=0 TO 10 DO
  x:=C.COMPLEX(FLOAT(i,REAL64),0.5);
  Msg("start at x=" & C.fmt(x,style:=Fmt.Style.Fix,prec:=3)); 
  TRY
    na.laguer(p,m,x);
    Msg(" found x=" & C.fmt(x,style:=Fmt.Style.Fix,prec:=5) 
      & "\n");
  EXCEPT
  | Error(code) => CASE code OF
                      | Err.divide_by_zero=>Msg(" divide by zero\n");
                      | Err.out_of_range=>Msg(" jumped out\n");
                      | Err.not_converging=>Msg(" not converging\n");
                      END;
  ELSE
    Msg(" other error\n");
  END;
END;
  RETURN result;   
END TestLaguer;
(*----------------------*)
PROCEDURE TestZRoots():BOOLEAN=
CONST
  ftn = Module & "TestZRoots";
  n=4; m=n-1;
VAR
  result:=TRUE;
  p:=NEW(na.cVector,n);
  roots:na.cVector;
BEGIN
  Debug(1,ftn,"begin\n");
  Msg("true roots: r1=" & R.fmt(r1)
               & " r2=" & R.fmt(r2)
               & " r3=" & R.fmt(r3) 
               & "\n");
  p^:=ARRAY [0..m] OF C.COMPLEX
      {C.COMPLEX(200.0,0.0), C.COMPLEX(-100.0,0.0),
       C.COMPLEX(-2.0,0.0),  C.COMPLEX(1.0,0.0)};
  TRY
    na.zroots(p,roots,polish:=FALSE);
    Msg("\n     raw roots:");
    FOR i:=0 TO m-1 DO
      Msg(" root[" & Fmt.Int(i) & "]="
        & na.Ctext(roots[i],prec:=4));
    END;
    na.zroots(p,roots,polish:=TRUE);
    Msg("\npolished roots:");
    FOR i:=0 TO m-1 DO
      Msg(" root[" & Fmt.Int(i) & "]="
        & na.Ctext(roots[i],prec:=4));
    END;
  EXCEPT
  | Error(code) => CASE code OF
                      | Err.divide_by_zero=>Msg(" divide by zero\n");
                      | Err.out_of_range=>Msg(" jumped out\n");
                      | Err.not_converging=>Msg(" not converging\n");
                      END;
  ELSE
    Msg(" other error\n");
  END;

  RETURN result;   
END TestZRoots;
******************************)
(*-------------------------*)
PROCEDURE TestRoot():BOOLEAN=
CONST ftn = Module & "TestCh09_root";
VAR result:=TRUE;
BEGIN
  NewLine(); EVAL TestQuadreal();
  NewLine(); EVAL TestQuadcmplx();
  (*NewLine(); EVAL TestBracket_out();*)
  (*NewLine(); EVAL TestBracket_in();*)
  (*NewLine(); EVAL TestBisect();*)
  (*NewLine(); EVAL TestBrent();*)
  (*NewLine(); EVAL TestNewtraph();*)

  RETURN result;
END TestRoot;
(*=======================*)
BEGIN
END TestRoot.
