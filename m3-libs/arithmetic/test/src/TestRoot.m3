MODULE tRoot EXPORTS test;
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
  Module = "tRoot.";
(*----------------------*)
PROCEDURE test_ABC():BOOLEAN=
CONST
  ftn = Module & "test_ABC";
VAR
  result:=TRUE;
BEGIN
  debug(1,ftn,"begin\n");

  RETURN result;   
END test_ABC;
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
PROCEDURE test_quadreal():BOOLEAN=
CONST
  ftn = Module & "test_quadreal";
VAR
  result:=TRUE;
  alpha,beta,x1,x2:C.COMPLEX;
  a,b,c:REAL64;
BEGIN
  debug(1,ftn,"begin\n");
  a:=1.0D0; b:=2.0D0; c:=-3.0D0;
  msg("Solve a*x^2+b*x+c=0 for"
    & "\na=" & R.fmt(a)
    & "\nb=" & R.fmt(b)
    & "\nc=" & R.fmt(c)
    & "\n");
    
  xRoot.quadreal(a,b,c,alpha,beta,x1,x2);
  
  msg("alpha=" & C.fmt(alpha)
   & " beta=" & C.fmt(beta)
   & "\n");
  msg("x1=" & C.fmt(x1)
   & " x2=" & C.fmt(x2)
   & "\n");

  RETURN result;
END test_quadreal;
(*-----------------------*)
PROCEDURE test_quadcmplx():BOOLEAN=
CONST
  ftn = Module & "test_cmplx";
VAR
  result:=TRUE;
  alpha,beta,x1,x2:C.COMPLEX;
  a,b,c:C.COMPLEX;
BEGIN
  debug(1,ftn,"begin\n");
  a:=C.COMPLEX{re:=1.0D0,im:=1.0D0};
  b:=C.COMPLEX{re:=2.0D0,im:=2.0D0};
  c:=C.COMPLEX{re:=3.0D0,im:=3.0D0};
  msg("Solve a*x^2+b*x+c=0 for"
    & "\na=" & C.fmt(a)
    & "\nb=" & C.fmt(b)
    & "\nc=" & C.fmt(c)
    & "\n");
    
  xRoot.quadcmpx(a,b,c,alpha,beta,x1,x2);
  
  msg("alpha=" & C.fmt(alpha)
   & " beta=" & C.fmt(beta)
   & "\n");
  msg("x1=" & C.fmt(x1)
   & " x2=" & C.fmt(x2)
   & "\n");
      
  RETURN result;
END test_quadcmplx;

(*=========================*)
(* NonLinears              *)
(*=========================*)
(*----------------------*)
PROCEDURE test_bracket_out():BOOLEAN=
CONST
  ftn = Module & "test_bracket_out";
  maxiter=10;
VAR
  result:=TRUE;
  x1,x2:REAL64;  
BEGIN
  debug(1,ftn,"begin\n");
  msg("true roots: r1=" & R.fmt(r1)
               & " r2=" & R.fmt(r2)
               & " r3=" & R.fmt(r3) 
               & " maxiter=" & Fmt.Int(maxiter)
               & "\n");
  FOR i:=1 TO 50 DO
    x1:=5.0D0*FLOAT(i,REAL64)-50.0D0; x2:=x1+1.0D0;
    msg("start at x1=" & R.fmt(x1,style:=Fmt.Style.Fix,prec:=3) 
              & " x2=" & R.fmt(x2,style:=Fmt.Style.Fix,prec:=3));
    TRY
      IF xRoot.bracket_out(myfun,x1,x2,maxiter:=maxiter) THEN
         msg(" end at x1=" & R.fmt(x1,style:=Fmt.Style.Fix,prec:=3) 
                  & " x2=" & R.fmt(x2,style:=Fmt.Style.Fix,prec:=3) 
                  & "\n");
      ELSE msg(" not found\n");
      END;
    EXCEPT
    | Error(code) => 
    END;
  END;  
  RETURN result;   
END test_bracket_out;
(*----------------------*)
PROCEDURE test_bracket_in():BOOLEAN=
CONST
  ftn = Module & "test_bracket_in";
VAR
  result:=TRUE;
  x1,x2:REAL64;
  nb:CARDINAL:=5;
  xb1:=NEW(Array,nb);
  xb2:=NEW(Array,nb);
  n,nbtmp:CARDINAL;  
BEGIN
  debug(1,ftn,"begin\n");
  msg("true roots: r1=" & R.fmt(r1)
               & " r2=" & R.fmt(r2)
               & " r3=" & R.fmt(r3) 
               & "\n");
  x1:=-50.0D0; x2:=+50.0D0;
  msg("start at x1=" & R.fmt(x1,style:=Fmt.Style.Fix,prec:=3) 
            & " x2=" & R.fmt(x2,style:=Fmt.Style.Fix,prec:=3)
            & " nb=" & Fmt.Int(nb)
            & "\n");
  FOR i:=10 TO 100 BY 10 DO
    n:=i; msg("n=" & Fmt.Int(n) & "\n");
    nbtmp:=nb; (*so we don't overwrite nb*)
    TRY
      IF xRoot.bracket_in(func:=myfun,x1:=x1,x2:=x2,n:=n,
      xb1:=xb1,xb2:=xb2,nb:=nbtmp) THEN
         FOR j:=0 TO nbtmp-1 DO
         msg(" found  x1=" & R.fmt(xb1[j],style:=Fmt.Style.Fix,prec:=3) 
                  & " x2=" & R.fmt(xb2[j],style:=Fmt.Style.Fix,prec:=3) 
                  & "\n");
         END;
      ELSE msg(" not found\n");
      END;
    EXCEPT
    | Error(code) => 
    END;
  END;  
  RETURN result;   
END test_bracket_in;
(*----------------------*)
PROCEDURE test_bisect():BOOLEAN=
CONST
  ftn = Module & "test_bisect";
VAR
  result:=TRUE;
  x1,x2,tol,root:REAL64;
BEGIN
  debug(1,ftn,"begin\n");
  msg("true roots: r1=" & R.fmt(r1)
               & " r2=" & R.fmt(r2)
               & " r3=" & R.fmt(r3) 
               & "\n");
  x1:=-1.0D0; x2:=2.9D0; tol:=0.001D0;
  msg("start at x1=" & R.fmt(x1,style:=Fmt.Style.Fix,prec:=3) 
            & " x2=" & R.fmt(x2,style:=Fmt.Style.Fix,prec:=3)
            & " tol=" & R.fmt(tol));
  root:=xRoot.bisect(myfun,x1,x2,tol);
  msg(" found  root=" & R.fmt(root,style:=Fmt.Style.Fix,prec:=3) 
    & "\n");
  RETURN result;   
END test_bisect;
(*----------------------*)
PROCEDURE test_brent():BOOLEAN=
CONST
  ftn = Module & "test_brent";
VAR
  result:=TRUE;
  x1,x2,tol,root:REAL64;
BEGIN
  debug(1,ftn,"begin\n");
  msg("true roots: r1=" & R.fmt(r1)
               & " r2=" & R.fmt(r2)
               & " r3=" & R.fmt(r3) 
               & "\n");
  x1:=-12.0D0; x2:=1.0D0; tol:=0.001D0;
  msg("start at x1=" & R.fmt(x1,style:=Fmt.Style.Fix,prec:=3) 
            & " x2=" & R.fmt(x2,style:=Fmt.Style.Fix,prec:=3)
            & " tol=" & R.fmt(tol));
  TRY
    root:=xRoot.brent(myfun,x1,x2,tol:=tol);
  EXCEPT
  | Error(code) =>
  ELSE
    msg("other error\n");
  END;
  msg(" found  root=" & R.fmt(root,style:=Fmt.Style.Fix,prec:=3) 
    & "\n");
  RETURN result;   
END test_brent;
(*----------------------*)
PROCEDURE test_newtraph():BOOLEAN=
CONST
  ftn = Module & "test_newtraph";
VAR
  result:=TRUE;
  x1,x2,tol,root:REAL64;
  maxiter:CARDINAL;
BEGIN
  debug(1,ftn,"begin\n");
  msg("true roots: r1=" & R.fmt(r1)
               & " r2=" & R.fmt(r2)
               & " r3=" & R.fmt(r3) 
               & "\n");
  x1:=6.0D0; x2:=5.0D0; tol:=0.001D0; maxiter:=15;
FOR i:=0 TO 10 DO
  x2:=x2+1.1D0; 
  msg("start at x1=" & R.fmt(x1,style:=Fmt.Style.Fix,prec:=3) 
            & " x2=" & R.fmt(x2,style:=Fmt.Style.Fix,prec:=3)
            & " tol=" & R.fmt(tol)
            & " maxiter=" & Fmt.Int(maxiter));
  TRY
    root:=xRoot.newtraph(myfun2,x1,x2,tol,maxiter);
    msg(" found  root=" & R.fmt(root,style:=Fmt.Style.Fix,prec:=5) 
      & "\n");
  EXCEPT
  | Error(code) => CASE code OF
                      | Err.not_bracketed=>msg(" not bracketed\n");
                      | Err.out_of_range=>msg(" jumped out\n");
                      | Err.not_converging=>msg(" not converging\n");
                      END;
  ELSE
    msg(" other error\n");
  END;
END;
  RETURN result;   
END test_newtraph;



(****************************
(*----------------------*)

PROCEDURE test_laguer():BOOLEAN=
CONST
  ftn = Module & "test_laguer";
  n=4; m=n-1;
VAR
  result:=TRUE;
  p:=NEW(na.cVector,n);
  x:C.COMPLEX;
BEGIN
  debug(1,ftn,"begin\n");
  msg("true roots: r1=" & R.fmt(r1)
               & " r2=" & R.fmt(r2)
               & " r3=" & R.fmt(r3) 
               & "\n");
  p^:=ARRAY [0..m] OF C.COMPLEX
      {C.COMPLEX(200.0,0.0), C.COMPLEX(-100.0,0.0),
       C.COMPLEX(-2.0,0.0),  C.COMPLEX(1.0,0.0)};
FOR i:=0 TO 10 DO
  x:=C.COMPLEX(FLOAT(i,REAL64),0.5);
  msg("start at x=" & C.fmt(x,style:=Fmt.Style.Fix,prec:=3)); 
  TRY
    na.laguer(p,m,x);
    msg(" found x=" & C.fmt(x,style:=Fmt.Style.Fix,prec:=5) 
      & "\n");
  EXCEPT
  | Error(code) => CASE code OF
                      | Err.divide_by_zero=>msg(" divide by zero\n");
                      | Err.out_of_range=>msg(" jumped out\n");
                      | Err.not_converging=>msg(" not converging\n");
                      END;
  ELSE
    msg(" other error\n");
  END;
END;
  RETURN result;   
END test_laguer;
(*----------------------*)
PROCEDURE test_zroots():BOOLEAN=
CONST
  ftn = Module & "test_zroots";
  n=4; m=n-1;
VAR
  result:=TRUE;
  p:=NEW(na.cVector,n);
  roots:na.cVector;
BEGIN
  debug(1,ftn,"begin\n");
  msg("true roots: r1=" & R.fmt(r1)
               & " r2=" & R.fmt(r2)
               & " r3=" & R.fmt(r3) 
               & "\n");
  p^:=ARRAY [0..m] OF C.COMPLEX
      {C.COMPLEX(200.0,0.0), C.COMPLEX(-100.0,0.0),
       C.COMPLEX(-2.0,0.0),  C.COMPLEX(1.0,0.0)};
  TRY
    na.zroots(p,roots,polish:=FALSE);
    msg("\n     raw roots:");
    FOR i:=0 TO m-1 DO
      msg(" root[" & Fmt.Int(i) & "]="
        & na.Ctext(roots[i],prec:=4));
    END;
    na.zroots(p,roots,polish:=TRUE);
    msg("\npolished roots:");
    FOR i:=0 TO m-1 DO
      msg(" root[" & Fmt.Int(i) & "]="
        & na.Ctext(roots[i],prec:=4));
    END;
  EXCEPT
  | Error(code) => CASE code OF
                      | Err.divide_by_zero=>msg(" divide by zero\n");
                      | Err.out_of_range=>msg(" jumped out\n");
                      | Err.not_converging=>msg(" not converging\n");
                      END;
  ELSE
    msg(" other error\n");
  END;

  RETURN result;   
END test_zroots;
******************************)
(*-------------------------*)
PROCEDURE test_Root():BOOLEAN=
CONST ftn = Module & "test_ch09_root";
VAR result:=TRUE;
BEGIN
  newline(); EVAL test_quadreal();
  newline(); EVAL test_quadcmplx();
  (*newline(); EVAL test_bracket_out();*)
  (*newline(); EVAL test_bracket_in();*)
  (*newline(); EVAL test_bisect();*)
  (*newline(); EVAL test_brent();*)
  (*newline(); EVAL test_newtraph();*)

  RETURN result;
END test_Root;
(*=======================*)
BEGIN
END tRoot.
