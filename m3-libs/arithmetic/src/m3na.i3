INTERFACE na;
(*The interface is Public Domain.
  The supporting implementations are copyrighted,
  but may be used free of charge so long as
  appropriate credit is given.  

  WARNING:  USE AT YOUR OWN RISK.
  The authors accept no responsibility for
  the accuracy, appropriateness or fitness for
  use of any of this material.

Abstract:  This is a Modula-3 rendition of a collection
           of numerical analysis routines.         

12/13/95   Harry George      Initial version
1/22/96    Harry George      Change to m3na project
*)
IMPORT Word,Fmt;

(*==================*)
(* xUtil: Utilities *)
(*==================*)              

EXCEPTION Error(Err);

TYPE Err= {bad_size       (*e.g., vector sizes mismatched*)
          ,b1_too_small    (*in tridiagonal, rewrite for n-1 eqns*)
          ,divide_by_zero  (*x/0 condition detected*)
          ,need_more_data  (*e.g., more data points in statistics*)
          ,not_bracketed   (*give x1,x2 do not bracket root*)
          ,not_converging  (*e.g., eps or maxiter too small*)
          ,not_implemented (*it's just a stub*)
          ,out_of_range    (*parm is out of range*)
         };

PROCEDURE sgn(a:REAL64):REAL64;
(*IF a >=0.0 THEN RETURN 1.0 ELSE RETURN -1.0; END:*)

(*=========================*)
(* xBits: Bits and Integers*)
(*=========================*)              
TYPE
  Card8  = BITS  8 FOR [0..16_FF];
  Card16 = BITS 16 FOR [0..16_FFFF];
  Card32 = Word.T;  (*use Word's operators*)

  Int8   = BITS  8 FOR [-16_80..16_7F];
  Int16  = BITS 16 FOR [-16_8000..16_7FFF];
  Int32  = BITS 32 FOR [FIRST(INTEGER)..LAST(INTEGER)];

PROCEDURE WordFmt(x:Word.T;
                  nbits:CARDINAL:=32;
                  base :CARDINAL:=2   (*typically 2 or 16*)
                      ):TEXT;
(*returns text for x, left padded
to nbits length by "0" if necessary.  
*)  
(*=================*)
(* Real Numbers    *)
(*=================*)
TYPE
  REAL32 = REAL;      (*IEEE 32-bit real*)
  REAL64 = LONGREAL;  (*IEEE 64-bit real*)
  Ftn    = PROCEDURE(x:REAL64):REAL64;
  
CONST
  (*---distinguished elements---*)
  Zero      = 0.0D0;
  Half      = 0.5D0;
  One       = 1.0D0;
  Two       = 2.0D0;
  SqrtTwo   = 1.414213562373095D0;
  LnTwo     = 0.693147180559945D0;  (*ln(2) *)
   
  Pi        = 3.141592653589793D0;
  TwoPi     = 6.283185307179586D0;
  OneOverPi = 0.318309886183791D0;
  TwoOverPi = 0.636619772367581D0;
  FourOverPi= 1.273239544735163D0;
  LnPi      = 1.144729885849400D0;  (*ln(pi) *)
  
  E         = 2.718281828459045D0;  (*natural log base "e"*)
  EulersConst=0.577215664901532D0;  (*Euler's constant "gamma"*)
  Golden    = 1.618033988749894D0;  (*golden ratio*)
  DegPerRad = 57.29577951308232D0;  (*degrees per radian*)
  RadPerDeg = 0.017453292519943D0;  (*radians per degree*)
 
  (*---boundaries for precision testing---*)
  TINY= 1.0D-300; (*nearly 0.0*)
  HUGE= 1.0D+300; (*nearly infinite*)
  EPS = 1.0D-17;  (*approx relative machine precision*)  

(*============================*)
(* xSpecFtn: Special Functions*)
(*============================*)
PROCEDURE factorial(n:CARDINAL):REAL64;

(*========================*)
(* xCmplx: Complex Numbers*)
(*========================*)
TYPE
   COMPLEX= RECORD re,im:REAL64; END;
   (*polar angles are in radians*)
   POLAR  = RECORD radius,angle:REAL64; END;

CONST
   cZero =  COMPLEX{re:=Zero,  im:=Zero};
   cOne  =  COMPLEX{re:=One,   im:=Zero};

TYPE  
  ComplexGroup <: PublicComplexGroup;
  PublicComplexGroup = OBJECT
  METHODS
    (*new, copy, zero, and one are via primitives*)
    lex(str:TEXT):COMPLEX RAISES {Error};
        (*reads after the "COMPLEX{" in COMPLEX{re:=<r>; im:=<r>},
        thru the "}"*)
    fmt(x:COMPLEX; 
        style:Fmt.Style:=Fmt.Style.Fix;
        prec:CARDINAL:=3):TEXT;
        (*outputs as "COMPLEX{re:=<r>; im:=<r>}"
        Uses simple Fmt.Real if x.im=0.0.*)

    abs(c:COMPLEX):REAL64;       (*return |c|*)
    arg(c:COMPLEX):REAL64;       (*return polar angle*)    
    conj(c:COMPLEX):COMPLEX;     (*return conjugate of c*)

    add(c1,c2:COMPLEX):COMPLEX;  (*return c1+c2*)
    sub(c1,c2:COMPLEX):COMPLEX;  (*return c1-c2*)
    mul(c1,c2:COMPLEX):COMPLEX;  (*return c1*c2*)
    div(c1,c2:COMPLEX):COMPLEX;  (*return c1/c2*)

    scale(c:COMPLEX;
          factor:REAL64):COMPLEX;(*return c*factor*)                 
    sqrt(c:COMPLEX):COMPLEX;     (*return sqrt(c) *)
    powN(c:COMPLEX;
         n:REAL64):COMPLEX;      (*return c^n*)
         (*NOTE: Also for roots, e.g., cube root: n=1/3*)
    powXY(x,y:COMPLEX):COMPLEX;  (*return x^y*)

    (*---transcendentals---*)
    exp(c:COMPLEX):COMPLEX;      (*return e^c *)
    ln (c:COMPLEX):COMPLEX;      (*return ln(c) *)

    (*---for trig and hyperbolics, must have |c|<=18---*)
    cos(c:COMPLEX):COMPLEX RAISES {Error}; (*return cos(c) *)
    sin(c:COMPLEX):COMPLEX RAISES {Error}; (*return sin(c) *)
    tan(c:COMPLEX):COMPLEX RAISES {Error}; (*return tan(c) *)
    cosh(c:COMPLEX):COMPLEX RAISES {Error};(*return cosh(c) *)
    sinh(c:COMPLEX):COMPLEX RAISES {Error};(*return sinh(c) *)
    tanh(c:COMPLEX):COMPLEX RAISES {Error};(*return tanh(c) *)
        
    (*---polar form---*)
    toPolar(c:COMPLEX):POLAR;
    fromPolar(c:POLAR):COMPLEX;
    fmtPolar(c:POLAR;
        style:Fmt.Style:=Fmt.Style.Fix;
        prec:CARDINAL:=3):TEXT;
        (*as "POLAR{radius:=<r>; angle:=<r>}"*)
        
    pmul(p1,p2:POLAR):POLAR;     (*return p1*p2*)
    pdiv(p1,p2:POLAR):POLAR;     (*return p1/p2*)

 END;


(*=======================*)
(* xQuatern: Quaternions *)
(*=======================*)
TYPE
  QUATERN = RECORD r,i,j,k:REAL64; END;
  
(*==================*)
(* xArray: Arrays   *)
(*==================*)
TYPE
  Array = REF ARRAY OF REAL64;
  iArray= REF ARRAY OF INTEGER;
  cArray= REF ARRAY OF COMPLEX;

(*
  sort(a:Array);
  isort(a:iArray);
  csort(a:cArray);

  minmax(a:Array;      (*for this array*)
         VAR min,max); (*find min and maxvalues*)
*)
(*===============================*)
(* xInterp: Interpolate          *)
(*===============================*)
PROCEDURE interp_linear(
                 READONLY xa,ya:ARRAY OF REAL64;  (*interp table*)
                 x:REAL64;                        (*the input*)
                 ):REAL64  RAISES {Error};
(*Given an interpolation table with xa input and ya output,
do linear interpolation for x.
*)

PROCEDURE interp_newt(
                 READONLY xa,ya:ARRAY OF REAL64;(*interp table*)
                 x:REAL64;                      (*the input*)
                 VAR dy:REAL64;                 (*the error estimate*)
                 start,len:CARDINAL:=0          (*for partial access*)
                 ):REAL64 RAISES {Error};
(*Given an interpolation table with xa input and ya output,
do Newton polynomial interpolation for x.  Report dy as error estimate.
Partial access: Give the starting index and the length to be used.
*)

(*===============================*)
(* xRoot: Find roots of functions*)
(*===============================*)
(*---See also Polynomials---*)

(*----------------*)
PROCEDURE bracket_out(func:Ftn;      (*find brackets for this function*)
                VAR x1,x2:REAL64;    (*starting with these points*)
                maxiter:CARDINAL:=55 (*growing maxiter times*)
                ):BOOLEAN RAISES {Error}; (*true if successful*)
(*Given x1,x2, search for points (returned in x1, x2) for which
func(x1) is opposite sign from func(x2).  Grow outward from
the original x1,x2 by golden ratio, for geometric growth.
Return true if a good x1,x2 can be found before getting to
maxiter, else return false. 

requires: x1<x2.
*)

(*----------------*)
PROCEDURE bracket_in(func:Ftn;       (*find brackets for this function*)
                VAR x1,x2:REAL64;    (*starting with these points*)
                n:CARDINAL;          (*using n segments*)
                xb1,xb2:Array;       (*returning pairs here*)
                VAR nb:CARDINAL      (*with this count of valid pairs*)
                ):BOOLEAN RAISES {Error}; (*true if successful*)
(*Break up the x1..x2 range into n segments.  Search for pairs which
allow bracketing.  Save up to nb of these pairs.

requires: x1<x2,  nb<n.
*)

(*------------------*)
PROCEDURE root_bisect(func:Ftn;      (*find root of this function*)
                 x1,x2:REAL64;       (*between these brackets*)
                 tol:REAL64;         (*to within +/- tolerance*)
                 maxiter:=45         (*but no more than maxiter cuts*)
                 ):REAL64 RAISES {Error}; (*returning the root*)
(*Given brackets x1,x2, find a root via bisection, and refine it
to within +/- tol
*)
(*------------------*)
PROCEDURE root_brent(func:Ftn;       (*find a root of this function*)
                 x1,x2:REAL64;       (*between these bracket points*)
                 tol:REAL64;       (*to this tolerance*)
                 maxiter:=100        (*with <= maxiter iterations*)
                 ):REAL64 RAISES {Error};
(*Use Brent's algorithm to find the real root between the
bracket points.  x1 and x2 must be of opposite signs.
*)
(*---------------------*)
PROCEDURE root_newtraph(
                 func:PROCEDURE(x:REAL64; VAR f,df:REAL64); (*this ftn*)
                 x1,x2:REAL64;    (*bracketed by these points*)
                 tol:REAL64;      (*find root to this precision*)
                 maxiter:=25      (*with no more than maxiter loops*)
                 ):REAL64 RAISES {Error};  (*returning root*)
(*Given a function which returns both f(x) and df(x),
and brackets x1 and x2, find the root to tol precision.
Works via newton-raphson and bisection.
*)


(*=================================*)
(* xCheby: Chebyshev Approximations*)
(*=================================*)
(*

TYPE
  ChebyApprox<: PublicCheby;
  PublicCheby = OBJECT
  METHODS
    init(func:Ftn;                         (*ftn to be approximated*)
         maxn:CARDINAL:=30                 (*number of coeffs to find*)
         ):ChebyApprox;
    findm(prec:REAL64:=0.0001D0):CARDINAL RAISES {Error};
                                           (*use m coeffs for precision*)
    derivative(m:CARDINAL):ChebyApprox;    (*return derivative*)
    integral(m:CARDINAL):ChebyApprox;      (*return integral*)
    eval(x:REAL64;                         (*-1<x<+1*)
         m:CARDINAL                        (*m:=findm() is a good idea*)
        ):REAL64 RAISES {Error};           (*returned value*)
  END;
  
*)
(*====================*)
(* xPoly: Polynomials *)
(*====================*)              

TYPE
  (*interpretation is: a[0] + a[1]*x + a[2]* x^2...a[n]*x^n *)
  (*text form is: Poly4{a0,a1,a2,a3} *)
  
  Poly = BRANDED "Poly"  REF ARRAY OF REAL64;
  cPoly =BRANDED "cPoly" REF ARRAY OF COMPLEX;

  PolyGroup <: PublicPolyGroup;
  PublicPolyGroup = OBJECT
  METHODS
    new(n:CARDINAL):Poly;    (*make a poly for a0..an*)
    copy(p:Poly):Poly;       (*copy p to a new poly*)
    lex(str:TEXT):Poly;
       (* from text form*)
    fmt(p:Poly;
        style:Fmt.Style:=Fmt.Style.Fix;
        prec:CARDINAL:=1):TEXT;
        (*to text form*)

    Zero(p:Poly);          (*set p to zeros*)
    One (p:Poly);          (*set p to 1*)
    
    eval(p:Poly;           (*eval this polynomial*)
         x:REAL64          (*at this point*)
         ):REAL64;
    add(p1,p2:Poly):Poly;  (*return p1+p2*)
    sub(p1,p2:Poly):Poly;  (*return p1-p2*)
    mul(p1,p2:Poly):Poly;  (*return p1*p2*)
    div(p1,p2:Poly;        (*compute p1/p2 *) 
        VAR q,r:Poly);     (*giving quotient q with remainder r*)
    deflate(p:Poly;        (*divide this polynomial*)
            c:REAL64;      (* by (x-c) *)
            VAR rem:REAL64);(*leaving remainder -- possible 0*)
    deriv(p:Poly;          (*eval this polynomial*)
          x:REAL64;        (*at this point*)
          pd:Array;        (*returning p(x), p'(x)...*)
          nd:CARDINAL      (*for up to nd derivatives*)
          ) RAISES {Error};
          (*raises:
               Err.bad_size if nd>NUMBER(pd)+1
          *)
  END;           
(*==================*)
(* xVect: Vectors   *)
(*==================*)              
TYPE
  (*text form: "[a0,a1,a2,a3,a4,a5]"*)
  Vector  = BRANDED "Vector"  REF ARRAY OF REAL64;

  VectorGroup<:PublicVectorGroup;
  PublicVectorGroup = OBJECT
  METHODS
    new(n:CARDINAL):Vector; (*make new nx1 Vector*)
    copy(v:Vector):Vector;
    lex(str:TEXT):Vector RAISES {Error};
    fmt(v:Vector;
        style:=Fmt.Style.Fix;
        prec:=2):TEXT;
    
    Zero(v:Vector);                   (*set to zero*)
    (*NOTE: you should make unit vectors as needed*)
    
    abs(v:Vector):REAL64;                      (*|v|*)
    add(v1,v2:Vector):Vector RAISES {Error};   (*v1+v2*)
    sub(v1,v2:Vector):Vector RAISES {Error};   (*v1-v2*)
    scale(v:Vector; factor:REAL64);            (*v1:=v1*factor*)
    dot(v1,v2:Vector):REAL64 RAISES {Error};   (*v1 dot v2*)
    cross(v1,v2:Vector):Vector RAISES {Error}; (*v1 x v2*)       
  END;


(*==================*)
(* xMat: Matrices   *)
(*==================*)              
(*-----------------*)
TYPE
(*
|   text form: "[[a00,a01,a02,a03,a04,a05]
|                [a10,a11,a12,a13,a14,a15]
|               ]"
*)
  Matrix  = BRANDED "Matrix"  REF ARRAY OF ARRAY OF REAL64;

  MatrixGroup<:PublicMatrixGroup;
  PublicMatrixGroup = OBJECT
  METHODS
    new(m,n:CARDINAL):Matrix; (*make new mxn matrix*)
    copy(mat:Matrix):Matrix;
    lex(str:TEXT):Matrix RAISES {Error};
    fmt(mat:Matrix;
        style:=Fmt.Style.Fix;
        prec:=2):TEXT;
    
    Zero(mat:Matrix);                              (*set to zeros*)
    One (mat:Matrix) RAISES {Error};               (*set to identity*)

    add(mat1,mat2:Matrix):Matrix RAISES {Error};   (*mat1  +mat2*)
    sub(mat1,mat2:Matrix):Matrix RAISES {Error};   (*mat1 - mat2*)
    mul(mat1,mat2:Matrix):Matrix RAISES {Error};   (*mat1 * mat2*)
    mulV(A:Matrix;b:Vector):Vector RAISES {Error}; (*A * b*)
    transpose(mat:Matrix):Matrix;                  (*mat^T*)
  END;

(*========================*)
END na.
        
