(* $Id$ *)

GENERIC MODULE SXNumOps(Elem, Elem_ElemFuncOps, Elem_IntFuncOps, Elem_BoolFuncOps);
IMPORT SXBool;
IMPORT SXInt;

PROCEDURE TimesB(a, b : Elem.Base) : Elem.Base =
  BEGIN RETURN a*b END TimesB;

PROCEDURE ModB(a, b : Elem.Base) : Elem.Base =
  BEGIN RETURN a MOD b END ModB;

PROCEDURE PlusB(a, b : Elem.Base) : Elem.Base =
  BEGIN RETURN a+b END PlusB;

PROCEDURE MinusB(a, b : Elem.Base) : Elem.Base =
  BEGIN RETURN a-b END MinusB;

PROCEDURE MaxB(a, b : Elem.Base) : Elem.Base =
  BEGIN RETURN MAX(a,b) END MaxB;

PROCEDURE MinB(a, b : Elem.Base) : Elem.Base =
  BEGIN RETURN MIN(a,b) END MinB;

PROCEDURE GTB(a, b : Elem.Base) : BOOLEAN =
  BEGIN RETURN a>b END GTB;

PROCEDURE LTB(a, b : Elem.Base) : BOOLEAN =
  BEGIN RETURN a<b END LTB;

PROCEDURE GEB(a, b : Elem.Base) : BOOLEAN =
  BEGIN RETURN a>=b END GEB;

PROCEDURE LEB(a, b : Elem.Base) : BOOLEAN =
  BEGIN RETURN a<=b END LEB;

(**********************************************************************)

PROCEDURE Times(a, b : Elem.T; ss : BOOLEAN) : Elem.T =
  BEGIN 
    IF ss THEN
      RETURN Elem_ElemFuncOps.BinarySymmetricShortCircuitFunc(a,b,TimesB,
                                                              Zero, Zero,
                                                              "Times") 
    ELSE
      RETURN Elem_ElemFuncOps.BinaryFunc(a,b,TimesB,"Times") 
    END
  END Times;

PROCEDURE Div(a, b : Elem.T) : Elem.T=
  BEGIN RETURN Elem_ElemFuncOps.BinaryFunc(a,b,DivB,"Div") END Div;

PROCEDURE Mod(a, b : Elem.T) : Elem.T =
  BEGIN RETURN Elem_ElemFuncOps.BinaryFunc(a,b,ModB,"Mod") END Mod;

PROCEDURE Plus(a, b : Elem.T) : Elem.T=
  BEGIN RETURN Elem_ElemFuncOps.BinaryFunc(a,b,PlusB,"Plus") END Plus;

PROCEDURE Minus(a, b : Elem.T) : Elem.T=
  BEGIN RETURN Elem_ElemFuncOps.BinaryFunc(a,b,MinusB,"Minus") END Minus;

PROCEDURE Min(a, b : Elem.T) : Elem.T=
  BEGIN RETURN Elem_ElemFuncOps.BinaryFunc(a,b,MinB,"Min") END Min;

PROCEDURE Max(a, b : Elem.T) : Elem.T=
  BEGIN RETURN Elem_ElemFuncOps.BinaryFunc(a,b,MaxB,"Max") END Max;

PROCEDURE Equal(a, b : Elem.T) : SXBool.T =
  BEGIN RETURN Elem_BoolFuncOps.BinaryFunc(a, b, Elem.BaseEqual,"Equal") END Equal;

PROCEDURE Compare(a, b : Elem.T) : SXInt.T =
  BEGIN RETURN Elem_IntFuncOps.BinaryFunc(a, b, Elem.BaseCompare, "Compare") END Compare;

PROCEDURE GT(a, b : Elem.T) : SXBool.T =
  BEGIN RETURN Elem_BoolFuncOps.BinaryFunc(a, b, GTB,"GT") END GT;

PROCEDURE LT(a, b : Elem.T) : SXBool.T =
  BEGIN RETURN Elem_BoolFuncOps.BinaryFunc(a, b, LTB,"LT") END LT;

PROCEDURE GE(a, b : Elem.T) : SXBool.T =
  BEGIN RETURN Elem_BoolFuncOps.BinaryFunc(a, b, GEB,"GE") END GE;

PROCEDURE LE(a, b : Elem.T) : SXBool.T =
  BEGIN RETURN Elem_BoolFuncOps.BinaryFunc(a, b, LEB,"LE") END LE;

(**********************************************************************)

PROCEDURE AbsB(a : Elem.Base) : Elem.Base =
  BEGIN RETURN ABS(a) END AbsB;

PROCEDURE Abs(a : Elem.T) : Elem.T =
  BEGIN RETURN Elem_ElemFuncOps.UnaryFunc(a, AbsB,"Abs") END Abs;

PROCEDURE UMinusB(a : Elem.Base) : Elem.Base =
  BEGIN RETURN -a END UMinusB;

PROCEDURE UMinus(a : Elem.T) : Elem.T =
  BEGIN RETURN Elem_ElemFuncOps.UnaryFunc(a, UMinusB,"UMinus") END UMinus;

(**********************************************************************)

PROCEDURE Sum(READONLY a : ARRAY OF Elem.T) : Elem.T =
  BEGIN RETURN Elem_ElemFuncOps.NAryFunc(a, SumB, "Sum") END Sum;

PROCEDURE Prod(READONLY a : ARRAY OF Elem.T) : Elem.T =
  BEGIN RETURN Elem_ElemFuncOps.NAryFunc(a, ProdB, "Prod") END Prod;

PROCEDURE SumB(READONLY a : ARRAY OF Elem.Base) : Elem.Base =
  VAR 
    s := Zero;
  BEGIN
    FOR i := FIRST(a) TO LAST(a) DO
      s := s + a[i]
    END;
    RETURN s
  END SumB;
    
PROCEDURE ProdB(READONLY a : ARRAY OF Elem.Base) : Elem.Base =
  VAR 
    s := One;
  BEGIN
    FOR i := FIRST(a) TO LAST(a) DO
      s := s * a[i]
    END;
    RETURN s
  END ProdB;

(**********************************************************************)    

TYPE
  Weights = Elem_ElemFuncOps.ON OBJECT
    w : REF ARRAY OF Elem.Base;
  OVERRIDES
    op := WeightedSumOp;
  END;

PROCEDURE WeightedSumOp(weights : Weights; 
                        READONLY a : ARRAY OF Elem.Base) : Elem.Base =
  VAR
    sum := Zero;
  BEGIN
    FOR i := FIRST(a) TO LAST(a) DO
      sum := sum + weights.w[i] * a[i]
    END;
    RETURN sum
  END WeightedSumOp;

PROCEDURE WeightedSum(READONLY w : ARRAY OF Elem.Base; 
                      READONLY a : ARRAY OF Elem.T) : Elem.T =
  BEGIN
    <* ASSERT NUMBER(w) = NUMBER(a) *>
    WITH weights = NEW(Weights, w := NEW(REF ARRAY OF Elem.Base, NUMBER(w))) DO
      weights.w^ := w;
      RETURN Elem_ElemFuncOps.NAryOFunc(a, weights, "WeightedSum")
    END
  END WeightedSum;

TYPE
  Ave = Elem_ElemFuncOps.ON OBJECT
  OVERRIDES
    op := AverageOp;
  END;

PROCEDURE AverageOp(<*UNUSED*>av : Ave;
                    READONLY a : ARRAY OF Elem.Base) : Elem.Base =
  VAR
    sum := Zero;
    cnt := Zero;
  BEGIN
    FOR i := FIRST(a) TO LAST(a) DO
      sum := sum + a[i];
      cnt := cnt + One
    END;
    RETURN DivB(sum,cnt)
  END AverageOp;

PROCEDURE Average(READONLY a : ARRAY OF Elem.T; u : CARDINAL) : Elem.T =
  BEGIN
    WITH ave = NEW(Ave) DO
      RETURN Elem_ElemFuncOps.NAryOUFunc(a, ave, u, "Average")
    END
  END Average;

BEGIN END SXNumOps.



