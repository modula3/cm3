(* $Id: Main.m3,v 1.1 2007/07/15 20:38:32 mika Exp $ *)

MODULE Main;
IMPORT StepFunction; FROM StepFunction IMPORT DomainError;
IMPORT Fmt;
IMPORT Debug, Math;

VAR
  f := NEW(StepFunction.Default).init();
  
PROCEDURE Eval(n : TEXT; f : StepFunction.T; x : LONGREAL) =
  BEGIN
    TRY
      Debug.Out(n & "(" & Fmt.LongReal(x) & ") = " & Fmt.LongReal(f.eval(x)))
    EXCEPT
      DomainError(at) => Debug.Out(n&"(" & Fmt.LongReal(x) & ") = : DomainError at x= "& Fmt.LongReal(at.x))
    END
  END Eval;

PROCEDURE Print(f : StepFunction.T; n : TEXT) =
  BEGIN

  FOR i := 0 TO 11 DO
    WITH lr = FLOAT(i,LONGREAL),
         less = lr - 0.1d0,
         more = lr + 0.1d0 DO
      Eval(n,f,less);
      Eval(n,f,lr);
      Eval(n,f,more)
    END
  END;

  WITH steps = f.steps() DO
    FOR i := 0 TO steps.size()-1 DO
      WITH lr = FLOAT(steps.get(i),LONGREAL),
           less = lr - 0.1d0,
           more = lr + 0.1d0 DO
        Eval(n&"/steps",f,less);
        Eval(n&"/steps",f,lr);
        Eval(n&"/steps",f,more)
      END
    END
  END
  END Print;

VAR
  one := StepFunction.One(1.3d0,3.9d0);

BEGIN
  FOR i := 0 TO 10 DO
    WITH lr = FLOAT(i,LONGREAL) DO
      f.add(lr,lr)
    END
  END;

  Print(f,"f");

  Print(StepFunction.Mul(f,one),"f*one");


END Main.
