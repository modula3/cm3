MODULE Main;

IMPORT Fmt, Random;

FROM Math IMPORT expm1, log1p, acosh, asinh, atanh, cosh, sinh, tanh;

IMPORT UnitTest, UnitTestList, UnitTestNumeric, UnitTestTerminalText;


(*
test:
   (1+expm1(x))*(1+expm1(y))=(1+expm1(x+y))
   expm1(x+y)  =  expm1(x) + expm1(y) + expm1(x)*expm1(y)

   log1p(x)+log1p(y)  =  log1p(x+y+x*y)

   expm1(log1p(x)) = x   log1p(expm1(x)) = x


   cosh(acosh(x)) = x   acosh(cosh(x)) = x
   sinh(asinh(x)) = x   asinh(sinh(x)) = x
   tanh(atanh(x)) = x   atanh(tanh(x)) = x
*)

PROCEDURE CheckInverse (SELF   : UnitTestNumeric.T;
                        f, finv: PROCEDURE (x: LONGREAL): LONGREAL;
                        fName, finvName : TEXT;
                        x, y, tolX, tolY: LONGREAL; ) =
  BEGIN
    IF NOT SELF.scalarMatch(finv(f(x)), x, tolX) THEN
      SELF.message(finvName & " o " & fName & "\n");
    END;

    IF NOT SELF.scalarMatch(f(finv(y)), y, tolY) THEN
      SELF.message(fName & " o " & finvName & "\n");
      SELF.message(
        Fmt.FN(
          "finv(y)=%s, f(finv(y))=%s\n",
          ARRAY OF TEXT{Fmt.LongReal(finv(y)), Fmt.LongReal(f(finv(y)))}));
    END;
  END CheckInverse;

PROCEDURE CheckExponential (SELF: UnitTestNumeric.T; ) =
  VAR rnd := NEW(Random.Default).init();
  BEGIN
    FOR j := 0 TO 100 DO
      (* The range is choosen with respect to thresholds in the Windows
         work-arounds for these functions, such that there is a 1:1 chance
         to get the built-in function and the M3 replacement. *)
      WITH x = rnd.longreal(-0.1D0, 0.1D0),
           y = rnd.longreal(-0.1D0, 0.1D0)  DO

        IF NOT SELF.scalarMatch(
                 expm1(x + y), expm1(x) + expm1(y) + expm1(x) * expm1(y),
                 1.0D-16) THEN
          SELF.message("exp as homomorphism\n");
        END;

        IF NOT SELF.scalarMatch(
                 log1p(x) + log1p(y), log1p(x + y + x * y), 1.0D-16) THEN
          SELF.message("log as homomorphism\n");
        END;
        CheckInverse(
          SELF, expm1, log1p, "expm1", "log1p", x, x, 1.0D-16, 1.0D-16);
      END;

      WITH x = rnd.longreal(-0.2D0, 0.2D0) DO
        CheckInverse(
          SELF, sinh, asinh, "sinh", "asinh", x, x, 1.0D-16, 1.0D-16);
        CheckInverse(
          SELF, tanh, atanh, "tanh", "atanh", x, x, 1.0D-16, 1.0D-16);
      END;

      WITH x = rnd.longreal(0.001D0, 0.1D0),
           y = rnd.longreal(1.0D0, 2.0D0)    DO
        CheckInverse(
          SELF, cosh, acosh, "cosh", "acosh", x, y, 1.0D-13, 1.0D-15);
      END;
    END;
  END CheckExponential;


PROCEDURE Test (): UnitTest.T =
  BEGIN
    RETURN
      NEW(UnitTestList.T).init(
        "Standard real functions",
        ARRAY OF
          UnitTest.T{NEW(UnitTestNumeric.T, test := CheckExponential).init(
                       "exponential")});
  END Test;

BEGIN
  UnitTest.Run(Test(), NEW(UnitTestTerminalText.T).init());
END Main.
