(* $Id$ *)

MODULE LimitFmt;
IMPORT Scan, Fmt;
IMPORT Lex, FloatMode;

PROCEDURE LongReal(lr, maxError : LONGREAL) : TEXT =
  <*FATAL Lex.Error, FloatMode.Trap *>
  BEGIN
    <* ASSERT maxError >= 0.0d0 *>
    IF maxError > 0.0d0 THEN
      FOR i := 0 TO 12 DO
        WITH fmt = Fmt.LongReal(lr, prec := i),
             rb = Scan.LongReal(fmt),
             absErr = ABS(lr-rb) DO
          IF absErr < maxError THEN 
            RETURN fmt
          END
        END
      END
    END;

    RETURN Fmt.LongReal(lr)
  END LongReal;

BEGIN END LimitFmt.
