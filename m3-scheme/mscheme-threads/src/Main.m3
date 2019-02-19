(* $Id$ *)

MODULE Main;
IMPORT Math;
IMPORT Thread, Scan, Params;

VAR  n := Scan.Int(Params.Get(1));

TYPE 
  T = Thread.Closure OBJECT
  OVERRIDES
    apply := Apply;
  END;

PROCEDURE Apply(t : T) : REFANY =
  VAR x := 1.2d0;
  BEGIN
    LOOP
      x := Math.sqrt(x);
      x := x * x;
    END
  END Apply;

BEGIN
  FOR i := 1 TO n DO
    EVAL Thread.Fork(NEW(T)) 
  END;

  LOOP
    Thread.Pause(1.0d0)
  END
END Main.
