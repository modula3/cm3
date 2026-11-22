(* $Id: Main.m3,v 1.1 2005/05/03 23:20:24 kp Exp $ *)

MODULE Main;
IMPORT Debug;
IMPORT IDGen;
IMPORT Fmt;

VAR
  gen := NEW(IDGen.Low).init();

BEGIN
  FOR i := 1 TO 99 DO
    Debug.S(Fmt.Int(gen.alloc()), 0);
  END;
  gen.free(39);
  gen.free(23);
  gen.free(78);
  FOR i := 1 TO 10 DO
    Debug.S(Fmt.Int(gen.alloc()), 0);
  END;
END Main.
