(* $Id$ *)

(*
  Copyright (c) 2008, Generation Capital Ltd.  All rights reserved.

  Author: Mika Nystrom <mika@alum.mit.edu>
*)

MODULE SchemeLongReal;
IMPORT Scan;
FROM SchemeUtils IMPORT Error, StringifyT;
FROM Scheme IMPORT Object, E;
IMPORT Lex, FloatMode;

PROCEDURE FromI(x : INTEGER) : T = 
  BEGIN RETURN FromLR(FLOAT(x,LONGREAL)) END FromI;

PROCEDURE FromLR(x : LONGREAL) : T =
  BEGIN
    IF x >= -1.0d0 AND x <= 2.0d0 THEN
      IF    x = 0.0d0 THEN RETURN Zero
      ELSIF x = 1.0d0 THEN RETURN One
      ELSIF x = -1.0d0 THEN RETURN NegOne
      ELSIF x = 2.0d0 THEN RETURN Two
      ELSE
        WITH new = NEW(T) DO new^ := x; RETURN new END
      END
    ELSE
      WITH new = NEW(T) DO new^ := x; RETURN new END
    END
  END FromLR;

PROCEDURE FromO(x : Object) : LONGREAL RAISES { E } =
  BEGIN
    IF x # NIL AND ISTYPE(x,T) THEN RETURN NARROW(x,T)^ 
    ELSE RETURN FromO(Error("expected a double, got: " & StringifyT(x)))
    END
  END FromO;

PROCEDURE Card(x : Object; roundOK : BOOLEAN) : CARDINAL RAISES { E } =
  BEGIN
    WITH int = Int(x,roundOK) DO
      IF int < FIRST(CARDINAL) OR int > LAST(CARDINAL) THEN
        RETURN Int(Error("number out of range : " & StringifyT(x)),FALSE)
      END;
      RETURN int
    END
  END Card;

PROCEDURE Int(x : Object; roundOK : BOOLEAN) : INTEGER RAISES { E } =
  BEGIN
    WITH lr = FromO(x) DO
      IF lr < FLOAT(FIRST(INTEGER),LONGREAL) 
        OR 
         lr > FLOAT(LAST(INTEGER),LONGREAL) THEN
        RETURN Int(Error("number out of range : " & StringifyT(x)),FALSE)
      END;

      WITH res = ROUND(lr) DO
        IF NOT roundOK AND FLOAT(res,LONGREAL) # lr THEN
          RETURN Int(Error("not an integer : " & StringifyT(x)),FALSE)
        END;
        RETURN res
      END
    END
  END Int;

PROCEDURE FromT(t : TEXT) : T RAISES { E }=
  BEGIN 
    TRY
      RETURN FromLR(Scan.LongReal(t)) 
    EXCEPT
      FloatMode.Trap, Lex.Error =>
      RETURN Error("Not a number: " & t) 
    END
  END FromT;

VAR
  NegOne, Two := NEW(T);

BEGIN 
  Zero := NEW(T); One := NEW(T);
  NegOne^ := -1.0d0;
  Zero^ := 0.0d0;
  One^  := 1.0d0;
  Two^ := 2.0d0;
END SchemeLongReal.




