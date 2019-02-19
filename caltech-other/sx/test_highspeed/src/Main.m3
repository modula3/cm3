(* $Id$ *)

MODULE Main;
IMPORT SXInt;
IMPORT Wr, Stdio, SXSelect, SX, Thread, Random, Fmt;

TYPE
  Reader = Thread.Closure OBJECT
    sx1, sx2 : SXInt.T;
  OVERRIDES
    apply := RApply;
  END;

PROCEDURE RApply(r : Reader) : REFANY = 
  BEGIN
    SX.Lock1(r.sx1);
    LOOP
      SXSelect.Wait1(r.sx1);
      WITH val = r.sx1.value() DO
        IF val = 0 THEN
          Wr.PutChar(Stdio.stdout, '.');
          Wr.PutChar(Stdio.stdout, '\n')
        ELSIF val > 9 THEN 
          Wr.PutChar(Stdio.stdout, 'X')
        ELSE
          Wr.PutChar(Stdio.stdout, VAL(ORD('0')+val,CHAR))
        END;
        NARROW(r.sx2, SXInt.Var).set(val)
      END;
      Wr.Flush(Stdio.stdout)
    END
  END RApply;

VAR
  sx1 : SXInt.Var := NEW(SXInt.Var).init();
  sx2 : SXInt.Var := NEW(SXInt.Var).init();
  rand := NEW(Random.Default).init();
BEGIN
  EVAL Thread.Fork(NEW(Reader, sx1 := sx1, sx2 := sx2));
  LOOP
    WITH num = rand.integer(1,10000) DO Wr.PutText(Stdio.stdout, "[" & Fmt.Int(num) & "]"); Wr.Flush(Stdio.stdout);
      FOR i := 1 TO num DO
        sx1.set(i)
      END;
      sx1.set(0);
      SX.Lock1(sx2);
      TRY 
        WHILE sx2.value() # 0 DO SXSelect.Wait1(sx2) END 
      EXCEPT SX.Uninitialized => (* skip *) 
      END;
      SX.Unlock1(sx2)
    END
  END
END Main.
