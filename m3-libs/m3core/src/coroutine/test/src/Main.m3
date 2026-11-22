MODULE Main;
IMPORT Coroutine;
IMPORT Debug;
IMPORT Word;

PROCEDURE A() =
  VAR
    bCl := NEW(Coroutine.Closure, apply := BApply);
    b : Coroutine.T;
  BEGIN
    Debug.Out("D0");
    b := Coroutine.Create(bCl);
    Debug.Out("D1");
    EVAL Coroutine.Call(b);
    Debug.Out("D3");
    EVAL Coroutine.Call(b);
    Debug.Out("D5");
    (*EVAL Coroutine.Call(b); (* should fail *)*)
  END A;

PROCEDURE BApply(cl : Coroutine.Closure; from : Coroutine.T) : REFANY =
  BEGIN
    <*ASSERT from # NIL*>
    Debug.Out("D2");
    EVAL Coroutine.Call(from);
    Debug.Out("D4");
    RETURN NIL
  END BApply;

PROCEDURE C() =
  VAR x := NEW(REF ARRAY OF CHAR, 32768);
      z : Word.T := 0;
  BEGIN
    FOR i := FIRST(x^) TO LAST(x^) DO
      z := Word.Plus(z, ORD(x[i]))
    END
  END C;

PROCEDURE D() =
  VAR
    eCl := NEW(Coroutine.Closure, apply := EApply);
    e : Coroutine.T;
  BEGIN
    e := Coroutine.Create(eCl);
    FOR i := 1 TO 1000000 DO
      EVAL Coroutine.Call(e)
    END
  END D;

PROCEDURE EApply(cl : Coroutine.Closure; from : Coroutine.T) : REFANY =
  BEGIN
    FOR i := 1 TO 1000000-1 DO
      EVAL Coroutine.Call(from)
    END;
    RETURN NIL
  END EApply;

PROCEDURE GApply(cl : Coroutine.Closure; from : Coroutine.T) : REFANY =
  BEGIN
    RETURN NIL
  END GApply;

PROCEDURE F() =
  BEGIN
    (* should not leak memory... *)
    FOR i := 1 TO 1000000 DO
      WITH cl = NEW(Coroutine.Closure, apply := GApply),
           g  = Coroutine.Create(cl) DO
        EVAL Coroutine.Call(g)
      END
    END
  END F;

CONST Test = ARRAY OF PROCEDURE() { A, C, D, F, H };

PROCEDURE H() =
  BEGIN
    WITH cl = NEW(Coroutine.Closure, apply := IApply),
         i  = Coroutine.Create(cl) DO
      EVAL Coroutine.Call(i);
      Debug.Out("res=" & Debug.UnNil(Coroutine.Retval(i)));
      EVAL Coroutine.Call(i);
      Debug.Out("res=" & Debug.UnNil(Coroutine.Retval(i)));
      EVAL Coroutine.Call(i);
    END
  END H;

PROCEDURE IApply(cl : Coroutine.Closure; from : Coroutine.T) : REFANY =
  BEGIN
    EVAL Coroutine.Call(from);
    RETURN "done!"
  END IApply;    
      
BEGIN
  FOR i := 0 TO 99999 DO
    Debug.Out("D-1");
    Test[0]()
  END
END Main.
