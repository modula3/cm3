(* $Id$ *)

GENERIC MODULE Map(From, To);
IMPORT Word;
IMPORT Thread, ThreadSeq;

REVEAL 
  T = Public BRANDED Brand OBJECT OVERRIDES 
    hash := THash;
    evalD := TEvalD;
  END;

TYPE
  PublicDef = T OBJECT METHODS wrap(f : F) : T END;

REVEAL 
  Default = PublicDef BRANDED "Default " & Brand OBJECT 
    f : F := NIL;
  OVERRIDES
    wrap := Wrap;  
    eval := DEval;
    evalHint := EvalHint;
    registerThread := RegisterThread;
    waitForSomeThreads := WaitForSomeThreads;
  END;

PROCEDURE THash(<*UNUSED*>a : T) : Word.T = BEGIN RETURN 1 END THash;

PROCEDURE Hash(a : T) : Word.T =            BEGIN RETURN a.hash() END Hash;

PROCEDURE Equal(a,b : T) : BOOLEAN =        BEGIN RETURN a = b END Equal;

PROCEDURE Wrap(a : Default; f : F) : T =
  BEGIN 
    <* ASSERT a.f = NIL *>
    a.f := f;
    RETURN a
  END Wrap;

VAR threads := NEW(ThreadSeq.T).init();
VAR mu := NEW(MUTEX);

TYPE 
  MapClosure = Thread.Closure OBJECT
    a : T;
    x : From.T;
  OVERRIDES
    apply := MapClApply
  END;

PROCEDURE MapClApply(cl : MapClosure) : REFANY =
  BEGIN EVAL cl.a.eval(cl.x); RETURN NIL END MapClApply;
  
PROCEDURE EvalHint(a : T; x : From.T) = 
  BEGIN 
    IF a.hintsByForking THEN
      LOCK mu DO
        IF threads.size() >= a.maxThreads THEN
          WITH t = threads.remhi() DO
            EVAL Thread.Join(t)
          END
        END;
        threads.addlo(Thread.Fork(NEW(MapClosure, a := a, x := From.Copy(x))));
      END
    END
  END EvalHint;

PROCEDURE RegisterThread(<*UNUSED*>a : T; t : Thread.T) =
  BEGIN LOCK mu DO threads.addlo(t) END END RegisterThread;

PROCEDURE WaitForSomeThreads(a : T) =
  BEGIN
    LOCK mu DO
      WHILE threads.size() >= a.maxThreads DO
        WITH t = threads.remhi() DO
          EVAL Thread.Join(t)
        END
      END
    END
  END WaitForSomeThreads;
  

PROCEDURE DEval(a : Default; x : From.T) : To.T =
  BEGIN RETURN a.f(x) END DEval;

(* The following is an inefficient, but correct, implementation of
   by-reference-return evaluation.

   An efficient implementation would do the evaluation in-place in y,
   destroying y's previous contents. *)

PROCEDURE TEvalD(self : T; x : From.T; VAR y : To.T) =
  VAR
    yy := self.eval(x);
  BEGIN
    y := yy
  END TEvalD;

BEGIN END Map.

