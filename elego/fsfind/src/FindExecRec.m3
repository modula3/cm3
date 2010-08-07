(*---------------------------------------------------------------------------*)
MODULE FindExecRec;

IMPORT Pathname;
IMPORT RegEx, FindExpr, FSFindError;

(*---------------------------------------------------------------------------*)
REVEAL 
  T = Public BRANDED "FindExecRec 0.0" OBJECT
    head : FindExpr.T;
    tail : T;
    cl   : Closure;
  OVERRIDES
    init := Init;
    append := Append;
    testAndExec := TestAndExec;
  END;

(*---------------------------------------------------------------------------*)
PROCEDURE Init(self : T; e : FindExpr.T; c: Closure) : T =
  BEGIN
    self.head := e;
    self.cl := c;
    self.tail := NIL;
    RETURN self;
  END Init;

(*---------------------------------------------------------------------------*)
PROCEDURE Append(self : T; e : T) : T =
  VAR n : T;
  BEGIN
    IF self.tail = NIL THEN
      self.tail := e;
    ELSE
      n := self.tail;
      WHILE n.tail # NIL DO
        n := n.tail;
      END;
      n.tail := e;
    END;
    RETURN self;
  END Append; 

(*---------------------------------------------------------------------------*)
PROCEDURE TestAndExec(self : T; fn : Pathname.T; all := TRUE) 
  RAISES {FSFindError.E} =
  BEGIN
    TRY
      IF self.head.test(Pathname.Last(fn)) THEN
        self.cl.apply(fn);
        IF NOT all THEN RETURN END;
      END;
      IF self.tail # NIL THEN
        TestAndExec(self.tail, fn, all);
      END;
    EXCEPT
      RegEx.Error(e) => RAISE FSFindError.E("regex error: " & e);
    END;
  END TestAndExec; 

(*---------------------------------------------------------------------------*)
PROCEDURE New(e : FindExpr.T; c: Closure) : T =
  BEGIN
    RETURN NEW(T).init(e, c);
  END New;

(*---------------------------------------------------------------------------*)
PROCEDURE Cons(e : FindExpr.T; c: Closure; tail : T) : T =
  VAR n : T;
  BEGIN
    n := NEW(T).init(e, c);
    n.tail := tail; 
    RETURN n;
  END Cons;

BEGIN
END FindExecRec. 
