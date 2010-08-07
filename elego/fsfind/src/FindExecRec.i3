(*---------------------------------------------------------------------------*)
INTERFACE FindExecRec;

IMPORT Pathname;
IMPORT FindExpr, FSFindError;

(*---------------------------------------------------------------------------*)
TYPE 
  Closure = OBJECT
  METHODS
    apply(fn : Pathname.T);
  END;

  T <: Public;

  Public = OBJECT
  METHODS
    init(e : FindExpr.T; c : Closure) : T;
    append(e : T) : T;
    testAndExec(fn : Pathname.T; all := TRUE) RAISES {FSFindError.E};
  END;

(*---------------------------------------------------------------------------*)
PROCEDURE New(e : FindExpr.T; c: Closure) : T;

(*---------------------------------------------------------------------------*)
PROCEDURE Cons(e : FindExpr.T; c: Closure; tail : T) : T;

END FindExecRec. 
