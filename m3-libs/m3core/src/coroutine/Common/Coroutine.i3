INTERFACE Coroutine;

TYPE
  T <: ROOT;
  
  Closure = OBJECT METHODS
    apply(from : T) : REFANY;
    (* when apply is called, it will be passed the handle of the coroutine
       calling it, which can be used in Call later *)
  END;

PROCEDURE Create(cl : Closure) : T;

PROCEDURE Call(t : T) : T;
  (* when it returns, returns the handle of the coroutine it was called from *)

PROCEDURE Retval(t : T) : REFANY;
  (* returns NIL if t is still active, returns return value of apply if 
     t is done executing *)

END Coroutine.

  
