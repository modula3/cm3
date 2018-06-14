INTERFACE Coroutine;

TYPE
  T <: ROOT;
  
  Closure = OBJECT METHODS
    apply(from : T) : REFANY;
  END;

PROCEDURE Create(cl : Closure) : T;

PROCEDURE Call(t : T) : T;
  (* when it returns, returns the handle of the coroutine it was called from *)

PROCEDURE IsAlive(t : T) : BOOLEAN;
  
END Coroutine.

  
