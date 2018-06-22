INTERFACE Coroutine;

TYPE
  T <: ROOT;
  
  Closure = OBJECT METHODS
    apply(from : T) : REFANY;
    (* 
       when apply is called, it will be passed the handle of the coroutine
       calling it, which can be used in Call later.   The initial activation
       point of the created coroutine will be at the start of the apply method
    *)
  END;

PROCEDURE Create(cl : Closure) : T;

PROCEDURE Call(c : T) : T;
  (* 
     call the coroutine c at its last activation point and suspend the
     current coroutine at its current activation point

     when Call returns, returns the handle of the coroutine it was
     called from

     if the called coroutine returns normally (using RETURN), the last
     coroutine to call c will be activated at its last activation
     point

   *)

PROCEDURE Retval(c : T) : REFANY;
  (* returns NIL if coroutine c is still active, returns return value
     of apply if c is done executing *)


  (* Example:

     VAR
       c := Create(NEW(Closure, apply := Apply));
     BEGIN
       IO.Put("hello");
       Call(c);
       IO.Put("world");
       Call(c);
       IO.Put(Retval(c));
     END;

     PROCEDURE Apply(cl : Closure; from : T) : REFANY =
       BEGIN
         IO.Put(", ");
         Call(from);
         IO.Put("!\n");
         RETURN "and goodbye!\n";
       END Apply;

      will print the text

      hello, world!
      and goodbye!

      on the terminal
  *)

END Coroutine.

  
