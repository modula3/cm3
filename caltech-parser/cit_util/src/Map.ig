(* $Id$ *)

GENERIC INTERFACE Map(From,To);
IMPORT Word;
IMPORT Thread;
IMPORT MapError;

(* A Map.T is a wrapper for a 
   PROCEDURE (x : From.T) : To.T;

   It allows such functions to be dynamic (i.e., have internal state).
   A simple example would be a function that interpolates an array of
   values provided at runtime.
 *)

TYPE

  T <: Public;

  Public = OBJECT 
    doHints := FALSE; (* should the hint routine be called 
                         before evaluating? *)
    hintsByForking := FALSE; (* should a hint be implemented by forking
                                and evaluating, throwing away the result? *)
    maxThreads : CARDINAL := 10; (* max. no of threads in case we do hints
                                    by forking. *)
  METHODS
    eval(x : From.T) : To.T RAISES { MapError.E }; (* must override this *)

    evalD(x : From.T; VAR y : To.T) RAISES { MapError.E }; 
    (* may override this if desired;
       inefficient default impl. is
       provided *)

    (* evalHint provides a rudimentary way to parallelize the evaluation
       of slow functions.
       
       A client may call evalHint to indicate that it intends to evaluate
       the map in the future.  An implementation may use this information
       to evaluate the map early and memoize it, so that it knows the
       answer when the eval call comes.  The eval call is not guaranteed
       to occur, so the implementation should be careful not to store
       too much information. 
       
       The default implementation of evalHint is a No-op unless hintsByForking
       is TRUE, in which case evalHint is a forked eval whose result is
       discarded.
    *)
    evalHint(x : From.T);

    (* if you override hint, you probably want to generate extra threads
       of your own.  Instead of waiting for them yourself, you can 
       simply add them to the internal threads queue. *)
    registerThread(t : Thread.T);

    (* to avoid generating too many threads, call waitForSomeThreads
       before spawning new ones. *)
    waitForSomeThreads();

    hash() : Word.T; (* may override this if desired *)
  END;

  (* sometimes all you need is a simple thing;
     in that case, you can use
     
     ft := NEW(Default).wrap(F)
  *)
  F = PROCEDURE (x : From.T) : To.T;

  Default <: T OBJECT METHODS
    wrap(f : F) : T;  
  END;

  Result = To.T;

  Argument = From.T;

CONST Brand = "Map from " & From.Brand & " to " & To.Brand;

PROCEDURE Hash(a : T) : Word.T; (* calls hash method *)

PROCEDURE Equal(a, b : T) : BOOLEAN;

END Map.
