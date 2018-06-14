INTERFACE Coroutine;

TYPE
  T <: REFANY;

  Arg = REF RECORD
    arg  : REFANY;
    this : T;
    co   : T;
  END;

  Entry = PROCEDURE(arg : Arg);

PROCEDURE CallPair(p : Entry; pArg : REFANY;
                   q : Entry; qArg : REFANY);
  (* this structure ensures that both p and q need to exit before
     the stack frame of CallPair itself can be reclaimed ... *)

PROCEDURE Call(to : T; from : T := NIL);
  (* call coroutine *)

CONST Brand = "Coroutine";
  
END Coroutine.
