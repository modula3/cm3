INTERFACE Watchdog;

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init(maxDelay : LONGREAL; callback : Callback := NIL) : T;

    reset();
    (* reset timer to zero *)

    pause();
    (* pause timer *)

    unpause();
    (* unpause timer *)

    kill();
    (* kill watchdog *)

    setExpireAction(callback : Callback);
    (* default exit action is to exit the program with status 1 *)
  END;

  Callback = OBJECT METHODS do() END;

CONST Brand = "Watchdog";

END Watchdog.
      
