
INTERFACE IntervalTimer;

(* A client can use the "IntervalTimer" interface to
   recieve periodic wake-up calls. *)

IMPORT Time;

TYPE
  T <: Public;
  Public = Private OBJECT METHODS 
    init (duration: Time.T): T;
    wakeup() := NIL; (* must be overridden. *)
    shutdown();
  END;
  Private <: ROOT;

(* The "init" call initializes a timer with the specified "duration".
   Each client must override the "wakeup" call to perform the 
   periodic task. When finished, clients must call "shutdown" in
   order to stop the interval timer from waking up periodically. *)

END IntervalTimer.

