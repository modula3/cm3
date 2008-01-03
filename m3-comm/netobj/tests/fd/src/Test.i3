(* Copyright 1994 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

INTERFACE Test;

IMPORT NetObj, Thread;

TYPE
  T = NetObj.T OBJECT METHODS 
    null() RAISES {NetObj.Error, Thread.Alerted};
    callback(w: Waiter) RAISES {NetObj.Error, Thread.Alerted};
  END;

  Waiter = NetObj.T OBJECT METHODS
    wait() RAISES {NetObj.Error, Thread.Alerted};
  END;

END Test.
