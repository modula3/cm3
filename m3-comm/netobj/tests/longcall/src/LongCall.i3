(* Copyright 1994 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

INTERFACE LongCall;

IMPORT NetObj, Thread;

TYPE
  T = NetObj.T OBJECT METHODS 
    wait() RAISES {NetObj.Error, Thread.Alerted};
  END;

END LongCall.
