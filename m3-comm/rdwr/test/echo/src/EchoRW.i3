(* Copyright 1994 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

INTERFACE EchoRW;

IMPORT NetObj, Rd, Wr, Thread;

TYPE
  T = NetObj.T OBJECT METHODS 
    echo(rd: Rd.T; wr: Wr.T) RAISES {NetObj.Error, Thread.Alerted};
    msgEcho(rd: Rd.T; wr: Wr.T) RAISES {NetObj.Error, Thread.Alerted};
  END;

END EchoRW.
