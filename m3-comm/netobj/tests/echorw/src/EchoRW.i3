(* Copyright 1994 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

INTERFACE EchoRW;

IMPORT NetObj, Rd, Wr;

TYPE
  T = NetObj.T OBJECT METHODS 
    echo(rd: Rd.T; wr: Wr.T);
  END;

END EchoRW.
