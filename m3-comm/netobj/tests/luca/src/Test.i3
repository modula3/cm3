(* Copyright 1994 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

INTERFACE Test;

IMPORT NetObj, OSError, Rd;

TYPE
  RdPkl = REF RECORD rd: Rd.T; END;

TYPE
  T = NetObj.T OBJECT METHODS 
    getRd(path: TEXT): Rd.T RAISES {NetObj.Error, OSError.E};
    getRdEmbedded(path: TEXT): RdPkl RAISES {NetObj.Error, OSError.E};
  END;

END Test.
