(* Copyright 1994 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

INTERFACE Test;

IMPORT NetObj;

TYPE
  T = NetObj.T OBJECT METHODS 
    swap(r: REFANY): REFANY RAISES {NetObj.Error};
  END;

END Test.
