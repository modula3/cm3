(* Copyright 1994 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

INTERFACE Test;

IMPORT NetObj, Rd, Wr;

TYPE
  ObjType = {Surrogate, Concrete, Unique};

  T = NetObj.T OBJECT METHODS 
    null();
    ten(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9: INTEGER); 
    objarg(arg: NetObj.T);
    rd() : Rd.T; 
    wr() : Wr.T;
    newObj(): NetObj.T;
    newReturn(type: ObjType; surr: NetObj.T): Return;
    pickle(r: REFANY);
  END;

  Return = NetObj.T OBJECT METHODS
    doIt() : NetObj.T;
  END;

END Test.
