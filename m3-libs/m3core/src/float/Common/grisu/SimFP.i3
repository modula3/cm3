(* Copyright (C) 2016 Peter McKinna. All rights reserved. *)
(* See file COPYRIGHT-BSD for details. *)

INTERFACE SimFP;

IMPORT Long;
FROM Ctypes IMPORT int;

CONST
  SignificandSize = 64;

TYPE
  Uint64 = Long.T;
  Uint32 = int;

  (* A simulated floating point object GFP - Grisu FP *)
  GFP <: Public;
  
  Public = OBJECT
  METHODS
    init(sig : Uint64; exp : INTEGER) : GFP;
    f() : Uint64;
    e() : INTEGER;
    setF(val : Uint64);
    setE(val : INTEGER);
    minus(sub : GFP) : GFP;
    times(mul : GFP) : GFP;
    normalize() : GFP;
  END;
  
END SimFP.