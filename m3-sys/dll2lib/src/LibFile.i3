(* Copyright 1996-2000 Critical Mass, Inc. All rights reserved.    *)
(* See file COPYRIGHT-CMASS for details. *)

INTERFACE LibFile;

TYPE
  Obj = REF RECORD
    next     : Obj;
    filename : TEXT;
    time     : INTEGER;
    body     : TEXT;
    symbols  : REF ARRAY OF TEXT;
  END;

PROCEDURE Gen (nm: TEXT;  contents: Obj);
(* Create a .LIB file named "nm" containing the object files "contents". *)

END LibFile.
