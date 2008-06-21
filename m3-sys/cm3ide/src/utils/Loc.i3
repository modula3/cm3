(* Copyright 1996 Critical Mass, Inc. All rights reserved.    *)

INTERFACE Loc;

IMPORT ID;

TYPE
  T = REF RECORD
    pkg    : ID.T;
    subdir : ID.T;
    next   : T;
  END;

PROCEDURE Add (pkg, subdir: ID.T): T;

END Loc.
