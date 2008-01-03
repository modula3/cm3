(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Thu Jan  5 21:25:04 PST 1995 by najork                   *)
(*       Created on Thu Apr 28 18:00:41 PDT 1994 by najork                   *)


INTERFACE Site;

CONST Brand = "Site";

TYPE
  T = RECORD
    uid : INTEGER;
    lab : TEXT;
    x, y: REAL;
    bool: BOOLEAN;
  END;

PROCEDURE Equal (a, b: T) : BOOLEAN;

END Site.
