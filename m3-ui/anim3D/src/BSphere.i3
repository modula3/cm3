(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Tue Mar 15 11:04:19 PST 1994 by najork                   *)
(*       Created on Tue Mar 15 10:59:53 PST 1994 by najork                   *)


INTERFACE BSphere;

IMPORT Point3;

TYPE
  T = RECORD
    center : Point3.T;
    radius : REAL;
  END;

END BSphere.
