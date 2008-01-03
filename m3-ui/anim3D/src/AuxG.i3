(* Copyright (C) 1993, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Created by Marc Najork                                                    *)
(* Last modified on Thu Feb  2 01:17:17 PST 1995 by najork                   *)


INTERFACE AuxG;

IMPORT Point3;

PROCEDURE GetUnitCirclePoints (prec : INTEGER) : REF ARRAY OF Point3.T;

END AuxG.
