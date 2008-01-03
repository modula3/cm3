(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Mon Feb  7 20:16:25 PST 1994 by najork                   *)
(*       Created on Mon Feb  7 20:12:10 PST 1994 by najork                   *)


MODULE Mth;

IMPORT Math;

PROCEDURE sin (x : REAL) : REAL =
  BEGIN
    RETURN FLOAT (Math.sin (FLOAT (x, LONGREAL)));
  END sin;

PROCEDURE cos (x : REAL) : REAL =
  BEGIN
    RETURN FLOAT (Math.cos (FLOAT (x, LONGREAL)));
  END cos;

PROCEDURE asin (x : REAL) : REAL =
  BEGIN
    RETURN FLOAT (Math.asin (FLOAT (x, LONGREAL)));
  END asin;

PROCEDURE acos (x : REAL) : REAL =
  BEGIN
    RETURN FLOAT (Math.acos (FLOAT (x, LONGREAL)));
  END acos;

PROCEDURE sqrt (x : REAL) : REAL =
  BEGIN
    RETURN FLOAT (Math.sqrt (FLOAT (x, LONGREAL)));
  END sqrt;


BEGIN
END Mth.

