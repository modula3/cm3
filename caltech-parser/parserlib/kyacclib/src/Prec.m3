(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: Prec.m3,v 1.1.1.1 2001-09-19 11:40:10 wagner Exp $ *)

MODULE Prec;
IMPORT Fmt;
PROCEDURE Format(a: T): TEXT =
  CONST
    k = ARRAY Kind OF TEXT{"Left","Right","None"};
  BEGIN
    RETURN k[a.kind] & Fmt.Int(a.val);
  END Format;
BEGIN
END Prec.
