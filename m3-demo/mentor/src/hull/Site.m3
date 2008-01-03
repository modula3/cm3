(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Thu Apr 28 18:03:17 PDT 1994 by najork                   *)
(*       Created on Thu Apr 28 18:01:31 PDT 1994 by najork                   *)


MODULE Site;

IMPORT Text;

PROCEDURE Equal (a, b: T) : BOOLEAN =
  BEGIN
    RETURN a.uid = b.uid AND a.x = b.x AND a.y = b.y AND a.bool = b.bool AND
           Text.Equal (a.lab, b.lab);
  END Equal;

BEGIN
END Site.
