(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Mon Nov  2 10:44:39 PST 1992 by kalsow     *)

MODULE B;

PROCEDURE IsOK (<*UNUSED*> x: TEXT): BOOLEAN =
  BEGIN RETURN TRUE END IsOK;

PROCEDURE Search (<*UNUSED*> path: TEXT;
                  <*UNUSED*> pred: Predicate := IsOK): TEXT =
  BEGIN RETURN NIL END Search;

BEGIN
END B.
