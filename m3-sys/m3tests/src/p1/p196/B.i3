(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Mon Nov  2 10:41:57 PST 1992 by kalsow     *)

INTERFACE B;

TYPE Predicate = PROCEDURE (x: TEXT): BOOLEAN;

PROCEDURE IsOK (x: TEXT): BOOLEAN;

PROCEDURE Search (path: TEXT;  pred: Predicate := IsOK): TEXT;

END B.
