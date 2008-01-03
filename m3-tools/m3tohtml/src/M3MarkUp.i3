(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Apr 12 15:12:21 PDT 1994 by kalsow                   *)

INTERFACE M3MarkUp;

IMPORT Rd;

TYPE
  Insertion = REF RECORD
    next   : Insertion;
    offset : INTEGER;
    insert : TEXT;
  END;

PROCEDURE Get (rd: Rd.T;  path: TEXT): Insertion;
(* generate the hypertext links needed to reflect IMPORTs and EXPORTs *)

END M3MarkUp.
