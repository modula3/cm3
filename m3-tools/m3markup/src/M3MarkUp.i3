(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Dec  8 09:53:52 PST 1994 by kalsow                   *)

INTERFACE M3MarkUp;

IMPORT Buf;

TYPE
  Insertion = REF RECORD
    next   : Insertion;
    offset : INTEGER;
    insert : TEXT;
  END;

PROCEDURE Get (buf: Buf.T): Insertion;
(* generate the hypertext links needed to reflect IMPORTs and EXPORTs *)

PROCEDURE SetHrefRoot(prefix: TEXT);
(* Set the prefix for hypertext href to m3browser *)
 
END M3MarkUp.
