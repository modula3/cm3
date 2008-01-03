(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE AST_Iter;

IMPORT AST;

TYPE Null_T = T OBJECT OVERRIDES next := NullNext END;

PROCEDURE Null(<*UNUSED*> n: NODE): T RAISES {}=
  BEGIN
    RETURN NEW(Null_T);
  END Null;

PROCEDURE NullNext(
    <*UNUSED*> iter: T;
    <*UNUSED*> VAR (*out*) n: AST.NODE
    ): BOOLEAN RAISES {}=
  BEGIN
    RETURN FALSE;
  END NullNext;

(***************************
EXCEPTION UpdateNotPossible;

PROCEDURE NullUpdate(
    <*UNUSED*> n: NODE;
    <*UNUSED*> iter: T;
    <*UNUSED*> nn: AST.NODE
    ) RAISES {}=
  <*FATAL UpdateNodePossible*>
  BEGIN
    RAISE UpdateNotPossible;
  END NullUpdate;
*******************************)

BEGIN
END AST_Iter.
