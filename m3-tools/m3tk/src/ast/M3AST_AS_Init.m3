(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE M3AST_AS_Init;

IMPORT AST, M3AST_AS, M3AST_AS_F;

PROCEDURE Exp_used_id(n: M3AST_AS_F.Exp_used_id): AST.NODE RAISES {}=
  BEGIN
    n.vUSED_ID := NEW(M3AST_AS.USED_ID).init();
    RETURN n;
  END Exp_used_id;

BEGIN
END M3AST_AS_Init.
