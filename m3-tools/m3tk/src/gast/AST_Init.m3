(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE AST_Init;

IMPORT AST;

PROCEDURE Null(n: NODE): AST.NODE RAISES {}=
  BEGIN 
    RETURN n;
  END Null;

BEGIN

END AST_Init.
