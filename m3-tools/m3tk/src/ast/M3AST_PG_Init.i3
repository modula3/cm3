(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE M3AST_PG_Init;


IMPORT AST, M3AST_AS_F;

PROCEDURE Interface(n: M3AST_AS_F.Interface): AST.NODE RAISES {};

PROCEDURE Interface_gen_def(n: M3AST_AS_F.Interface_gen_def
    ): AST.NODE RAISES {};

PROCEDURE DECL(n: M3AST_AS_F.DECL): AST.NODE RAISES {};


END M3AST_PG_Init.
