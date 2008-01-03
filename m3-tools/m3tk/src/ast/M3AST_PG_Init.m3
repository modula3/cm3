(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE M3AST_PG_Init;

IMPORT AST, M3AST_AS_F, M3AST_PG;


PROCEDURE Interface(n: M3AST_AS_F.Interface): AST.NODE RAISES {}=
  BEGIN
    n.vEXTERNAL_DECL := NEW(M3AST_PG.EXTERNAL_DECL).init();
    RETURN n;
  END Interface;


PROCEDURE Interface_gen_def(n: M3AST_AS_F.Interface_gen_def
    ): AST.NODE RAISES {}=
  BEGIN
    n.vEXTERNAL_DECL := NEW(M3AST_PG.EXTERNAL_DECL).init();
    RETURN n;
  END Interface_gen_def;


PROCEDURE DECL(n: M3AST_AS_F.DECL): AST.NODE RAISES {}=
  BEGIN
    n.vEXTERNAL_DECL := NEW(M3AST_PG.EXTERNAL_DECL).init();
    RETURN n;
  END DECL;

BEGIN
END M3AST_PG_Init.
