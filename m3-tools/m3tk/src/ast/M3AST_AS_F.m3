(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE M3AST_AS_F;

IMPORT M3AST_AS;

PROCEDURE NotA_USED_ID(<*UNUSED*> n: NODE;
                    <*UNUSED*> VAR (*out*) used_id: M3AST_AS.USED_ID): BOOLEAN=
  BEGIN
    RETURN FALSE;
  END NotA_USED_ID;

PROCEDURE A_USED_ID(n: NODE;
                    VAR (*out*) used_id: M3AST_AS.USED_ID): BOOLEAN=
  BEGIN
    used_id := n;
    RETURN TRUE;
  END A_USED_ID;

PROCEDURE A_Exp_USED_ID(n: NODE;
                    VAR (*out*) used_id: M3AST_AS.USED_ID): BOOLEAN=
  BEGIN
    used_id := NARROW(n, M3AST_AS.Exp_used_id).vUSED_ID;
    RETURN TRUE;
  END A_Exp_USED_ID;

PROCEDURE NotA_ID(<*UNUSED*> n: NODE;
                    <*UNUSED*> VAR (*out*) id: M3AST_AS.ID): BOOLEAN=
  BEGIN
    RETURN FALSE;
  END NotA_ID;

PROCEDURE A_ID(n: NODE;
                    VAR (*out*) id: M3AST_AS.ID): BOOLEAN=
  BEGIN
    id := n;
    RETURN TRUE;
  END A_ID;

PROCEDURE A_Exp_ID(n: NODE;
                    VAR (*out*) id: M3AST_AS.ID): BOOLEAN=
  BEGIN
    id := NARROW(n, M3AST_AS.Exp_used_id).vUSED_ID;
    RETURN TRUE;
  END A_Exp_ID;

BEGIN
END M3AST_AS_F.
