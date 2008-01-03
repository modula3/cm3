(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE M3AST_AS_Iter_rep;

IMPORT AST_Iter;

REVEAL
  AST_Iter.T = AST_Iter.T_public BRANDED OBJECT
    slot: CARDINAL := 0;
  END;

END M3AST_AS_Iter_rep.
