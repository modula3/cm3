(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE M3CPragmaF;
IMPORT M3AST_AS, M3CPragma, M3CSrcPos;

REVEAL
  M3CPragma.T = BRANDED OBJECT
    prev, next:  M3CPragma.Iter := NIL;
    pos: M3CSrcPos.T;
    body: TEXT;
    precedingNode, followingNode: M3AST_AS.SRC_NODE := NIL;
    precedingStmOrDecl: M3AST_AS.SRC_NODE := NIL;
    hook: REFANY := NIL;
  END;

  M3CPragma.Iter = M3CPragma.T BRANDED OBJECT END;

  M3CPragma.Store = BRANDED REF RECORD
    first, last: M3CPragma.Iter := NIL;
  END;

END M3CPragmaF.
