(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE M3AST_LX_F;

IMPORT M3AST, M3AST_LX; 
IMPORT SeqM3AST_LX_SRC_NODE;

TYPE
  SRC_NODE = M3AST.NODE OBJECT
    lx_srcpos: M3AST_LX.SrcPos := 0;
  END;

  SRC_NODE_C = M3AST_LX.SRC_NODE OBJECT
    lx_node_s := SeqM3AST_LX_SRC_NODE.Null;
  END;

  ID = M3AST_LX.SRC_NODE OBJECT
    lx_symrep: M3AST_LX.Symbol_rep := NIL;
  END;

(* To reduce space requirements "M3AST_AS" reveals a LITERAL to be 
a subtype of "EXP", so we defer the exposure of the "lx" component
until then.

  LITERAL = M3AST_LX.SRC_NODE OBJECT
    lx_litrep: M3AST_LX.Literal_rep := NIL;
  END;
*)

  Whitespace = M3AST_LX.SRC_NODE OBJECT
    lx_whitespace_rep: M3AST_LX.Whitespace_rep := NIL;
  END;

  Comment = M3AST_LX.SRC_NODE OBJECT
    lx_comment_rep: M3AST_LX.Comment_rep := NIL;
  END;

  Pragma = M3AST_LX.SRC_NODE OBJECT
    lx_pragma_rep: M3AST_LX.Pragma_rep := NIL;
  END;

  BadChar = M3AST_LX.SRC_NODE OBJECT
   lx_badchar_rep: M3AST_LX.BadChar_rep := NIL;
  END;

  Token = M3AST_LX.SRC_NODE OBJECT
    lx_token_rep: M3AST_LX.Token_rep := NIL;
  END;  

REVEAL
  M3AST_LX.SRC_NODE <: SRC_NODE;
  M3AST_LX.SRC_NODE_C <: SRC_NODE_C;
  M3AST_LX.ID <: ID;
(*M3AST_LX.LITERAL <: LITERAL;*)
  M3AST_LX.Whitespace <: Whitespace;
  M3AST_LX.Comment <: Comment;
  M3AST_LX.Pragma <: Pragma;
  M3AST_LX.BadChar <: BadChar;
  M3AST_LX.Token <: Token;

END M3AST_LX_F.

  
  
