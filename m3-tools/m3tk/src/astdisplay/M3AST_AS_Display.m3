(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE M3AST_AS_Display;

IMPORT AST_DisplayRep;
IMPORT M3AST_LX, M3AST_AS_F;
IMPORT SeqM3AST_LX_SRC_NODE;

IMPORT Wr;
IMPORT M3CId, M3CLiteral, M3CToken, M3CWhitespace;
IMPORT M3ASTDisplay_handle;



PROCEDURE SRC_NODE_C(n: M3AST_AS_F.SRC_NODE_C; h: AST_DisplayRep.Handle)
    RAISES ANY=
  VAR iter := SeqM3AST_LX_SRC_NODE.NewIter(n.lx_node_s);
    src_node: M3AST_LX.SRC_NODE;
  BEGIN
    WHILE SeqM3AST_LX_SRC_NODE.Next(iter, src_node) DO
      h.Visit(src_node);
    END;
  END SRC_NODE_C;


PROCEDURE ID(n: M3AST_AS_F.ID; h: AST_DisplayRep.Handle) RAISES ANY=
  BEGIN
    Wr.PutText(h.stream, M3CId.ToText(n.lx_symrep));
  END ID;


PROCEDURE LITERAL(n: M3AST_AS_F.LITERAL; h: AST_DisplayRep.Handle) RAISES ANY
    =
  BEGIN
    Wr.PutText(h.stream, M3CLiteral.ToText(n.lx_litrep));
  END LITERAL;


PROCEDURE Whitespace(n: M3AST_AS_F.Whitespace; h: AST_DisplayRep.Handle)
    RAISES ANY=
  BEGIN
    Wr.PutText(h.stream, M3CWhitespace.ToText(n.lx_whitespace_rep));
  END Whitespace;


PROCEDURE Comment(<*UNUSED*> n: M3AST_AS_F.Comment;
                  <*UNUSED*> h: AST_DisplayRep.Handle) =
  BEGIN
  END Comment;


PROCEDURE Pragma(<*UNUSED*> n: M3AST_AS_F.Pragma;
                 <*UNUSED*> h: AST_DisplayRep.Handle) =
  BEGIN
  END Pragma;


PROCEDURE BadChar(<*UNUSED*> n: M3AST_AS_F.BadChar;
                  <*UNUSED*>  h: AST_DisplayRep.Handle) =
  BEGIN
  END BadChar;


PROCEDURE Token(n: M3AST_AS_F.Token; h: AST_DisplayRep.Handle) RAISES ANY=
  BEGIN
    Wr.PutText(h.stream, M3CToken.Token_repToText(n.lx_token_rep));
  END Token;

BEGIN
END M3AST_AS_Display.
