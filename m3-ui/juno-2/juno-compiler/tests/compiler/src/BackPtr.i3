(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Jul 19 16:36:09 PDT 1993 by heydon                   *)
(*      modified on Tue Jul 16 16:23:33 PDT 1993 by lin                      *)

INTERFACE BackPtr;

IMPORT JunoAST, Wr;

PROCEDURE ShowAll(ast: JunoAST.T; wr: Wr.T);
(* The predecessor of an AST node "n" is the node with a self-backpointer
   reached by successively following the backpointers from "n".  If one of the
   backpointers along the chain is NIL, then the predecessor of "n" is
   undefined.

   For each node "n" in the tree "ast", unparse "n" and its predecessor to
   "wr" if that predecessor is defined and if it is different from "n". *)

END BackPtr.
