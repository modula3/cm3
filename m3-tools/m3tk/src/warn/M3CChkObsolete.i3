(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE M3CChkObsolete;

IMPORT AST, M3AST_AS;
IMPORT ASTWalk;


TYPE
  Handle <: ASTWalk.Closure;

PROCEDURE NewHandle(cu: M3AST_AS.Compilation_Unit): Handle RAISES {};
(* Create a new handle for use with the 'Node' procedure. Since
it is a subtype of 'ASTWalk.Closure', it can be passed directly to
the tree walker.  *)

PROCEDURE Node(h: Handle; n: AST.NODE; vm: ASTWalk.VisitMode) RAISES {};
(* This procedure is intended to be called from the tree walker (in
entry and exit mode) so that it is called on every node in a section of AST. 
It checks for implicit Obsolete calls. It is pointless to call this procedure 
on nodes which are in an interface. *)

END M3CChkObsolete.
