(* Copyright 1996 Critical Mass, Inc. All rights reserved.    *)

INTERFACE Decl;

IMPORT Node, RegExpr, Source;

TYPE
  T <: Node.Named_T;

PROCEDURE FindNodes (parent: Source.T;  pattern: RegExpr.T): Node.Set;

PROCEDURE Init ();

END Decl.
