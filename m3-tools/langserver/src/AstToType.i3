(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE AstToType;

IMPORT M3Context;
IMPORT Type;
IMPORT M3AST_AS;

TYPE
  Handle <: Public;
  Public = OBJECT
    context: M3Context.T;
  END;

PROCEDURE NewHandle(c: M3Context.T): Handle;
(* Creates a new handle suitable for passing to "ASTWalk.VisitNodes",
   or directly to "Node", from another walk. The "callback" method
   is set to "Node". *)
 
PROCEDURE ProcessM3Type (h: Handle; m3type: M3AST_AS.M3TYPE): Type.T;

END AstToType.
