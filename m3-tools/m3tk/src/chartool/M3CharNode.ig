(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(* Last modified on Fri Jan 28 10:56:19 PST 1994 by detlefs   *)

GENERIC INTERFACE M3CharNode();

IMPORT AST, ASTWalk;

TYPE
  Handle <: Public;
  Public = ASTWalk.Closure OBJECT
  END;

PROCEDURE NewHandle(): Handle;
(* Creates a new handle suitable for passing to "ASTWalk.VisitNodes",
   or directly to "Node", from another walk. The "callback" method
   is set to "Node". *)

PROCEDURE Node(h: Handle; n: AST.NODE; vm: ASTWalk.VisitMode);
(* *)

END M3CharNode.
