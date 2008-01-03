(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE M3CharExprsToConsider;

IMPORT AST, ASTWalk;

TYPE
  Handle <: Public;
  Public = ASTWalk.Closure OBJECT
  END;

PROCEDURE NewHandle(consider, distant: BOOLEAN): Handle;
(* Creates a new handle suitable for passing to "ASTWalk.VisitNodes",
   or directly to "Node", from another walk. The "callback" method
   is set to "Node".
   The two arguments control which kinds of code fragments are
   flagged; at least one must be TRUE.
   *)

PROCEDURE Node(h: Handle; n: AST.NODE; vm: ASTWalk.VisitMode);
(* *)

END M3CharExprsToConsider.
