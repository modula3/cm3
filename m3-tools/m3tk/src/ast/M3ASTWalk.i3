(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE M3ASTWalk;

(* This is just a pass through to ASTWalk, to cater for the old days.
   See ASTWalk for documentation, and use it instead. *)

IMPORT AST, ASTWalk;

EXCEPTION Aborted;    (* = ASTWalk.Aborted *)

TYPE
  VisitMode = ASTWalk.VisitMode;
  VisitModeControl = ASTWalk.VisitModeControl;
  Closure = ASTWalk.Closure;
  NodeCallbackProc = ASTWalk.NodeCallbackProc;

CONST 
  OnEntry = ASTWalk.OnEntry;
  OnExit = ASTWalk.OnExit;
  OnEntryAndExit = ASTWalk.OnEntryAndExit;

PROCEDURE VisitNodes(n: AST.NODE; vc: Closure) RAISES ANY;
PROCEDURE ModeVisitNodes(n: AST.NODE; vc: Closure; 
    vm: VisitModeControl) RAISES ANY;
PROCEDURE NodeProcClosure(p: NodeCallbackProc): Closure RAISES {};
PROCEDURE IgnoreChildren(vc: Closure) RAISES {};
PROCEDURE Abort() RAISES {ASTWalk.Aborted};
(* caught and converted to our Aborted by Visit/ModeVisitNodes *)
  
END M3ASTWalk.
