(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* "AST_Init" supports the dynamic initialisation of an AST node. *)

INTERFACE AST_Init;

IMPORT AST;

TYPE
  NODE = OBJECT
    METHODS
      init(): AST.NODE := Null;
  END;

REVEAL
  AST.NODE <: NODE;

PROCEDURE Null(n: NODE): AST.NODE;
(* Has no side effects and returns "n". *)

END AST_Init.

(* Occasionally a particular type of AST node requires dynamic
initialisation, and this can be achieved overriding the "init" method
defined here. The default value is simply a no-op.  Therefore the
conventional way to create an instance of an AST node, "SomeNode", is
"NEW(SomeNode).init()". Note that, since this "init" method returns
the type "AST.NODE" rather than "SomeNode", the layer in which the
latter is defined might choose to redefine the "init" method to return
"SomeNode", thus allowing declarations of the form:

| VAR n := NEW(SomeNode).init();
*)

