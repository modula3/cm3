(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* "AST_Name" defines a method that returns a print-name for an AST node. *)
INTERFACE AST_Name;

IMPORT AST;
IMPORT AST_Init AS Previous_View;

TYPE
  NODE = Previous_View.NODE OBJECT
    METHODS
      name(): TEXT RAISES {} := Null;
  END;

REVEAL
  AST.NODE <: NODE;

PROCEDURE Null(n: NODE): TEXT RAISES {};
(* Returns a text for a node without a specific "name" method,
   of the form:
|  "no name for node with typecode " & 
|       Fmt.Int(TYPECODE(n)) 
*)

END AST_Name.

(* It is occasionally useful to have access to a name for an AST node,
in the form of a "TEXT". This view provides such a method, with a
suitable default. *)

