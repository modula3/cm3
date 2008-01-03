(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE AST_LAST;

(* easy identification of last interface to subtype AST.NODE *)

IMPORT AST_DisplayRep;

TYPE NODE = AST_DisplayRep.NODE;

END AST_LAST.
