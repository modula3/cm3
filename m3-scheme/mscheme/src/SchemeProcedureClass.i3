(* $Id$ *)

(*
  Copyright (c) 2008, Generation Capital Ltd.  All rights reserved.

  Author: Mika Nystrom <mika@alum.mit.edu>
*)

INTERFACE SchemeProcedureClass;
IMPORT SchemeProcedure;

REVEAL SchemeProcedure.T <: Private;

TYPE
  Private = SchemeProcedure.Public OBJECT
    name := DefaultName
  END;

CONST DefaultName = "anonymous procedure";

END SchemeProcedureClass.
