(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id$ *)

INTERFACE InitGen;

PROCEDURE Get(form, src: TEXT): TEXT;
(* src = Modula-3 record field declarations
   form = e.g. "result.%name := %val;\n" 
   returns: for each initialized variable, substitute %name and %val *)

END InitGen.
