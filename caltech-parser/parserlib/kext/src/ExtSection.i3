(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id$ *)

INTERFACE ExtSection;
TYPE
  T = {Pragma, Interface, Proc, Module};
PROCEDURE GetText(kind: CHAR; i: T): TEXT;
PROCEDURE Res(name: TEXT): TEXT;
END ExtSection.
