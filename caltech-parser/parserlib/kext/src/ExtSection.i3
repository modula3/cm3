(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: ExtSection.i3,v 1.1.1.1 2001-09-19 11:40:10 wagner Exp $ *)

INTERFACE ExtSection;
TYPE
  T = {Pragma, Interface, Proc, Module};
PROCEDURE GetText(kind: CHAR; i: T): TEXT;
PROCEDURE Res(name: TEXT): TEXT;
END ExtSection.
