(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id$ *)

INTERFACE OneFont;
IMPORT Font;
TYPE
  T = Font.T;
PROCEDURE FromSize(ptSize: CARDINAL): T;
END OneFont.
