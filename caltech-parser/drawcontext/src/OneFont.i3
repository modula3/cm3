(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: OneFont.i3,v 1.2 2001-09-19 15:30:31 wagner Exp $ *)

INTERFACE OneFont;
IMPORT Font;
TYPE
  T = Font.T;
PROCEDURE FromSize(ptSize: CARDINAL): T;
END OneFont.
