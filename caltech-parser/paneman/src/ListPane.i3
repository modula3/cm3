(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: ListPane.i3,v 1.2 2001-09-19 14:22:13 wagner Exp $ *)

INTERFACE ListPane;
IMPORT Pane;
CONST
  Name = "Panes List";
  Ext = "";
  StartKey = '\000';
TYPE
  T = Pane.T;
END ListPane.
