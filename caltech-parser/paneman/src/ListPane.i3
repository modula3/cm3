(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: ListPane.i3,v 1.1.1.1 2001-09-19 11:40:10 wagner Exp $ *)

INTERFACE ListPane;
IMPORT Pane;
CONST
  Name = "Panes List";
  Ext = "";
  StartKey = '\000';
TYPE
  T = Pane.T;
END ListPane.
