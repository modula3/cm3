(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id$ *)

GENERIC INTERFACE VBTPane(SubPaneVBT);
(* a VBTPane is a pane which simply displays a PaneVBT. 
   most document panes will be VBTPanes. *)
IMPORT Pane;
CONST
  Name = SubPaneVBT.Name;
  Ext = SubPaneVBT.Ext;
  StartKey = SubPaneVBT.StartKey;
TYPE
  T <: Pane.T;
END VBTPane. 
