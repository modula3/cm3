(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id$ *)

INTERFACE TestVBT;
IMPORT PaneVBT;

(* allow a VBTPane to be constructed from this interface *)
CONST
  Name = "Test";
  Ext = "";
  StartKey = 'n';

TYPE
  T <: PaneVBT.T;

END TestVBT. 
