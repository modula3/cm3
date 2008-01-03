(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: TestVBT.i3,v 1.2 2001-09-19 15:30:31 wagner Exp $ *)

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
