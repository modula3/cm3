(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id$ *)

INTERFACE PSReaderPaneVBT;
IMPORT TextSubs;
IMPORT DCPaneVBT;

(* allow a VBTPane to be constructed from this interface *)
CONST
  Name = "Test";
  Ext = "eps";
  StartKey = 'n';

TYPE
  T <: Public;
  Public = DCPaneVBT.T OBJECT
  METHODS
    setSubs(subs: TextSubs.T);
  END;

END PSReaderPaneVBT. 
