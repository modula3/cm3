(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: TextPane.i3,v 1.1.1.1 2001-09-19 11:40:10 wagner Exp $ *)

INTERFACE TextPane;
IMPORT Pane;
CONST
  Name = "Text";
  Ext = "";
  StartKey = 't';
TYPE
  T <: Public;
  Public = Pane.T OBJECT
  METHODS
    setText(t: TEXT);
    getText(): TEXT;
  END;
END TextPane.
