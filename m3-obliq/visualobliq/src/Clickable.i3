(* Copyright (C) 1993, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* Last modified on Fri Aug 27 17:45:40 PDT 1993 by bharat *)
(* modified on Fri Jul 2 16:33:31 PDT 1993 by mhb *)
<* PRAGMA LL *>

(* All components (forms/frames/widgets) in "NodeVBT" are instances *)
(* of NodeVBT *)

INTERFACE Clickable;

IMPORT NodeVBT;

TYPE
  ButtonNode <: NodeVBT.Widget;
  BooleanNode <: NodeVBT.Widget;
  ChoiceNode <: NodeVBT.Widget;
  
PROCEDURE Initialize();
  
END Clickable.













