(* Copyright (C) 1993, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* Last modified on Mon Aug 23 15:22:19 PDT 1993 by bharat *)
(* modified on Fri Jul 2 16:33:31 PDT 1993 by mhb *)
<* PRAGMA LL *>

(* All components (forms/frames/widgets) in "NodeVBT" are instances *)
(* of NodeVBT *)

INTERFACE Textual;

IMPORT NodeVBT;

TYPE
  TextEditNode <: NodeVBT.Widget;
  TextNode <: NodeVBT.Widget;
  TypeInNode <: NodeVBT.Widget;

PROCEDURE Initialize();

END Textual.













