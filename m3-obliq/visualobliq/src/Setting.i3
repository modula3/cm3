(* Copyright (C) 1993, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* Last modified on Tue Aug 24 09:55:00 PDT 1993 by bharat *)
(* modified on Fri Jul 2 16:33:31 PDT 1993 by mhb *)
<* PRAGMA LL *>

(* All components (forms/frames/widgets) in "NodeVBT" are instances *)
(* of NodeVBT *)

INTERFACE Setting;

IMPORT NodeVBT;

TYPE
  NumericNode <: NodeVBT.Widget;
  ScrollerNode <: NodeVBT.Widget;
  HScrollerNode <: ScrollerNode;
  VScrollerNode <: ScrollerNode;


PROCEDURE Initialize();

END Setting.













