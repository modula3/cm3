(* Copyright (C) 1994, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* Last modified on Thu Jun 23 16:12:55 PDT 1994 by bharat *)

<* PRAGMA LL *>


INTERFACE VideoWidget;

IMPORT NodeVBT;

TYPE
  VideoNode <: NodeVBT.Widget;
 
PROCEDURE Initialize();

END VideoWidget.
