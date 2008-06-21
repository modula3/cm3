(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Dec  8 09:53:52 PST 1994 by kalsow                   *)

INTERFACE M3MarkUp;

IMPORT Buf, Marker;

PROCEDURE Get (buf: Buf.T;  target: TEXT): Marker.CharInsertion;
(* generate the hypertext links needed to reflect IMPORTs and EXPORTs.
   If "target" is non-NIL, generate a "ThisDecl" anchor at the symbol's
   definition. *)

CONST (* special arcs *)
  Intf_to_Impl_Mark = "_EXPORTERS_";
  Impl_to_Intf_Mark = "_EXPORTS_";

CONST (* special anchors *)
  ThisDecl = "_DECL_";

END M3MarkUp.
