(* Copyright (C) 1992, Digital Equipment Corporation                     *)
(* All rights reserved.                                                  *)
(* See the file COPYRIGHT for a full description.                        *)
(*                                                                       *)
(* Last modified on Thu Sep 10 20:48:51 PDT 1992 by mhb                  *)

(* This interface is part of the "m3bundle" facility. See the manpage 
   for "m3bundle" for details. *)

INTERFACE Bundle;

TYPE T <: REFANY;

PROCEDURE Get (bundle: T; element: TEXT): TEXT;
(* If an element namemd "element" was bundled into "bundle", then
   return the contents of "element" as a "TEXT".  Otherwise, 
   return NIL. *)

(********** UNIMPLEMENTED **********)
PROCEDURE Elts (bundle: T): REF ARRAY OF TEXT;
(* Returns a list of elements that are bundled into "bundle". *)

END Bundle.
