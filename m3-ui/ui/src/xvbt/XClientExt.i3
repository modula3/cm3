(* Copyright (C) 1993, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* Last modified on Thu Nov 11 10:44:21 PST 1993 by kalsow   *)

UNSAFE INTERFACE XClientExt;

(* the interface isolates all the extension-dependant fields in an
   XClient.T definition. XClientF.T_Ext must <: XClientF.T_Rel *)

IMPORT XClientF, XSharedMem;

REVEAL XClientF.T_Ext = XSharedMem.XClient_T BRANDED OBJECT END;

END XClientExt.
