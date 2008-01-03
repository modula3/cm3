(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: MxOut.i3                                              *)
(* Last Modified On Thu Jul  7 09:49:09 PDT 1994 By kalsow     *)

INTERFACE MxOut;

IMPORT Wr, Mx;

(*------------------------------------------------------------------------*)

PROCEDURE WriteUnits (units: Mx.UnitList;  output: Wr.T);
(* write the linker info for the 'units' on 'output'.  This is
   the inverse of 'MxIn.ReadUnits' *)

END MxOut.
