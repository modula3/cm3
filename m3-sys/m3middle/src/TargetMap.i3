(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: TargetMap.i3                                          *)
(* Last Modified On Fri Nov 19 09:33:45 PST 1993 By kalsow     *)
(*      Modified On Mon Mar 11 22:16:28 1991 By muller         *)

INTERFACE TargetMap;

(*  Modula-3 target description

    This interface defines some useful arrays that map over
    target's types.
*)

FROM Target IMPORT CGType, Precision, Int_type, Float_type;

VAR(* CONST *)
  CG_Align       : ARRAY CGType OF CARDINAL;
  CG_Align_bytes : ARRAY CGType OF CARDINAL;
  CG_Size        : ARRAY CGType OF CARDINAL;
  CG_Bytes       : ARRAY CGType OF CARDINAL;
  Word_types     : ARRAY [0..3] OF Int_type;
  Integer_types  : ARRAY [0..3] OF Int_type;
  Float_types    : ARRAY Precision OF Float_type;

PROCEDURE Init();
(* initializes the maps from the values exported by Target *)

END TargetMap.
