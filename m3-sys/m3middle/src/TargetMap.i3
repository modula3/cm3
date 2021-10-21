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

  (* CG_Align, CG_Align_bytes, CG_Size, and CG_Bytes each extract a
  column from the type definition table in Target.  They provide a
  different view on the same data. *)

  CG_Align       : ARRAY CGType OF CARDINAL;
  (* Minimum type alignment in bits. *)

  CG_Align_bytes : ARRAY CGType OF CARDINAL;
  (* Minimum type alignment in 8-bit bytes. *)

  CG_Size        : ARRAY CGType OF CARDINAL;
  (* Minimum type size in bits. *)

  CG_Bytes       : ARRAY CGType OF CARDINAL;
  (* Minimum type size in 8-bit bytes. *)


  (* Word_types, Integer_types, and Float_types select subsets from
  the rows of the same type definition table. *)

  Word_types     : ARRAY [0..3] OF Int_type;
  (* Lists the 4 primitive unsigned integer types: 8-, 16-, 32-, and 64-bit. *)

  Integer_types  : ARRAY [0..3] OF Int_type;
  (* Lists the 4 primitive signed integer types: 8-, 16-, 32-, and 64-bit. *)

  Float_types    : ARRAY Precision OF Float_type;
  (* List the 3 primitive floating-point types: short, long, and extended. *)


PROCEDURE Init();
(* Initializes the maps from the values exported by Target

This is called from Target.Init to resolve initialization order
dependencies between the two modules. *)

END TargetMap.
