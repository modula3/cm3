(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Fri May  6 14:38:14 PDT 1994 by kalsow     *)

INTERFACE RTPacking;

TYPE
  T = RECORD
    word_size     : CARDINAL;  (* 8, 16, 32, or 64 *)
    max_align     : CARDINAL;  (* 8, 16, 32, or 64 *)
    struct_align  : CARDINAL;  (* 8, 16, 32, or 64 *)
    float         : FloatKind;
    little_endian : BOOLEAN;
  END;

TYPE
  FloatKind = { IEEE, VAX, other };

PROCEDURE Local (): T;
(* Return the packing rules for the host machine *)

PROCEDURE Encode (READONLY t: T): INTEGER;
PROCEDURE Decode (i: INTEGER): T;
(* convert between the packed and unpacked representations of a 'T' *)

END RTPacking.


