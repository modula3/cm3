(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Fri May  6 14:38:14 PDT 1994 by kalsow     *)

INTERFACE RTPacking;

TYPE
  T = RECORD
    word_size     : CARDINAL;  (* 8, 16, 32, or 64 *)
    long_size     : CARDINAL;  (* 8, 16, 32, or 64 *)
    (* Compatibility note:  
         If an encoded value was written by an earlier version of this module,
         (that does not support LONGINT,) and then decoded by the current
         version, the encoded value will have no field for long_size.  
         This will decode perversely into bit size 8 for long_size.
         Although ugly, it is a safe bet that this is not a reasonable size 
         for LONGINT, so it could be tested for explicitly and interpreted
         as meaning "LONGINT doesn't exist". 
         As of 2008-1-26, the only uses of this module in the entire cm3
         distribution are in Pickle2, and there, the value of long_size
         would not be accessed unless the pickle was written by a program
         compiled by a cm3 that has LONGINT.
    *) 
    max_align     : CARDINAL;  (* 8, 16, 32, or 64 *)
    struct_align  : CARDINAL;  (* 8, 16, 32, or 64 *)
    float         : FloatKind;
    little_endian : BOOLEAN;
    lazy_align    : BOOLEAN;
    widechar_size : CARDINAL   (* 16 or 32 *) 
  END;

TYPE
  FloatKind = { IEEE, VAX, other };

PROCEDURE Local (): T;
(* Return the packing rules for the host machine *)

PROCEDURE Encode (READONLY t: T): INTEGER;
PROCEDURE Decode (i: INTEGER): T;
(* convert between the packed and unpacked representations of a 'T' *)

END RTPacking.


