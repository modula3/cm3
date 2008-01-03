(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Mon Jun 20 10:57:39 PDT 1994 by kalsow     *)

INTERFACE RTTypeMap;

(* An RTTypeMap.T, type map, defines the layout of runtime values. Type
   maps are provided by the compiler.

   A type map is a pointer to a byte stream that defines a program
   for a simple virtual machine.  By executing that program each
   scalar field of a runtime value can be located.  See RTMapOp for
   a detailed description of the virtual instructions.
*)

TYPE
  T    = ADDRESS;
  Mask = SET OF Kind;
  Kind = {
    Ref, UntracedRef, Proc,         (* traced ref, untraced ref, procedure *)
    Real, Longreal, Extended,       (* floating point value *)
    Int_1, Int_2, Int_4, Int_8,     (* 1, 2, 4, or 8 byte signed integer *)
    Word_1, Word_2, Word_4, Word_8, (* 1, 2, 4, or 8 byte unsigned integer *)
    Int_Field, Word_Field,          (* signed or unsigned bit field *)
    Set                             (* bit set *)
  };

TYPE Visitor <: V_;
     V_ = OBJECT METHODS apply (field: ADDRESS;  k: Kind) RAISES ANY END;

PROCEDURE WalkRef (r: REFANY;  m: Mask;  v: Visitor) RAISES ANY;
(* Locate each scalar field of the referent "r^".  For each field with
   kind 'k' in 'm' that's found at address 'x', call 'v.apply(x, k)'.
   The only exceptions raised are those raised by 'v's apply method. *)

END RTTypeMap.

