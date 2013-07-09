(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: TipeDesc.i3                                           *)
(* Last Modified On Tue Jul  5 15:39:08 PDT 1994 by kalsow     *)

INTERFACE TipeDesc;

IMPORT Target, M3;

(* A "type description" is a stream of bytes.  It represents a Modula-3
   type.  It begins with an integer equal to the number of graph nodes,
   then a preorder traversal of the corresponding AST nodes.  Each of the
   type constructors is represented by a single byte (=ORD(Op)) and zero
   or more operands.

   Integer values are encoded in a variable-length byte stream. The
   first byte contains two flag bits: the sign bit (s=16_80) and
   the special bit (x=16_40).  The rest of the first byte (v=16_3f)
   represents the values 0..16_3f.  So, given a stream x0, x1, x2, ...
   of 0..255 values where x0 is split into the fields (s/x/v), the
   represented integer value is:

          (x=0)             value = sign(s) * v
          (x=1) (v=1..8)    value = sign(s) * (SUM(i=1 to v) xi<<8*i)
    (s=0) (x=1) (v=16_3e)   value = +16_7fffffff  = +2^31 - 1
    (s=1) (x=1) (v=16_3e)   value = -16_80000000  = -2^31
    (s=0) (x=1) (v=16_3f)   value = +16_7fffffffffffffff  = +2^63 - 1
    (s=1) (x=1) (v=16_3f)   value = -16_8000000000000000  = -2^63

   where sign(0)=+1, sign(1)=-1.

*)
  
   

TYPE (* This must be kept in sync with type RTTipe.m3.Op, in runtime. *) 
  Op = {
  (* opcode         -- op  --- operands ------------------- *)
     Address,       (* 00                                   *)
     Array,         (* 01, #elements: INT, element: TYPE    *)
     Boolean,       (* 02                                   *)
     Cardinal,      (* 03                                   *)
     Char,          (* 04                                   *)
     Enum,          (* 05, #elements: INT                   *)
     Extended,      (* 06                                   *)
     Integer,       (* 07                                   *)
     Longcard,      (* 08                                   *)
     Longint,       (* 09                                   *)
     Longreal,      (* 0a                                   *)
     Null,          (* 0b                                   *)
     Object,        (* 0c, #fields: INT, {fields: TYPE}     *)
     OpenArray,     (* 0d, #dimensions: INT, element: TYPE  *)
     Packed,        (* 0e, bit size: INT, base type: TYPE   *)
     Proc,          (* 0f                                   *)
     Real,          (* 10                                   *)
     Record,        (* 11, #fields: INT, {fields: TYPE}     *)
     Ref,           (* 12, self id: UID                     *)
     Refany,        (* 13                                   *)
     Set,           (* 14, #elements: INT                   *)
     Subrange,      (* 15, min, max: INT                    *)
     UntracedRef,   (* 16, self id: UID                     *)
  (* Widechar is denoted as Enum, with #elements = 16_110000   
     This could be fixed by a coordinated change, here, in 
     Enumtype.m3, and RTTipe.m3, but would invalidate 
     compiled code.                                         *)
     OldN,          (* 17, node #: INT                      *)
     Old0           (* 18                                   *)
  };(* Old1, Old2, ... Old(255-ORD(Old0)) *)

PROCEDURE Start ();
(* begin the construction of a description *)

PROCEDURE Finish (a, b, c, d: TEXT := NIL): INTEGER;
(* finish the description, allocate global constant space for it and emit it.
   Returns the offset of the generated map in the global constant pool. *)

PROCEDURE AddO (o: Op;  type: M3.Type): BOOLEAN;
(* add operator 'o' to the current description.  If 'type' has already been
   seen, adds an 'Old' opcode and returns FALSE, otherwise returns TRUE. *)

PROCEDURE AddU (uid: INTEGER);
(* add the 32-bit type 'uid' to the current description. *)

PROCEDURE AddI (i: INTEGER);
(* add integer 'i' to the current description (max 32 bits!)*)

PROCEDURE AddX (READONLY i: Target.Int);
(* add a variable length target integer 'i' to the description *)

PROCEDURE Reset ();

END TipeDesc.
