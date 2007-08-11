(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE Long;

(* A Long.T w represents a sequence of Long.Size bits
	  w(0), ..., w(Long.Size-1).
   It also represents the unsigned number
	  sum of 2^(i) * w(i) for i in 0, ..., Long.Size-1. *)

TYPE
  T = LONGINT;
  (* encoding is implementation-dependent; e.g., 2's complement. *)

CONST
  Size : INTEGER = BITSIZE (T);                (* implementation-dependent *)

<*EXTERNAL*>
PROCEDURE Plus (x, y: T): T;         (* (x + y) MOD 2^[Long.Size] *)
<*EXTERNAL*>
PROCEDURE Times (x, y: T): T;        (* (x * y) MOD 2^[Long.Size] *)
<*EXTERNAL*>
PROCEDURE Minus (x, y: T): T;        (* (x - y) MOD 2^[Long.Size] *)
<*EXTERNAL*>
PROCEDURE Divide (x, y: T): T;       (* x divided by y *)
<*EXTERNAL*>
PROCEDURE Mod (x, y: T): T;          (* x MOD y *)
<*EXTERNAL*>
PROCEDURE LT (x, y: T): BOOLEAN;     (* x < y *)
<*EXTERNAL*>
PROCEDURE LE (x, y: T): BOOLEAN;     (* x <= y *)
<*EXTERNAL*>
PROCEDURE GT (x, y: T): BOOLEAN;     (* x > y *)
<*EXTERNAL*>
PROCEDURE GE (x, y: T): BOOLEAN;     (* x >= y *)
<*EXTERNAL*>
PROCEDURE And (x, y: T): T;          (* Bitwise AND of x and y *)
<*EXTERNAL*>
PROCEDURE Or (x, y: T): T;           (* Bitwise OR of x and y *)
<*EXTERNAL*>
PROCEDURE Xor (x, y: T): T;          (* Bitwise XOR of x and y *)
<*EXTERNAL*>
PROCEDURE Not (x: T): T;             (* Bitwise complement of x *)

<*EXTERNAL*>
PROCEDURE Shift (x: T; n: INTEGER): T;
(* For all i such that both i and i - n are in the range [0 .. Long.Size - 1],
   bit i of the result equals bit i - n of x. The other bits of the result are
   0. Thus, shifting by n > 0 is like multiplying by 2^(n) *)

<*EXTERNAL*>
PROCEDURE LeftShift (x: T; n: [0..Size-1]): T;
(* = Shift (x, n) *)

<*EXTERNAL*>
PROCEDURE RightShift (x: T; n: [0..Size-1]): T;
(* = Shift (x, -n) *)

<*EXTERNAL*>
PROCEDURE Rotate (x: T; n: INTEGER): T;
(* Bit i of the result equals bit (i - n) MOD Long.Size of x. *)

<*EXTERNAL*>
PROCEDURE LeftRotate (x: T; n: [0..Size-1]): T;
(* = Rotate (x, n) *)

<*EXTERNAL*>
PROCEDURE RightRotate (x: T; n: [0..Size-1]): T;
(* = Rotate (x, -n) *)
 
<*EXTERNAL*>
PROCEDURE Extract (x: T; i, n: CARDINAL): T;
(* Take n bits from x, with bit i as the least significant bit, and return them
   as the least significant n bits of a word whose other bits are 0. A checked
   runtime error if n + i > Long.Size. *)

<*EXTERNAL*>
PROCEDURE Insert (x, y: T; i, n: CARDINAL): T;
(* Return x with n bits replaced, with bit i as the least significant bit, by
   the least significant n bits of y. The other bits of x are unchanged. A
   checked runtime error if n + i > Long.Size. *)

END Long.
