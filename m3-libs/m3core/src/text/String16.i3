(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

(* This interface defines operations on strings of 16-bit "WIDECHAR"s.
   They are represented by address of the first character of
   the string and the string's length in characters. *)

INTERFACE String16;

PROCEDURE Equal (a, b: ADDRESS;  len: CARDINAL): BOOLEAN;
(* Return "TRUE" if the strings of "len" characters starting
   at "a" and "b" have the same (case-sensitive) contents. *)
      
PROCEDURE Compare (a: ADDRESS;  len_a: CARDINAL;
                   b: ADDRESS;  len_b: CARDINAL): [-1..1];
(* Return "-1" if string "a" occurs before string "b", "0" if the
   strings are equal, and "+1" if "a" occurs after "b" in
   lexicographic order. *)

PROCEDURE Hash (a: ADDRESS;  len: CARDINAL;  initial: INTEGER): INTEGER;
(* Return a hash function of the contents of string "a" starting
   with the value "initial". *)

PROCEDURE FindChar (a: ADDRESS;  len: CARDINAL;  c: WIDECHAR): INTEGER;
(* If "c = a[i]" for some "i" in "[0~..~len-1]", return the
   smallest such "i"; otherwise, return "-1". *)

PROCEDURE FindCharR (a: ADDRESS;  len: CARDINAL;  c: WIDECHAR): INTEGER;
(* If "c = a[i]" for some "i" in "[0~..~len-1]", return the
   largest such "i"; otherwise, return "-1". *)

PROCEDURE ArrayStart (READONLY a: ARRAY OF WIDECHAR): ADDRESS;
(* Returns the address of the first character of "a" if it is
   non-empty, otherwise returns "NIL".  WARNING: the returned
   address is only valid as long as "a" does not move.  To
   prevent heap allocated arrays from moving, keep the returned
   address on the stack. *)

END String16.
