(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

(* NOTE on naming.  This source file was named when WIDECHAR was always 16-bits.
   Today, it can be that, or 32 bits, with upper bound of 16_10FFFF, for full
   Unicode range.  It's tedious to change many source file names in CVS, so
   the "16" remains. *)  

(* This interface defines operations on strings of "WIDECHAR"s.
   They are represented by address of the first character of
   the string and the string's length in characters. *)

INTERFACE String16;

PROCEDURE Equal (a, b: ADDRESS;  len: CARDINAL): BOOLEAN;
(* Return "TRUE" if the strings of "len" characters starting
   at "a" and "b" have the same (case-sensitive) contents. *)
(* PRE: a MOD ADRSIZE(WIDECHAR) = 0 AND b MOD ADRSIZE(WIDECHAR) = 0 *) 
      
PROCEDURE Compare (a: ADDRESS;  len_a: CARDINAL;
                   b: ADDRESS;  len_b: CARDINAL): [-1..1];
(* Return "-1" if string "a" occurs before string "b", "0" if the
   strings are equal, and "+1" if "a" occurs after "b" in
   lexicographic order. *)
(* PRE: a MOD ADRSIZE(WIDECHAR) = 0 AND b MOD ADRSIZE(WIDECHAR) = 0 *) 

PROCEDURE Hash (a: ADDRESS;  len: CARDINAL;  initial: INTEGER): INTEGER;
(* Return a hash function of the contents of string "a" starting
   with the value "initial". *)
(* PRE: a MOD ADRSIZE(WIDECHAR) = 0 *) 

PROCEDURE FindChar (a: ADDRESS;  len: CARDINAL;  c: WIDECHAR): INTEGER;
(* If "c = a[i]" for some "i" in "[0~..~len-1]", return the
   smallest such "i"; otherwise, return "-1". *)
(* PRE: a MOD ADRSIZE(WIDECHAR) = 0 *) 

PROCEDURE FindCharR (a: ADDRESS;  len: CARDINAL;  c: WIDECHAR): INTEGER;
(* If "c = a[i]" for some "i" in "[0~..~len-1]", return the
   largest such "i"; otherwise, return "-1". *)
(* PRE: a MOD ADRSIZE(WIDECHAR) = 0 *) 

PROCEDURE HasWideChars (a: ADDRESS; len: CARDINAL): BOOLEAN;
(* Return ORD(a[i]) > LAST (CHAR), for some "i" in "[0~..~len-1]". *) 
(* PRE: a MOD ADRSIZE(WIDECHAR) = 0 *) 

PROCEDURE ArrayStart (READONLY a: ARRAY OF WIDECHAR): ADDRESS;
(* Returns the address of the first character of "a" if it is
   non-empty, otherwise returns "NIL".  WARNING: the returned
   address is only valid as long as "a" does not move.  To
   prevent heap allocated arrays from moving, keep the returned
   address on the stack. *)

END String16.

