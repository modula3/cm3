(* Copyright (C) 2002 Hewlett-Packard Company *)
(* Copyright (C) 2000, 2002 Compaq Computer Corporation *)
(* Last modified on Fri Mar 15 17:58:23 PST 1996 by detlefs *)
(*      modified on Thu Feb 29 11:58:50 1996 by gnelson *)
(* Copyright 1996 by Digital *)

(* A fingerprint is a hash function that is very unlikely to produce
   collisions.  See the "Fingerprint" interface in the SRC Modula-3
   standard interfaces for a description of how to use fingerprints.
   This interface is just like the standard interface "Fingerprint",
   except that it provides a few convenience procedures for use in
   ESC. *)

INTERFACE FPrint31; IMPORT Fingerprint;

TYPE 
  T = Fingerprint.T;

CONST
   Zero = Fingerprint.Zero;
   Brand = "FPrint31";

VAR (*CONST*) OfEmpty: T; 

(* "OfEmpty" is the fingerprint of the empty text. *)

PROCEDURE FromText(t: TEXT): T;
PROCEDURE FromInt(i: INTEGER): T;
PROCEDURE FromLongReal(r: LONGREAL): T;
(* These three procedures return the fingerprint of their argument. *)

PROCEDURE Combine(f, g: T): T;
(* Return the fingerprint of the pair "(f,g)".  *)

PROCEDURE OfChars(init: T; READONLY t: ARRAY OF CHAR): T;
(* Return the fingerprint of "s & t", given that "init" is the
   fingerprint of "s". *)

PROCEDURE AddInt(fp: T; i: INTEGER): T;
(* Given "fp" the fingerprint of a sequence "s" of integers,
   return the fingerprint of "s&[i]". *)

(* "AddInt" can be used to  fingerprint a sequence of integers,
   by starting with "OfEmpty" or "Zero" (or any other fingerprint) 
   and combining them into a result one by one with "AddInt".

   To avoid collisions, it is important to avoid fingerprinting
   fingerprints, except via the procedures "Combine" and "AddInt". *) 

(* The next three procedures allow you to use "FPrint31.T"s as
   the key type in a generic table, and in other generics. *)

PROCEDURE Equal(fp1, fp2: T): BOOLEAN;

PROCEDURE Hash(fp: T): INTEGER;

PROCEDURE Compare(fp1, fp2: T): [-1..1];

PROCEDURE ToText(fp: T): TEXT;

END FPrint31.
