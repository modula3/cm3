(* Copyright 1997, Critical Mass, Inc.  All rights reserved. *)

(* A "TextCat.T" represents a concatenation of other texts. *)

INTERFACE TextCat;

TYPE
  T <: Public;
  Public = TEXT OBJECT
    a, b         : TEXT;      (* READONLY *)
    a_len, b_len : CARDINAL;  (* READONLY *)
    a_or_b_wide  : BOOLEAN;   (* READONLY *)
  END;
  (* Represents:  "a & b" *)

PROCEDURE New (t, u: TEXT): TEXT;
(* Return the concatenation of "t" and "u". *)

PROCEDURE NewMulti (READONLY x: ARRAY OF TEXT): TEXT;
(* Return the concatenation of "t" and "u". *)

END TextCat.
