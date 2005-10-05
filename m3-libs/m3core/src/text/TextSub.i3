(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

(* A "TextSub.T" represents a subsequence of another text. *)

INTERFACE TextSub;

TYPE
  TT <: Public;
  Public = TEXT OBJECT
    base  : TEXT;
    start : CARDINAL;
    len   : CARDINAL;
  END;

PROCEDURE New (t: TEXT;  start, length: CARDINAL): TEXT;
(* Return a sub-sequence of "t": empty if "start >= Length(t)" or
   "length = 0"; otherwise the subsequence ranging from "start" to the
   minimum of "start+length-1" and "Length(t)-1". *)

END TextSub.
