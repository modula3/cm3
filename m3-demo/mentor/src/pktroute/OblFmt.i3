(* Copyright 1993 Digital Equipment Corporation.                             *)
(* Distributed only by permission.                                           *)
(*                                                                           *)
(* Last modified on Mon Aug  2 00:08:32 PDT 1993 by heydon                   *)

INTERFACE OblFmt;

(* An interface for converting Modula-3 value to Oblique values. *)

PROCEDURE Bool(b: BOOLEAN): TEXT;
(* Returns "true" or "false" as "b" is TRUE or FALSE, respectively. *)

PROCEDURE Real(r: REAL): TEXT;
(* Returns the value "r" as a text so as to conform to Oblique syntax. In
   Oblique, a real number has the form: "[~] Digit {Digit} . Digit {Digit}",
   where "[~]" denotes an optional negation, and "{Digit}" denotes zero or
   more digits. *)

END OblFmt.
