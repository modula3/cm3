(*| Copyright (C) 1993, Digital Equipment Corporation           *)
(*| All rights reserved.                                        *)
(*| See the file COPYRIGHT for a full description.              *)
(*| Last modified on Sun Feb 21 14:26:31 PST 1993 by jdd        *)
(*|      modified on Mon Feb  8 08:49:20 PST 1993 by kalsow     *)
(*|      modified on Tue Oct  9 21:54:08 1990 by muller         *)

(*
  "RTProcedure" provides runtime access to the fingerprints of procedures.

  A procedure's fingerprint is a 64-bit checksum computed from its name
  and signature.  The probability of distinct procedures having the same
  fingerprint is very small.  See the "Fingerprint" interface for more details.
*)

INTERFACE RTProcedure;

IMPORT Fingerprint;

TYPE Proc = ADDRESS;
(* Representing a procedure by its address seems to be the best Modula-3
   approximation we can give for the supertype of all procedure types. *)

PROCEDURE ToFingerprint(p: Proc): Fingerprint.T;
(* Return the fingerprint of the top-level procedure "p".  It is a
   checked runtime error if "p" is not a top-level procedure. *)

PROCEDURE FromFingerprint(READONLY fp: Fingerprint.T): Proc;
(* Return the address of the top-level procedure with fingerprint "fp".
   If no such procedure exists, returns "NIL". *)

END RTProcedure.

