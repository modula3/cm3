(*| Copyright (C) 1993, Digital Equipment Corporation           *)
(*| All rights reserved.                                        *)
(*| See the file COPYRIGHT for a full description.              *)
(*| Last modified on Tue Nov  9 12:19:36 PST 1993 by mcjones    *)
(*|      modified on Thu Oct 21 15:01:22 PDT 1993 by kalsow     *)
(*|      modified on Sun Feb 21 14:27:08 PST 1993 by jdd        *)
(*|      modified on Tue Sep 25 00:38:09 1990 by muller         *)

(*
  "RTTypeFP" provides runtime access to type fingerprints.
  \index{fingerprint!of type}
  \index{type!fingerprint of}

  A type's fingerprint is a 64-bit checksum computed from its declaration.
  The probability of distinct types having the same fingerprint is very
  small.  See the "Fingerprint" interface for more details.

  Typecodes may vary between executions of a program but fingerprints
  do not.  Fingerprints are portable across multiple runs of a single
  program and across all programs compiled by the same compiler.
*)

INTERFACE RTTypeFP;

IMPORT Fingerprint;
FROM RTType IMPORT Typecode;

PROCEDURE ToFingerprint(tc: Typecode): Fingerprint.T;
(* Return the fingerprint corresponding to "tc".  It is a checked
   runtime error if "tc" is not proper or does not name a traced
   reference type. *)

PROCEDURE FromFingerprint(READONLY fp: Fingerprint.T)
  : Typecode;
(* Return the typecode that corresponds to "fp".  If no such typecode
   exists, returns "RTType.NoSuchType". *)

END RTTypeFP.


