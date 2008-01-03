(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Mon May 11 21:22:42 1992 by mhb   *)
(*      modified on Thu Apr 30  1:04:02 PDT 1992 by johnh *)

INTERFACE TracedAlg;

IMPORT Algorithm;

(* A "TracedAlg.T" is an "Algorithm.T" subclass that is useful for
   debugging. It outputs to stdout each time that one of its methods 
   is invoked. *)

TYPE
  T <: Algorithm.T;

END TracedAlg.
 
