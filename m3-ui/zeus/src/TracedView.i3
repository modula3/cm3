(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Mon May 11 21:21:47 1992 by mhb   *)

INTERFACE TracedView;

IMPORT View;

(* A "TracedView.T" is a "View.T" subclass that is useful for
   debugging. It outputs to stdout each time that one of its methods 
   is invoked. *)

TYPE
  T <: View.T;

END TracedView.
 
