INTERFACE M3LOpaque;

(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

IMPORT M3Context;

PROCEDURE SetReveal(c: M3Context.T) RAISES {};
(* Set up the tmp_rev_type_spec attribute for opaque types
   to be the same as the sm_concrete_type_spec. *)

END M3LOpaque.
