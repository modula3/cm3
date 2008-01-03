(* Copyright 1994 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

INTERFACE XRd;

(* An "XRd.T" is a reader whose source is a string of "X"s. *)

IMPORT Rd;

TYPE T <: Public;
     Public = Rd.T OBJECT METHODS init (len: CARDINAL): T END;

(* "NEW (T).init (n)" returns a reader whose source is n "X"s. *)

END XRd.
       
