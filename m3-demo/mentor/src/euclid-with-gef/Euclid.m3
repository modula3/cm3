(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Wed Feb 17 15:41:49 PST 1993 by steveg *)
(*      modified on Thu Sep 24 12:14:33 PDT 1992 by mhb *)

MODULE Euclid;

IMPORT GEFView, List;

BEGIN
  GEFView.Create("Euclid", "P47", "View", "EuclidAlg.gef", "EuclidView.gef",
                 List.List1(List.List2("Euclid's proof", "proof"))) ;
END Euclid.


