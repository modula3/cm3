(* Copyright 1992 Digital Equipment Corporation.            *)
(* Distributed only by permission.                          *)
(* Last modified on Thu Apr 28 18:11:21 PDT 1994 by najork  *)
(*      modified on Tue Aug 11 14:37:44 PDT 1992 by ramshaw *)

INTERFACE AlgTypes;

TYPE
  Relativity = {Absolute, Small, Big};
  Site = RECORD uid: INTEGER; 
                lab: TEXT;
                x, y: INTEGER; 
                rel: Relativity := Relativity.Absolute 
         END;
  Sites = REF ARRAY OF Site;  

(* For an absolute site, the pair (x, y) gives the position.
   For a relative site, the pair (x, y) gives the direction of the
     vector from the other site currently under discussion to this
     site.  The length of that offset vector is infinite or 
     infinitesimal according as the relative site is Big or Small. *)

END AlgTypes.

