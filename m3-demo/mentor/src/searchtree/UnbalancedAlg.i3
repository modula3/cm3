(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Jul 27 10:10:59 PDT 1992 by heydon                   *)

INTERFACE UnbalancedAlg;

IMPORT BSTAlg; 
FROM Thread IMPORT Alerted;

PROCEDURE Insert(alg: BSTAlg.T; n: BSTAlg.Node)  RAISES {Alerted};
(* Insert node ``n'' in the tree ``alg.tree'', and generate animation
events for views and code views. *)

END UnbalancedAlg.
