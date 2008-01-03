(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* by Steve Glassman and Stephen Harrison *)
(* Last modified on Mon Jul 27 19:45:34 1992 by mjordan*)
(*      modified on Tue Jun  9 22:40:35 1992 by steveg *)

INTERFACE DFSTreeView;

IMPORT GenericTree, DGraphViewClass;

TYPE
  T <: PublicT;
  PublicT = DGraphViewClass.T OBJECT
    v: GenericTree.V;
  END;

END DFSTreeView.
