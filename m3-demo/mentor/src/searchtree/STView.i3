(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Wed Jun 15 19:35:19 PDT 1994 by heydon                   *)
(*      modified on Wed Jan  6 15:55:22 PST 1993 by steveg                   *)

INTERFACE STView;

IMPORT View, SearchTreeViewClass, GenericTree, PaintOp, Font;

TYPE
  T <: TPublic;
  TPublic = SearchTreeViewClass.T OBJECT
    v: GenericTree.V;			 (* child VBT of T *)
  END;

CONST
  BorderWidth = 10.0;
  BorderHeight = 20.0;
  NodeWidth = 20.0;
  NodeHeight = 20.0;
  ThinWeight = 2.0;
  ThickWeight = 4.0;
  ThickLineWeight = 6.0;

VAR (* READONLY *)
  nodeColor: PaintOp.ColorScheme;	 (* color of tree nodes *)
  currentColor: PaintOp.ColorScheme;	 (* color of "current" node *)
  compareColor: PaintOp.ColorScheme;	 (* color of "compared" node *)
  font: Font.T;

PROCEDURE New(view: T; v: GenericTree.V): View.T;
(* Return a the search tree view "view" initialized to contain the generic
   tree view "v". *)

PROCEDURE StartRun(view: T);
(* Startrun procedure for use by all search tree views. This procedure is
   meant to be used as the OVERRIDE value for the startRun() method of a
   View.T. *)

END STView.
