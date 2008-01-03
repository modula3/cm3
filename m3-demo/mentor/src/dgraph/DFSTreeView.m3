(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman and Stephen Harrison                                    *)
(* Last modified on Thu Jan  5 17:01:28 PST 1995 by najork   *)
(*      modified on Wed Jan  6 16:50:40 PST 1993 by steveg   *)
(*      modified on Tue Aug  4 02:03:06 1992 by mjordan  *)

<*PRAGMA LL*>

MODULE DFSTreeView;

IMPORT AdjMatrix, Axis, Filter, GenericSubTreeSelector, GenericTree, MG, 
       MGPublic, R2, View, ZeusPanel, TextVBT;


REVEAL
  T = PublicT BRANDED OBJECT
        forest_root: GenericTree.GenericTree := NIL;
        forest_child_pred: GenericTree.GenericTree := NIL;
      OVERRIDES
        startrun      := StartRunT;
        oeSetup       := SetupT;
        oeHighlight   := HighlightT;
        oeAddChild    := AddChildT;
        oeRemoveChild := RemoveChildT;
        oeNewTree     := NewTreeT;
      END;

PROCEDURE SetupT(view: T; <*UNUSED*> m: AdjMatrix.T) =
  BEGIN 
    view.v := NEW(GenericTree.V, view := view,
                  selector := GenericSubTreeSelector.closest,
                  border := ARRAY Axis.T OF REAL{Width, Height}).init();
    (* CreateForestRoot(view); BUG in MG *)
    EVAL Filter.Replace(view, view.v);
  END SetupT;

CONST
  Width = 12.0;
  Height = 12.0;
  ForestRootId = LAST(INTEGER);

PROCEDURE StartRunT(view: T) =
  BEGIN
    View.T.startrun(view);
  END StartRunT;

VAR 
  highlight := MGPublic.ColorFromText("Pink");
  color := MGPublic.ColorFromText("LightTan");

PROCEDURE NewTreeT(view: T; new: INTEGER; label: TEXT) RAISES {}=
  BEGIN
    WITH tree_root = NewNode(view, new, label) DO
      IF view.forest_root = NIL THEN
        CreateForestRoot(view); (* MG BUG *)
      END; (* if *)
      GenericTree.AddChild(
        view.forest_root, view.v, view.forest_child_pred,
        tree_root);
      view.forest_child_pred := tree_root;
    END;
  END NewTreeT;


PROCEDURE NewNode (view: T; new: INTEGER; label: TEXT
    ): GenericTree.GenericTree =
  BEGIN
    RETURN NEW(GenericTree.GenericTree, id := new).init(
           view.v, NEW(MG.Ellipse, label := label, color := color).init(
                     R2.Origin, R2.T{Width, Height}));
  END NewNode;

PROCEDURE AddChildT (view: T; parent, pred, new: INTEGER; label: TEXT) =
  VAR v := view.v;
  BEGIN
    GenericTree.AddChild(
      MGPublic.Lookup(v, parent), v, MGPublic.Lookup(v, pred),
      NewNode(view, new, label));
  END AddChildT;

PROCEDURE RemoveChildT (view: T; parent, ch: INTEGER) =
  VAR v := view.v;
  BEGIN
    GenericTree.RemoveChild(
      MGPublic.Lookup(v, parent), v, MGPublic.Lookup(v, ch));
  END RemoveChildT;

PROCEDURE HighlightT (view    : T;
                      node    : INTEGER;
                      value   : REAL;
                      nodeOnly: BOOLEAN   ) =
  VAR
    v := view.v;
    t := MGPublic.Lookup(v, node);
  BEGIN
    IF nodeOnly THEN t := NARROW(t, GenericTree.SubTree).graphic END;
    MGPublic.SetHighlight(t, v, value);
    IF value > 0.0 THEN
      MGPublic.SetColor(t, v, highlight);
    ELSE
      MGPublic.SetColor(t, v, color);
    END;
  END HighlightT;

PROCEDURE CreateForestRoot(view: T) RAISES {}=
  BEGIN
    WITH forest_root = NEW(GenericTree.Forest, id := ForestRootId).init(
                                                              view.v) DO
      view.forest_root := forest_root;
      GenericTree.SetRoot(forest_root, view.v);
    END; (* with *)
  END CreateForestRoot;

PROCEDURE New (): View.T =
  VAR dummy := TextVBT.New("Depth First Search Tree View");
  BEGIN
    RETURN NEW(T).init(dummy);
  END New;

BEGIN
  ZeusPanel.RegisterView (New, "Depth First Search Tree View", "DGraph");
END DFSTreeView.
