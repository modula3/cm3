(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* Last modified on Thu Sep 23 11:18:16 PDT 1993 by mhb    *)

INTERFACE GraphVBTExtras;

IMPORT Font, GraphVBT, MG, Point, R2;

<* PRAGMA LL *>

<* LL.sup >= graph.mu for all procedures *>

PROCEDURE GetMG(graph: GraphVBT.T): MG.V;

PROCEDURE WorldPosToPts (graph: GraphVBT.T; READONLY pos: R2.T): R2.T;

PROCEDURE WorldSizeToPts (graph: GraphVBT.T; READONLY size: R2.T): R2.T;

PROCEDURE ScreenPtToWorldPos (graph: GraphVBT.T; READONLY pt: Point.T): R2.T;

PROCEDURE WorldFontFromText (font: TEXT): GraphVBT.WorldFont;

PROCEDURE WorldFontFromFont (font: Font.T): GraphVBT.WorldFont;

PROCEDURE FontFromWorldFont (wf: GraphVBT.WorldFont): Font.T;

END GraphVBTExtras.
