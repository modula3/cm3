(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(*                                                                           *)
(* Parts Copyright (C) 1997, Columbia University                             *)
(* All rights reserved.                                                      *)
(*
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Mon Aug  4 15:04:56 1997
 *)

INTERFACE ObLibAnim;
IMPORT ObValue, ObLibUI, VBT, GraphVBT, PaintOpAnim, SynWr;

  PROCEDURE PackageSetup();
  (* To be called at least once before any other use of the obliqlibanim package. *)

(* ============ "rects" package ============ *)
  TYPE
    ValRects = ObLibUI.ValVBT BRANDED "ObLibAnim.ValRects" OBJECT
        n: INTEGER;
        shown: BOOLEAN;
      END;


(* ============ "graph" package ============ *)

  TYPE
    ValGraph = ObLibUI.ValVBT BRANDED "ObLibAnim.ValGraph" OBJECT
        shown: BOOLEAN;
      END;

    Graph = GraphVBT.T OBJECT
        valGraph: ValGraph;
        clickAction, clickReleaseAction, doubleClickAction: ObValue.ValFun;
        swr: SynWr.T;
      OVERRIDES
        mouse := Mouse;
      END;
      
    ValVertex =
      ObValue.ValAnything BRANDED "ObLibAnim.ValVertex" OBJECT
        vertex: GraphVBT.Vertex;
      OVERRIDES Is := IsVertex;
      END;
      
    ValVertexHiLi =
      ObValue.ValAnything BRANDED "ObLibAnim.ValVertexHiLi" OBJECT
        vertexHiLi: GraphVBT.VertexHighlight;
      OVERRIDES Is := IsVertexHiLi;
      END;
      
    ValEdge =
      ObValue.ValAnything BRANDED "ObLibAnim.ValEdge" OBJECT
        edge: GraphVBT.Edge;
      OVERRIDES Is := IsEdge;
      END;
      
    ValPolygon =
      ObValue.ValAnything BRANDED "ObLibAnim.ValPolygon" OBJECT
        polygon: GraphVBT.Polygon;
      OVERRIDES Is := IsPolygon;
      END;

    ValFont =
      ObValue.ValAnything BRANDED "ObLibAnim.ValFont" OBJECT
        font: GraphVBT.WorldFont;
      OVERRIDES Is := IsFont;
      END;

    ValSpectrum =
      ObValue.ValAnything BRANDED "ObLibAnim.ValSpectrum" OBJECT
        graph: Graph;
        spectrum: PaintOpAnim.T;
      OVERRIDES Is := IsSpectrum;
      END;

  PROCEDURE Mouse(self: Graph; READONLY cd: VBT.MouseRec);

  PROCEDURE IsVertex(self: ValVertex; other: ObValue.ValAnything): BOOLEAN;
  PROCEDURE IsVertexHiLi(self: ValVertexHiLi; other: ObValue.ValAnything): BOOLEAN;
  PROCEDURE IsEdge(self: ValEdge; other: ObValue.ValAnything): BOOLEAN;
  PROCEDURE IsPolygon(self: ValPolygon; other: ObValue.ValAnything): BOOLEAN;
  PROCEDURE IsFont(self: ValFont; other: ObValue.ValAnything): BOOLEAN;
  PROCEDURE IsSpectrum(self: ValSpectrum; other: ObValue.ValAnything): BOOLEAN;

END ObLibAnim.
