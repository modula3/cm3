(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

INTERFACE ObLibAnim;
IMPORT ObValue, ObLibUI, VBT, GraphVBT, PaintOpAnim;

  PROCEDURE PackageSetup();
  (* To be called at least once before any other use of the obliqlibanim package. *)

(* ============ "rects" package ============ *)
  TYPE
    ValRects = ObLibUI.ValVBT BRANDED OBJECT
        n: INTEGER;
        shown: BOOLEAN;
      END;


(* ============ "graph" package ============ *)

  TYPE
    ValGraph = ObLibUI.ValVBT BRANDED OBJECT
        shown: BOOLEAN;
      END;

    Graph = GraphVBT.T OBJECT
        valGraph: ValGraph;
        clickAction, clickReleaseAction, doubleClickAction: ObValue.ValFun;
      OVERRIDES
        mouse := Mouse;
      END;
      
    ValVertex =
      ObValue.ValAnything BRANDED OBJECT
        vertex: GraphVBT.Vertex;
      OVERRIDES Is := IsVertex;
      END;
      
    ValVertexHiLi =
      ObValue.ValAnything BRANDED OBJECT
        vertexHiLi: GraphVBT.VertexHighlight;
      OVERRIDES Is := IsVertexHiLi;
      END;
      
    ValEdge =
      ObValue.ValAnything BRANDED OBJECT
        edge: GraphVBT.Edge;
      OVERRIDES Is := IsEdge;
      END;
      
    ValPolygon =
      ObValue.ValAnything BRANDED OBJECT
        polygon: GraphVBT.Polygon;
      OVERRIDES Is := IsPolygon;
      END;

    ValFont =
      ObValue.ValAnything BRANDED OBJECT
        font: GraphVBT.WorldFont;
      OVERRIDES Is := IsFont;
      END;

    ValSpectrum =
      ObValue.ValAnything BRANDED OBJECT
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
