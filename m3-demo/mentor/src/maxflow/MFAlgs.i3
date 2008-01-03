(* Copyright 1992 Digital Equipment Corporation.             *)
(* Distributed only by permission.                           *)
(* Last modified on Wed May  4 09:30:41 PDT 1994 by najork   *)
(*      modified on Mon Jul 27 04:56:44 1992 by karsenty     *)

INTERFACE MFAlgs;

IMPORT MFGraph, R2;

TYPE
  MFEdge = MFGraph.Edge BRANDED OBJECT
      capacity, flow : REAL;
  END;

  MFVertex = MFGraph.Vertex BRANDED OBJECT
      wherefrom : MFEdge;
      marked : BOOLEAN;
      pos: R2.T; (* the GraphVBT position *)
      label: TEXT; (* the GraphVBT label *)
  END;

PROCEDURE NullText(g: MFGraph.T) : TEXT;

PROCEDURE PrintText(t: TEXT) : TEXT;

END MFAlgs.
