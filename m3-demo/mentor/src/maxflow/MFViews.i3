(* Copyright 1992 Digital Equipment Corporation.             *)
(* Distributed only by permission.                           *)
(* Last modified on Wed May  4 09:33:33 PDT 1994 by najork   *)
(*      modified on Mon Jul 27 04:56:31 1992 by karsenty     *)

INTERFACE MFViews;

IMPORT MFGraph, PaintOp;

PROCEDURE PrintEdge(e: MFGraph.Edge) : TEXT;
PROCEDURE Color(color: TEXT): PaintOp.T;
PROCEDURE BlueColor(sat: REAL): PaintOp.T;

END MFViews.
