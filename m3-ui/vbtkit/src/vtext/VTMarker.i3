(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified On Tue Jun 16 13:12:42 PDT 1992 by muller *)
(*      modified On Mon Oct 14 21:48:33 PDT 1991 by meehan *)
(*      Modified On Mon Nov 12 17:28:12 1990 by jdd *)

INTERFACE VTMarker;

IMPORT VTDef;

TYPE
  T = VTDef.T;
  I = VTDef.I;
  Index = VTDef.Index;
  Marker = VTDef.Marker;
  MarkerOptions = VTDef.MarkerOptions;
  OnOffState = VTDef.OnOffState;
  Tint = VTDef.Tint;
  WhichEnd = VTDef.WhichEnd;


PROCEDURE New
  (vt: T; at: Index; READONLY options: MarkerOptions): Marker RAISES {};

PROCEDURE MakeOptions
  (whichEnd: WhichEnd; top, bottom: BOOLEAN; stroke: Tint): MarkerOptions
   RAISES {};

PROCEDURE Switch (marker: Marker; state: OnOffState) RAISES {};

PROCEDURE Move (marker: Marker; h: Index) RAISES {};

PROCEDURE ChangeOptions
  (marker: Marker; READONLY options: MarkerOptions) RAISES {};

PROCEDURE Close (marker: Marker) RAISES {};

PROCEDURE Fix (vt: T) RAISES {};

PROCEDURE FirstMarker (vt: T; at: I): Marker RAISES {};

PROCEDURE NextMarker (vt: T; VAR (*INOUT*) marker: Marker) RAISES {};

END VTMarker.
