(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id$ *)

INTERFACE StartingStarters;
IMPORT StarterScanList;
TYPE
  T = StarterScanList.T;

PROCEDURE Get(): T;
(* Return a list consisting of StarterScan.FromPath(arg) for
   each command line argument. *)

END StartingStarters.
