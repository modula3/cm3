(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: TrestleExtras.i3,v 1.2 2001-09-19 15:30:31 wagner Exp $ *)

INTERFACE TrestleExtras;
IMPORT VBT;

PROCEDURE LazyAwaitDelete(v: VBT.T);
(* kill the process when this window dies *)

END TrestleExtras.
