(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: DFATransOp.i3,v 1.1.1.1 2001-09-19 11:40:10 wagner Exp $ *)

INTERFACE DFATransOp;
IMPORT DFATransList;
IMPORT DFATransIntTbl;
TYPE
  T = DFATransList.T;

PROCEDURE GetTarget(t: T; c: CHAR): INTEGER;

PROCEDURE Simplify(t: T): T; (* express as fewer DFATrans.T intervals by *)
  (* allowing overlapping intervals. First in list has highest priority. *)
  (* on entry assumes t is sorted by decreasing key *)

PROCEDURE Tally(table: DFATransIntTbl.T; t: T);
(* increment the entry for t in table *)

PROCEDURE Sort(table: DFATransIntTbl.T; t: T);
(* sort intervals in t by increasing tally, commutativity permitting. *)

END DFATransOp.
