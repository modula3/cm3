(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: NFA.i3,v 1.2 2001-09-19 15:04:09 wagner Exp $ *)

INTERFACE NFA;
IMPORT Interval;
IMPORT CharRange;
CONST
  OrMore = LAST(INTEGER);
  Brand = "NFA";
TYPE
  T <: REFANY;

PROCEDURE Copy(a: T): T;
PROCEDURE Empty(): T;
PROCEDURE FromString(s: TEXT): T;
PROCEDURE FromRange(c: CharRange.T): T;
PROCEDURE FromChar(c: CHAR): T;

(* These procs destroy a,b to produce new NFA.T result *)
PROCEDURE Concat(a, b: T): T;
PROCEDURE Or(a, b: T; endCap: BOOLEAN := TRUE): T;
PROCEDURE Rept(a: T; count: Interval.T): T;
PROCEDURE Output(a: T; code: INTEGER): T;

(* Must assign IDs before calling NFAState.Step *)
PROCEDURE AssignIDs(a: T): INTEGER;
END NFA.
