(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Test EXPORTS Main;

TYPE
  Key = REFANY;
  Value = REFANY;
  CompareProc = PROCEDURE (arg: REFANY; key1: Key; key2: Key): INTEGER;
  Link = REF Node;
  Node = RECORD
              key: Key;
              value: Value;
              red: BOOLEAN; (* color of link pointing to this node *)
              l, r: Link
            END;
  T = RECORD
             h: Link;
             z: Link; (* shared external node *)
             y: Link; (* son of z *)
             compare: CompareProc;
             compareArg: REFANY
           END;

PROCEDURE Put (table: T; key: Key; value: Value): BOOLEAN =
  VAR
    x: Link;
    gg: Link; (* great-grandfather of x *)
  PROCEDURE Split () =
    PROCEDURE Rotate (y: Link): Link =
      VAR
        s: Link; (* son of y *)
        gs: Link; (* grandson of y *)
        yLeft: BOOLEAN;
      BEGIN
        yLeft := table.compare (table.compareArg, key, y.key) < 0;
        IF table.compare (table.compareArg, key, s.key) < 0 THEN
        ELSE
        END;
        RETURN gs
      END Rotate;
    BEGIN
      x := Rotate (gg);
    END Split;
  BEGIN
    Split();
    EVAL value;
    RETURN FALSE
  END Put;

VAR t: T; r: REF INTEGER;
BEGIN
  IF Put(t, r, r) THEN ELSE END
END Test.
