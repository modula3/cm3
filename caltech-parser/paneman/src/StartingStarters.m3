(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id$ *)

MODULE StartingStarters;
IMPORT StarterScan;
IMPORT StarterScanList;
IMPORT Args;
IMPORT Text;

PROCEDURE Get(): T =
  CONST
    Ignore = SET OF CHAR{'-', '@'};
  VAR
    args := Args.CommandLine();
    result: StarterScanList.T := NIL;
  BEGIN
    FOR i := 0 TO LAST(args^) DO
      IF NOT Text.GetChar(args[i], 0) IN Ignore THEN
        result := StarterScanList.Cons(StarterScan.FromPath(args[i]), result);
      END;
    END;
    RETURN result;
  END Get;

BEGIN
END StartingStarters.
