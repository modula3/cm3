(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id$ *)

INTERFACE RuleListState;
IMPORT Rule;
IMPORT RuleList;
IMPORT PDATrans;
IMPORT TextTextTbl;
CONST
  Brand = "RuleListState";
TYPE
  T <: Public;
  Public = OBJECT ID: INTEGER; END;
  Action = RECORD
    kind: PDATrans.ActKind;
    next: T := NIL;       (* nonNIL if type = ActKind.Shift *)
    rule: Rule.T := NIL;  (* nonNIL if type = ActKind.Reduce *)
  END;
PROCEDURE New(r: RuleList.T; warnings: TextTextTbl.T): T;
PROCEDURE Expand(self: T; VAR est: INTEGER); (* must expand before step *)
PROCEDURE Step(a: T; code: INTEGER; symName: TEXT): Action;
PROCEDURE Equal(a, b: T): BOOLEAN;
PROCEDURE Hash(a: T): INTEGER;
PROCEDURE Format(a: T): TEXT;
END RuleListState.
