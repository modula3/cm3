(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: NFANode.i3,v 1.1.1.1 2001-09-19 11:40:10 wagner Exp $ *)

INTERFACE NFANode;
IMPORT NFA;
CONST
  Brand = "NFANode";
  NoOutput = LAST(INTEGER);
  Epsilon = '\000';
TYPE
  T = REF RECORD
    targ1, targ2: T := NIL;
    keyBegin, keyEnd: CHAR := Epsilon;
    output: INTEGER := NoOutput;  (* effective on match or keyBegin=\0 *)
    next: T := NIL;          (* for enumerating nodes *)
    ID: INTEGER := -1;       (* position in NFANodeList.T *)
    marked: BOOLEAN := FALSE; (* for epsilon chases *)
  END;
REVEAL
  NFA.T = BRANDED REF RECORD
    start: T;   (* NFA start and Nodelist first. Never NIL. *)
    end: T;     (* NFA end and Nodelist last. targs uninitialized. *)
  END;

PROCEDURE Compare(a, b: T): [-1 .. 1];
PROCEDURE Equal(a, b: T): BOOLEAN;
PROCEDURE Hash(a: T): INTEGER; (* assumes ID assigned *)
PROCEDURE Format(a: T): TEXT;
END NFANode.
