(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Last modified on Mon Feb 10 17:52:29 PST 1992 by muller                   *)
(*      modified on Mon Feb 10 16:54:50 PST 1992 by ayers                    *)



INTERFACE DisplayListStack;

IMPORT DisplayList, DPS, Linked2Tree;

TYPE T = DisplayList.T OBJECT  
 METHODS 
  NswBoxOfChild (e: DisplayList.E; box: DPS.Box) := NewBoxOfChild; 
  Stack (firstHighY: REAL := -1.0) := Stack; 
 OVERRIDES 
  Prepend := Prepend; 
  Append := Append; 
  InsertBefore := InsertBefore; 
  InsertAfter := InsertAfter; 
  RemoveChild := RemoveChild; 
  END;
TYPE E = T;
TYPE R = T;

PROCEDURE NewBoxOfChild (t: T; e: DisplayList.E; box: DPS.Box);
PROCEDURE Prepend (t: T; e: Linked2Tree.E);
PROCEDURE Append (t: T; e: Linked2Tree.E);
PROCEDURE InsertBefore (t: T; e, before: Linked2Tree.E);
PROCEDURE InsertAfter (t: T; e, after: Linked2Tree.E);
PROCEDURE RemoveChild (t: T; e: Linked2Tree.E);

PROCEDURE Stack (t: T; firstHighY: REAL := -1.0); 
  
  END DisplayListStack.



