(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Last modified on Fri Jan 14 12:20:08 PST 1994 by kalsow                   *)
(*      modified on Mon Feb 10 17:52:22 PST 1992 by muller                   *)
(*      modified on Mon Feb 10 16:54:50 PST 1992 by ayers                    *)

INTERFACE Pile;

TYPE Element = OBJECT 
  next: Element; 
  key: INTEGER := 0;
  name: TEXT := NIL
  END; 

TYPE T = OBJECT 
  mutex: MUTEX; 
  first: Element 
 METHODS 
  Insert (it: Element) := Insert; 
  Delete (it: Element) := Delete; 
  FindByName (name: TEXT): Element := FindByName;
  FindByKey (key: INTEGER): Element := FindByKey 
  END;
PROCEDURE New (): T; 

PROCEDURE Insert (pile: T; it: Element); 
PROCEDURE Delete (pile: T; it: Element); 
PROCEDURE FindByName (pile: T; name: TEXT): Element; 
PROCEDURE FindByKey (pile: T; key: INTEGER): Element; 

  END Pile.

