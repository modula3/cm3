(* Copyright (C) 2002 Hewlett-Packard Company *)
(* Copyright (C) 2000, 2002 Compaq Computer Corporation *)
(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Jun 10 21:15:11 PDT 1999 by saxe                     *)
(*      modified on Wed Sep 20 17:05:53 PDT 1995 by detlefs                  *)

MODULE RefListMisc;

(* Generally useful procedures on "RefList.T"'s. *)

IMPORT RefList;

PROCEDURE SetSubset(l1, l2: RefList.T; equiv: EquivProc): BOOLEAN =
  BEGIN
    (* Is "l1" a subset of "l2"? *)
    WHILE l1 # NIL DO
      VAR l1Hd := l1.head;
          l2a := l2;
      BEGIN
        WHILE l2a # NIL AND NOT equiv(l1Hd, l2a.head) DO
          l2a := l2a.tail
        END (* WHILE *);
        IF l2a = NIL THEN RETURN FALSE END (* IF *)
      END (* BEGIN *);
      l1 := l1.tail
    END (* WHILE *);
    RETURN TRUE
  END SetSubset;

PROCEDURE SetEquiv(l1, l2: RefList.T; equiv: EquivProc): BOOLEAN =
  BEGIN 
    IF l1 = l2 THEN 
      RETURN TRUE 
    ELSE
      RETURN SetSubset(l1, l2, equiv) AND SetSubset(l2, l1, equiv)
    END
  END SetEquiv;

PROCEDURE ListEquiv(l1, l2: RefList.T; equiv: EquivProc): BOOLEAN =
  BEGIN
    (* Is each element of "l1" equivalent to the corresponding element
       of "l2"? *)
    WHILE l1 # NIL AND l2 # NIL DO
      IF equiv(l1.head, l2.head) THEN
        l1 := l1.tail;
        l2 := l2.tail
      ELSE
        RETURN FALSE
      END (* IF *);
    END (* WHILE *);
    RETURN l1 = NIL AND l2 = NIL;
  END ListEquiv;

BEGIN
END RefListMisc.
