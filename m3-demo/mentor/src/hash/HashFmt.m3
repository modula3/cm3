(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Thu Jan  5 21:00:24 PST 1995 by najork                   *)
(*       Created on Fri Apr 29 15:05:10 PDT 1994 by najork                   *)


UNSAFE MODULE HashFmt;

IMPORT Fmt;
IMPORT FormsVBT AS FV;
IMPORT IntList AS IL;

PROCEDURE FormsVBT (<*UNUSED*> form : FV.T) : TEXT =
  BEGIN
    RETURN "<a FormsVBT.T>";
  END FormsVBT;


PROCEDURE IntList (l : IL.T) : TEXT =
  VAR 
    t := "";
  BEGIN
    WHILE l # NIL DO
      t := t & Fmt.Int (l.head);
      l := l.tail;
      IF l # NIL THEN 
        t := t & ","; 
      END;
    END;
    RETURN "[" & t & "]";
  END IntList;


BEGIN
END HashFmt.
