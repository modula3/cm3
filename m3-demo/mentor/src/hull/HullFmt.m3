(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Tue May 24 09:00:30 PDT 1994 by najork     *)
(*      modified on Sat Oct 17 13:52:24 PDT 1992 by ramshaw    *)


MODULE HullFmt;

IMPORT Fmt;
IMPORT IntList AS IL;
IMPORT SiteList AS SL;

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


PROCEDURE SiteList (<* UNUSED *> l : SL.T) : TEXT =
  BEGIN 
    RETURN "<a SiteList.T>";
  END SiteList;

BEGIN
END HullFmt.
