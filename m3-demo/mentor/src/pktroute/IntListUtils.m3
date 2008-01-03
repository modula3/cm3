(* Copyright 1993 Digital Equipment Corporation.                             *)
(* Distributed only by permission.                                           *)
(*                                                                           *)
(* Last modified on Sat Jul 17 16:40:06 PDT 1993 by heydon                   *)

MODULE IntListUtils;

IMPORT IntList, Fmt;

PROCEDURE ToText(l: IntList.T): TEXT =
  VAR res := "["; BEGIN
    WHILE l # NIL DO
      res := res & Fmt.Int(l.head);
      l := l.tail;
      IF l # NIL THEN res := res & ", " END
    END;
    RETURN res & "]"
  END ToText;

BEGIN
END IntListUtils.
