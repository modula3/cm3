(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE M3PathDefaults;

IMPORT Process, OSError;
IMPORT M3PathElemList, M3PathElem;

VAR
  currentElem_g: M3PathElem.T := NIL;

PROCEDURE EnsureCurrentFirst(l: M3PathElemList.T): M3PathElemList.T=
  VAR t := l;
  <* FATAL OSError.E *>
  BEGIN
    IF currentElem_g = NIL THEN
      WITH cd = Process.GetWorkingDirectory() DO
        currentElem_g := M3PathElem.FromText(cd, cd);
      END;
    END;
    WHILE t # NIL DO
      IF t.head = currentElem_g THEN
        (* delete it and move it to front *)
        l := M3PathElemList_DeleteD(l, currentElem_g);
        EXIT;
      ELSE
        t := t.tail;
      END;
    END;
    RETURN M3PathElemList.Cons(currentElem_g, l)
  END EnsureCurrentFirst;

PROCEDURE Add(l: M3PathElemList.T; d: M3PathElem.T): M3PathElemList.T=
  VAR t := l; 
  BEGIN
    WHILE t # NIL DO
      IF t.head = d THEN
        RETURN l;
      ELSE
        t := t.tail;
      END;
    END;
    RETURN M3PathElemList.AppendD(l, M3PathElemList.List1(d));
  END Add;

PROCEDURE M3PathElemList_DeleteD(list: M3PathElemList.T;
                                 x: REFANY): M3PathElemList.T=
  VAR result, t: M3PathElemList.T;
  BEGIN
    IF list.head = x THEN result := list.tail
    ELSE
      result := list; t := list; list := list.tail;
      WHILE list # NIL DO
        IF list.head = x THEN t.tail := list.tail; EXIT
        ELSE t := list; list := list.tail;
        END;
      END;
    END;
    RETURN result;
  END M3PathElemList_DeleteD;

BEGIN
END M3PathDefaults.

