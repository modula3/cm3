(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE M3PathElem;

IMPORT M3PathElemOS, M3PathElemList, Process, Text;

IMPORT OSError;
<*FATAL OSError.E*>

VAR
  elemTable_g: M3PathElemList.T := NIL;

REVEAL
  T = Public BRANDED OBJECT
    elemText: TEXT;
    elemUnexpanded: TEXT;
    elemReadOnly: BOOLEAN := FALSE;
    uid: M3PathElemOS.T;
  OVERRIDES
    text := ElemText;
    unexpanded := ElemUnexpanded;
    readOnly := ElemReadOnly;
    setReadOnly := SetReadOnly
  END;

PROCEDURE ElemText(t: T): TEXT=
  BEGIN
    RETURN t.elemText;
  END ElemText;

PROCEDURE ElemUnexpanded(t: T): TEXT=
  BEGIN
    RETURN t.elemUnexpanded;
  END ElemUnexpanded;

PROCEDURE ElemReadOnly(t: T): BOOLEAN=
  BEGIN
    RETURN t.elemReadOnly;
  END ElemReadOnly;

PROCEDURE Equal(e1, e2: T): BOOLEAN=
  BEGIN
    RETURN e1 = e2;
  END Equal;

PROCEDURE FromText(expanded, unexpanded: TEXT; readOnly := FALSE): T=
  VAR
    uid: M3PathElemOS.T;
    list := elemTable_g;
    result: T := NIL;
  BEGIN
    IF Text.Equal(expanded, CurrentDir) THEN
      expanded := Process.GetWorkingDirectory()
    END;
    uid := M3PathElemOS.Uid(expanded);
    WHILE list # NIL DO
      IF M3PathElemOS.Equal(list.head.uid, uid) THEN
        result := list.head;
        EXIT;
      ELSE
        list := list.tail;
      END;
    END;
    IF result = NIL THEN
      result := NEW(T, elemText := expanded, elemUnexpanded := unexpanded,
                    uid := uid, elemReadOnly := readOnly);
      elemTable_g := M3PathElemList.Cons(result, elemTable_g);
    END;
    RETURN result;
  END FromText;

PROCEDURE SetReadOnly(elem: T; ro := TRUE)=
  BEGIN
    elem.elemReadOnly := ro;
  END SetReadOnly;

BEGIN
END M3PathElem.
