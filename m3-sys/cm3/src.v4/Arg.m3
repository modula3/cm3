(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Wed Jul  6 16:22:57 PDT 1994 by kalsow     *)

MODULE Arg;

PROCEDURE NewList (): List =
  BEGIN
    RETURN NEW (List);
  END NewList;

PROCEDURE Append (list: List;  val: TEXT) =
  VAR n := NEW (T, next := NIL, arg := val);
  BEGIN
    IF (list.head = NIL)
      THEN list.head := n;
      ELSE list.tail.next := n;
    END;
    list.tail := n;
    INC (list.cnt);
  END Append;

PROCEDURE Prepend (list: List;  val: TEXT) =
  VAR n := NEW (T, next := list.head, arg := val);
  BEGIN
    IF (list.tail = NIL) THEN list.tail := n END;
    list.head := n;
    INC (list.cnt);
  END Prepend;

PROCEDURE AppendL (a, b: List) =
  VAR n := b.head;
  BEGIN
    WHILE (n # NIL) DO
      Append (a, n.arg);
      n := n.next;
    END;
  END AppendL;

PROCEDURE Pop (list: List): TEXT =
  VAR txt: TEXT;
  BEGIN
    IF (list = NIL) OR (list.cnt <= 0) THEN RETURN NIL END;
    txt := list.head.arg;
    list.head := list.head.next;
    DEC (list.cnt);
    RETURN txt;
  END Pop;

PROCEDURE Flatten (list: List;  other: TEXT): REF ARRAY OF TEXT =
  VAR n := 0;  array: REF ARRAY OF TEXT;  t: T;
  BEGIN
    IF (list # NIL) AND (list.cnt > 0) THEN INC (n, list.cnt); END;
    IF (other # NIL) THEN INC (n); END;
    array := NEW (REF ARRAY OF TEXT, n);
    n := 0;
    IF (list # NIL) THEN
      t := list.head;
      WHILE (t # NIL) DO
        array [n] := t.arg;  INC (n);
        t := t.next;
      END;
    END;
    IF (other # NIL) THEN
      array [n] := other;  INC (n);
    END;
    RETURN array;
  END Flatten;

BEGIN
END Arg.
