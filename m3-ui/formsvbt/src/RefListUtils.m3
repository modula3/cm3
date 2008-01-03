(* Copyright © 1993, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Jun 28 09:00:16 PDT 1993 by steveg                   *)
(*      modified on Thu Jun  3 21:56:35 PDT 1993 by meehan                   *)

MODULE RefListUtils;

IMPORT Atom, RefList, Text;

PROCEDURE Pop (VAR list: RefList.T): REFANY =
  VAR car := list.head;
  BEGIN
    list := list.tail;
    RETURN car
  END Pop;

PROCEDURE Push (VAR list: RefList.T; item: REFANY) =
  BEGIN
    list := RefList.Cons (item, list)
  END Push;

PROCEDURE Assoc (z: RefList.T; item: REFANY): RefList.T =
  VAR pair: RefList.T;
  BEGIN
    WHILE z # NIL DO
      pair := Pop (z);
      IF Equal (pair.head, item) THEN RETURN pair END
    END;
    RETURN NIL
  END Assoc;

PROCEDURE AssocQ (z: RefList.T; item: REFANY): RefList.T =
  VAR pair: RefList.T;
  BEGIN
    WHILE z # NIL DO
      pair := Pop (z);
      IF pair.head = item THEN RETURN pair END
    END;
    RETURN NIL
  END AssocQ;

PROCEDURE Equal (a, b: REFANY): BOOLEAN =
  BEGIN
    IF a = b THEN RETURN TRUE END;
    IF a = NIL OR b = NIL THEN RETURN FALSE END;
    TYPECASE a OF
    | REF CHAR (ar) =>
        TYPECASE b OF
          REF CHAR (br) => RETURN ar^ = br^
        ELSE
          RETURN FALSE
        END
    | REF INTEGER (ar) =>
        TYPECASE b OF
          REF INTEGER (br) => RETURN ar^ = br^
        ELSE
          RETURN FALSE
        END
    | REF REAL (ar) =>
        TYPECASE b OF
          REF REAL (br) => RETURN ar^ = br^
        ELSE
          RETURN FALSE
        END
    | REF LONGREAL (ar) =>
        TYPECASE b OF
          REF LONGREAL (br) => RETURN ar^ = br^
        ELSE
          RETURN FALSE
        END
    | REF EXTENDED (ar) =>
        TYPECASE b OF
          REF EXTENDED (br) => RETURN ar^ = br^
        ELSE
          RETURN FALSE
        END
    | Atom.T => RETURN FALSE
    | Text.T (ta) =>
        TYPECASE b OF
        | TEXT (tb) => RETURN Text.Equal(ta, tb)
        ELSE
          RETURN FALSE
        END;
    | RefList.T (ar) =>
        TYPECASE b OF
        | RefList.T (br) =>
            VAR al := RefList.Length(ar);
            BEGIN
              IF al # RefList.Length(br) THEN RETURN FALSE END;
              WHILE ar # NIL DO
                IF NOT Equal(Pop(ar), Pop(br)) THEN RETURN FALSE END
              END;
              RETURN TRUE
            END
        ELSE
          RETURN FALSE
        END
    ELSE
      RETURN FALSE
    END
  END Equal;


PROCEDURE SetNth (list: RefList.T; n: CARDINAL; item: REFANY) =
  BEGIN
    LOOP
      IF list = NIL THEN
        RETURN
      ELSIF n = 0 THEN
        list.head := item;
        RETURN
      ELSE
        list := list.tail;
        DEC (n)
      END
    END
  END SetNth;

PROCEDURE NthTail (l: RefList.T; n: CARDINAL): RefList.T =
  BEGIN
    WHILE n > 0 AND l # NIL DO l := l.tail; DEC (n) END;
    RETURN l
  END NthTail;

PROCEDURE Delete (VAR list: RefList.T; item: REFANY) =
  VAR l, t: RefList.T;
  BEGIN
    LOOP
      IF list = NIL THEN
        RETURN
      ELSIF Equal (list.head, item) THEN
        list := list.tail
      ELSE
        l := list;
        LOOP
          t := l.tail;
          IF t = NIL THEN
            RETURN
          ELSIF Equal (t.head, item) THEN
            l.tail := t.tail
          ELSE
            l := t
          END
        END
      END
    END;
  END Delete;

PROCEDURE DeleteQ (VAR list: RefList.T; item: REFANY) =
  VAR l, t: RefList.T;
  BEGIN
    LOOP
      IF list = NIL THEN
        RETURN
      ELSIF Equal (list.head, item) THEN
        list := list.tail
      ELSE
        l := list;
        LOOP
          t := l.tail;
          IF t = NIL THEN
            RETURN
          ELSIF t.head = item THEN
            l.tail := t.tail
          ELSE
            l := t
          END
        END
      END
    END;
  END DeleteQ;




BEGIN
END RefListUtils.
