(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Jun 16 16:46:21 PDT 1992 by muller                   *)

MODULE SortedHashTable;


PROCEDURE New(size: INTEGER): T =
  BEGIN
    RETURN NEW(T, size);
  END New;


PROCEDURE Clear(table: T) =
  BEGIN
    FOR i := 0 TO NUMBER(table^)-1 DO
      table^[i] := NIL;
    END;
  END Clear;


PROCEDURE Insert(table: T; new_key: REAL; new_data: INTEGER): BOOLEAN =
  VAR
    head, tail, node: REF ItemNode;
    index: INTEGER;
    done: BOOLEAN;
  BEGIN
    index := FLOOR(new_key * FLOAT(NUMBER(table^) - 1));
    IF index > NUMBER(table^) - 1 THEN
      RETURN FALSE;
    ELSIF index < 0 THEN
      RETURN FALSE;
    END;
    head := table^[index];
    tail := head;
    node := NEW(REF ItemNode, key:=new_key, data:=new_data, next:=NIL);

    done := FALSE;
    WHILE (head # NIL) AND (NOT done) DO
      IF head^.key >= new_key THEN
        IF tail = head THEN
          table^[index] := node;
          node^.next := tail;
        ELSE
            tail^.next := node;
            node^.next := head;
        END;
        done := TRUE;             (* to make sure the loop terminates *)
      ELSE
        tail := head;
        head := head^.next
      END
    END;

    IF (NOT done) THEN
      IF tail = NIL THEN
        table^[index] := node;
      ELSE
        tail^.next := node;
      END;
    END;
    RETURN TRUE;
  END Insert;

BEGIN
END SortedHashTable.
    
  






