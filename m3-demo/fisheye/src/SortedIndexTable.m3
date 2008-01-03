(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Jun 16 16:46:20 PDT 1992 by muller                   *)

MODULE SortedIndexTable;

IMPORT SortedHashTable, PrintUtil;

CONST
     NULL_KEY: REAL = -1.0;
     NULL_DATA: INTEGER = -1;


PROCEDURE New(size: INTEGER): T =
  BEGIN
    RETURN NEW(T, number := 0, items := NEW(REF ARRAY OF Item, size));
  END New;


PROCEDURE Clear(table: T) =
  BEGIN
    FOR i := 0 TO NUMBER(table^.items^)-1 DO
      table^.items^[i].key := NULL_KEY;
      table^.items^[i].data := NULL_DATA;
    END;
    table^.number := 0;
  END Clear;


PROCEDURE Insert(table: T; item: Item): BOOLEAN =
  VAR
    i: INTEGER;
    size: INTEGER;
    found: BOOLEAN;
  BEGIN
    size := NUMBER(table^.items^);
    IF table^.number >= size THEN RETURN FALSE; END;

    i := 0;
    found := FALSE;
    WHILE (NOT(found) AND (i < (table^.number-1))) DO
      IF (table^.items^[i].data = item.data) THEN
        found := TRUE;
      ELSE
        i := i + 1;
      END;
    END;

    IF NOT found THEN
      table^.items^[table^.number] := item;
      table^.number := table^.number + 1;
    END;

    RETURN TRUE;
  END Insert;


PROCEDURE CopySortedIndexTable(
     fromSortedIndexTable: T;
     toSortedIndexTable: T; 
     n: INTEGER): BOOLEAN =
  BEGIN
    IF ((n <= fromSortedIndexTable^.number) AND
        (n <= NUMBER(toSortedIndexTable^.items^))) THEN
      FOR i := 0 TO (n-1) DO
        toSortedIndexTable^.items^[i] := fromSortedIndexTable^.items^[i]; 
      END;
      toSortedIndexTable^.number := n;
      RETURN TRUE;
    ELSE
      RETURN FALSE;
    END;
  END CopySortedIndexTable;


PROCEDURE Reverse(table: T) =
  VAR
    item: Item;
    n,j: INTEGER;
  BEGIN
     n := table^.number DIV 2;
     FOR i := 0 TO (n - 1 ) DO
       j := (table^.number - 1) - i;
       item := table^.items[i];
       table^.items[i] := table^.items[j];
       table^.items[j] := item;
     END;
  END Reverse;


PROCEDURE CopySortedHashTable(fromSortedHashTable: SortedHashTable.T; 
                            toSortedIndexTable: T; n: INTEGER): BOOLEAN =
  VAR
    i,j: INTEGER;
    bound, buckets: INTEGER;
    head: REF SortedHashTable.ItemNode;
  BEGIN

    bound := NUMBER(toSortedIndexTable^.items^);
    buckets := NUMBER(fromSortedHashTable^);
    IF (n <= bound) THEN
      i := 0;
      j := 0;
      WHILE ((j < buckets) AND (i < n)) DO
        head := fromSortedHashTable^[j];
        WHILE head # NIL DO
          IF (i >= bound) THEN RETURN FALSE; END;
          toSortedIndexTable^.items^[i].key := head^.key;
          toSortedIndexTable^.items^[i].data := head^.data;
          i := i + 1;
          head := head^.next;
        END;
        j := j + 1;
      END;
        toSortedIndexTable^.number := i;
        RETURN TRUE;
    ELSE
      RETURN FALSE;
    END;
  END CopySortedHashTable;

PROCEDURE Print(table: T) =
  BEGIN
    PrintUtil.PrintInt("Total entries ", table^.number);
    PrintUtil.NewLine();
    FOR i := 0 TO table^.number-1 DO
      PrintUtil.PrintInt("Index =", table^.items[i].data);
      PrintUtil.PrintReal(" Value =", table^.items[i].key);
      PrintUtil.NewLine();
    END;
  END Print;


BEGIN
END SortedIndexTable.
    
  






