
MODULE SortedTableVBT;
IMPORT TableVBT, GridSplit;
IMPORT VBT;

REVEAL 
  Private = TableVBT.T BRANDED OBJECT END;
  T = Public BRANDED OBJECT 
  OVERRIDES
    insert_sorted := Insert;
  END;

PROCEDURE DefaultOrder(<*UNUSED*>v: T; 
                       <*UNUSED*>READONLY data: ARRAY OF VBT.T): INTEGER = 
  BEGIN
    RETURN 0;
  END DefaultOrder;

PROCEDURE Insert(v: T; READONLY data: ARRAY OF VBT.T): CARDINAL =
  VAR
    row := NEW(REF ARRAY OF VBT.T, NUMBER(data));
  BEGIN
    FOR i := v.numrows()-1 TO 0 BY -1 DO 
      GridSplit.GetRow(v.contents(), i, row^);
      IF v.order(v, data) > v.order(v, row^) THEN
        TableVBT.T.insert (v, i+1, data);
        RETURN i;
      END;
    END;
    TableVBT.T.insert(v, LAST(CARDINAL), data);
    RETURN v.numrows()-1;
  END Insert;

BEGIN
END SortedTableVBT.
