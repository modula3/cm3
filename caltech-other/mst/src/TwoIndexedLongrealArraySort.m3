MODULE TwoIndexedLongrealArraySort;
IMPORT TwoIndexedLongreal AS Elem;

PROCEDURE Sort (VAR a: ARRAY OF Elem.T; num : INTEGER) =
  BEGIN
    IF num = -1 THEN num := NUMBER(a) END;
    QuickSort (a, 0, num);
    InsertionSort (a, 0, num);
  END Sort;

PROCEDURE QuickSort (VAR a: ARRAY OF Elem.T;  lo, hi: INTEGER) =
  CONST CutOff = 9;
  VAR i, j: INTEGER;  key, tmp: Elem.T;
  BEGIN
    WHILE (hi - lo > CutOff) DO (* sort a[lo..hi) *)

      (* use median-of-3 to select a key *)
      i := (hi + lo) DIV 2;
      IF (a[lo].d < a[i].d) THEN
        IF (a[i].d < a[hi-1].d) THEN
          key := a[i];
        ELSIF (a[lo].d < a[hi-1].d) THEN
          key := a[hi-1];  a[hi-1] := a[i];  a[i] := key;
        ELSE
          key := a[lo];  a[lo] := a[hi-1];  a[hi-1] := a[i];  a[i] := key;
        END;
      ELSE (* a[lo] >= a[i] *)
        IF (a[hi-1].d < a[i].d) THEN
          key := a[i];  tmp := a[hi-1];  a[hi-1] := a[lo];  a[lo] := tmp;
        ELSIF (a[lo].d < a[hi-1].d) THEN
          key := a[lo];  a[lo] := a[i];  a[i] := key;
        ELSE
          key := a[hi-1];  a[hi-1] := a[lo];  a[lo] := a[i];  a[i] := key;
        END;
      END;

      (* partition the array *)
      i := lo+1;  j := hi-2;

      (* find the first hole *)
      WHILE (a[j].d > key.d) DO DEC (j) END;
      tmp := a[j];
      DEC (j);

      LOOP
        IF (i > j) THEN EXIT END;

        WHILE (a[i].d < key.d) DO INC (i) END;
        IF (i > j) THEN EXIT END;
        a[j+1] := a[i];
        INC (i);

        WHILE (a[j].d > key.d) DO DEC (j) END;
        IF (i > j) THEN  IF (j = i-1) THEN  DEC (j)  END;  EXIT  END;
        a[i-1] := a[j];
        DEC (j);
      END;

      (* fill in the last hole *)
      a[j+1] := tmp;
      i := j+2;

      (* then, recursively sort the smaller subfile *)
      IF (i - lo < hi - i)
        THEN  QuickSort (a, lo, i-1);   lo := i;
        ELSE  QuickSort (a, i, hi);     hi := i-1;
      END;

    END; (* WHILE (hi-lo > CutOff) *)
  END QuickSort;


PROCEDURE InsertionSort (VAR a: ARRAY OF Elem.T;  lo, hi: INTEGER) =
  VAR j: INTEGER;  key: Elem.T;
  BEGIN
    FOR i := lo+1 TO hi-1 DO
      key := a[i];
      j := i-1;
      WHILE (j >= lo) AND (key.d < a[j].d) DO
        a[j+1] := a[j];
        DEC (j);
      END;
      a[j+1] := key;
    END;
  END InsertionSort;

BEGIN
END TwoIndexedLongrealArraySort.
