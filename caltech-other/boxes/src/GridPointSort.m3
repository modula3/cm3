(* $Id$ *)

MODULE GridPointSort;
IMPORT GridPoint AS Elem;

PROCEDURE Sort (VAR a: ARRAY OF Elem.T; 
                lo, hi : CARDINAL;
                cmp : Comparer) =
  BEGIN
    QuickSort (a, lo, hi + 1, cmp);
    InsertionSort (a, lo, hi + 1, cmp);
  END Sort;

PROCEDURE QuickSort (VAR a: ARRAY OF Elem.T;  lo, hi: CARDINAL; c : Comparer) =
  CONST CutOff = 9;
  VAR i, j: INTEGER;  key, tmp: Elem.T;
  BEGIN
    WHILE (hi - lo > CutOff) DO (* sort a[lo..hi) *)

      (* use median-of-3 to select a key *)
      i := (hi + lo) DIV 2;
      IF c(a[lo],a[i])=-1 THEN
        IF c(a[i],a[hi-1])=-1 THEN
          key := a[i];
        ELSIF c(a[lo],a[hi-1])=-1 THEN
          key := a[hi-1];  a[hi-1] := a[i];  a[i] := key;
        ELSE
          key := a[lo];  a[lo] := a[hi-1];  a[hi-1] := a[i];  a[i] := key;
        END;
      ELSE (* a[lo] >= a[i] *)
        IF c(a[hi-1] , a[i])=-1 THEN
          key := a[i];  tmp := a[hi-1];  a[hi-1] := a[lo];  a[lo] := tmp;
        ELSIF c(a[lo] , a[hi-1])=-1 THEN
          key := a[lo];  a[lo] := a[i];  a[i] := key;
        ELSE
          key := a[hi-1];  a[hi-1] := a[lo];  a[lo] := a[i];  a[i] := key;
        END;
      END;

      (* partition the array *)
      i := lo+1;  j := hi-2;

      (* find the first hole *)
      WHILE c(a[j] , key)=1 DO DEC (j) END;
      tmp := a[j];
      DEC (j);

      LOOP
        IF (i > j) THEN EXIT END;

        WHILE c(a[i] , key)=-1 DO INC (i) END;
        IF (i > j) THEN EXIT END;
        a[j+1] := a[i];
        INC (i);

        WHILE c(a[j] , key)=1 DO DEC (j) END;
        IF (i > j) THEN  IF (j = i-1) THEN  DEC (j)  END;  EXIT  END;
        a[i-1] := a[j];
        DEC (j);
      END;

      (* fill in the last hole *)
      a[j+1] := tmp;
      i := j+2;

      (* then, recursively sort the smaller subfile *)
      IF (i - lo < hi - i)
        THEN  QuickSort (a, lo, i-1,c);   lo := i;
        ELSE  QuickSort (a, i, hi,c);     hi := i-1;
      END;

    END; (* WHILE (hi-lo > CutOff) *)
  END QuickSort;


PROCEDURE InsertionSort (VAR a: ARRAY OF Elem.T;  lo, hi: CARDINAL; c : Comparer) =
  VAR j: INTEGER;  key: Elem.T;
  BEGIN
    FOR i := lo+1 TO hi-1 DO
      key := a[i];
      j := i-1;
      WHILE (j >= lo) AND c(key , a[j])=-1 DO
        a[j+1] := a[j];
        DEC (j);
      END;
      a[j+1] := key;
    END;
  END InsertionSort;

BEGIN
END GridPointSort.
