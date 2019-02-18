(* $Id$ *)

MODULE LongrealSort;
IMPORT LongrealArraySort;
IMPORT LongFloat;

PROCEDURE Sort(VAR a: ARRAY OF LONGREAL; 
               cmp : Compar;
               treatNanAs : Mode) =
  VAR
    nan : LONGREAL;
  BEGIN 
    IF treatNanAs = Mode.DontTouch THEN
      LongrealArraySort.Sort(a,cmp)
    ELSE
      FOR i := FIRST(a) TO LAST(a) DO
        IF LongFloat.IsNaN(a[i]) THEN
          nan := a[i];
          CASE treatNanAs OF
            Mode.Largest => a[i] := LAST(LONGREAL)
          |
            Mode.Smallest => a[i] := FIRST(LONGREAL)
          ELSE
            <* ASSERT FALSE *>
          END
        END
      END;

      LongrealArraySort.Sort(a,cmp);

      FOR i := FIRST(a) TO LAST(a) DO
        CASE treatNanAs OF
          Mode.Largest => IF a[i] = LAST(LONGREAL) THEN a[i] := nan END
        |
          Mode.Smallest=> IF a[i] = FIRST(LONGREAL) THEN a[i] := nan END
        ELSE
          <* ASSERT FALSE *>
        END
      END
    END
  END Sort;

BEGIN END LongrealSort.
