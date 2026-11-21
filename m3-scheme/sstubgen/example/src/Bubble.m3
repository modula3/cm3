(* $Id: Bubble.m3,v 1.1 2009/04/16 09:43:18 mika Exp $ *)

MODULE Bubble;

PROCEDURE Sort(VAR a : ARRAY OF INTEGER) =
  BEGIN
    FOR i := 0 TO LAST(a) DO
      FOR j := 0 TO LAST(a)-i-1 DO
        IF a[j] > a[j+1] THEN 
          Swap(a[j],a[j+1])
        END
      END
    END
  END Sort;

PROCEDURE Swap(VAR a, b : INTEGER) =
  VAR t := a;
  BEGIN
    a := b;
    b := t
  END Swap;

BEGIN END Bubble.
