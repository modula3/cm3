(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Last modified on Thu Sep 22 19:40:50 PDT 1994 by heydon     *)
(*      modified on Thu Apr 22 08:34:33 PDT 1993 by mcjones    *)
(*      modified on Wed Feb 17 21:55:29 PST 1993 by mjordan    *)

GENERIC MODULE ListSort(Elem);

PROCEDURE Sort(l: T; c := Elem.Compare): T=
  BEGIN
    RETURN SortD(Copy(l), c);
  END Sort;

PROCEDURE SortD(l: T; c := Elem.Compare): T=
  VAR
    l1, l2, lm, lmHead: T;
    i, iHigh: CARDINAL;
    a: ARRAY [0..27] OF T;
    (* a[i] is a sorted list of length 0 or 2^(i+1).  Hence when a
       fills up, there are 2^(HIGH(a)+2)-1 list cells allocated, at
       least 8 bytes each. *)
  BEGIN
    iHigh := 0;
    lmHead := NEW(T);

    (* dismantle l, filling a *)
    LOOP
      (* merge two length-one lists into l1 *)
      l1 := l;
      IF l1 = NIL THEN EXIT; END;
      l2 := l1.tail;
      IF l2 = NIL THEN EXIT; END;
      l := l2.tail;
      IF c( l1.head, l2.head ) = -1 THEN
          l1.tail := l2;  l2.tail := NIL;
      ELSE
          l2.tail := l1;  l1.tail := NIL;  l1 := l2;
      END;
      
      (* l1 is a sorted length-two list; merge into a *)
      i := 0;
      LOOP
        l2 := a[i];
        IF l2 = NIL THEN
          a[i] := l1;
          EXIT
        ELSE
          (* merge equal-length sorted lists l1 and l2 *)
          a[i] := NIL;
          lm := lmHead;
          LOOP
            <* ASSERT l1 # NIL AND l2 # NIL *>
            IF c( l1.head, l2.head ) = -1 THEN
              lm.tail := l1;  lm := l1;  l1 := l1.tail;
              IF l1 = NIL THEN  lm.tail := l2; EXIT END;
            ELSE
              lm.tail := l2;  lm := l2;  l2 := l2.tail;
              IF l2 = NIL THEN  lm.tail := l1; EXIT END;
            END;
          END; (*LOOP*)
          l1 := lmHead.tail;
          INC(i);
          IF i > iHigh THEN iHigh := i END;
        END
      END (*LOOP*)
    END; (*LOOP*)

    (* l1 is a list of length 0 or 1; merge l1 and a[0..iHigh] into l1 *)
    i := 0;
    IF l1 = NIL THEN
      WHILE (a[i] = NIL) AND (i # iHigh) DO INC(i) END;
      l1 := a[i];
      INC(i);
    END;
    
    (* l1 # NIL or i > iHigh *)
    WHILE i <= iHigh DO
      l2 := a[i];
      IF l2 # NIL THEN
        lm := lmHead;
        LOOP
          IF c( l1.head, l2.head ) = -1 THEN
            lm.tail := l1;  lm := l1;  l1 := l1.tail;
            IF l1 = NIL THEN lm.tail := l2; EXIT END;
          ELSE
            lm.tail := l2;  lm := l2;  l2 := l2.tail;
            IF l2 = NIL THEN lm.tail := l1; EXIT END;
          END
        END; (*LOOP*)
        l1 := lmHead.tail
      END;
      INC(i)
    END;

    RETURN l1;
  END SortD;

PROCEDURE Copy(l: T): T RAISES {} =
  VAR last, rest, result: T; BEGIN
    IF l = NIL THEN RETURN NIL; END;
    result := NEW( T, head := l.head, tail := NIL );
    last := result;
    rest := l.tail;
    WHILE rest # NIL DO
      last.tail := NEW(T, head := rest.head, tail := NIL);
      last := last.tail;
      rest := rest.tail;
    END;
    RETURN result;
  END Copy;

BEGIN
END ListSort.

