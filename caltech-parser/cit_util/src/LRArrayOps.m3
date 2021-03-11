(* $Id$ *)

MODULE LRArrayOps;
IMPORT LongrealArraySort;

PROCEDURE Percentile(READONLY a : ARRAY OF LONGREAL; p : LONGREAL) : LONGREAL =
  (* a must be sorted *)
  VAR
    q := p * FLOAT(LAST(a),LONGREAL);
    n := TRUNC(q);
    np1 := n+1;
    w1 := q - FLOAT(n,LONGREAL);
    w0 := 1.0d0 - w1;
    res : LONGREAL;
  CONST
    EPS = 1.0d-6;
  BEGIN
    IF np1 <= LAST(a) THEN
      res := w0 * a[n] + w1 * a[np1];
    ELSE
      <* ASSERT n = LAST(a) *>
      <* ASSERT ABS(w1) < EPS *>
      res := a[n]
    END;   
    RETURN res   
  END Percentile;

PROCEDURE Interpolate(READONLY a : ARRAY OF LONGREAL; p : LONGREAL) : LONGREAL =
  VAR
    q := p;
    n := TRUNC(q);
    np1 := n+1;
    w1 := q - FLOAT(n,LONGREAL);
    w0 := 1.0d0 - w1;
    res : LONGREAL;
  CONST
    EPS = 1.0d-6;
  BEGIN
    <* ASSERT p <= FLOAT(LAST(a),LONGREAL) *>
    <* ASSERT p >= 0.0d0 *>
    IF np1 <= LAST(a) THEN
      res := w0 * a[n] + w1 * a[np1];
    ELSE
      <* ASSERT n = LAST(a) *>
      <* ASSERT ABS(w1) < EPS *>
      res := a[n]
    END;   
    RETURN res   
  END Interpolate;

PROCEDURE Sort(VAR a : ARRAY OF LONGREAL) =
  BEGIN LongrealArraySort.Sort(a) END Sort;

PROCEDURE TrimmedMean(READONLY a : ARRAY OF LONGREAL;
                      weight : LONGREAL) : LONGREAL =
  VAR 
    sum,cnt : LONGREAL;
  BEGIN
    <* ASSERT weight > 0.0d0 AND weight <= 1.0d0 *>
    WITH n = FLOAT(NUMBER(a),LONGREAL),
         lo = 0.50d0-weight/2.0d0,
         hi = 0.50d0+weight/2.0d0,

         p = lo * n,
         q = hi * n,

         pfl = FLOOR(p),     pflf = FLOAT(pfl,LONGREAL),
         qfl = CEILING(q)-1, qflf = FLOAT(qfl,LONGREAL),

         pfrac = 1.0d0 - (p - pflf),
         qfrac = q - qflf DO
      sum := pfrac*a[pfl];

      FOR i := pfl+1 TO qfl-1 DO
        sum := sum + a[i]
      END;

      sum := sum + qfrac*a[qfl];

      cnt := pfrac + (qflf-1.0d0)-(pflf+1.0d0)+1.0d0 + qfrac
    END;

    RETURN sum/cnt
  END TrimmedMean;

BEGIN END LRArrayOps.
