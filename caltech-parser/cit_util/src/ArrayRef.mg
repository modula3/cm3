GENERIC MODULE ArrayRef(Elem);

PROCEDURE Cat(READONLY a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,
              q,r,s,t,u,v,w,x,y,z := ARRAY OF Elem.T{}): T =
  VAR
    size, totalSize := 0;
    result: T := NIL;
    index := 0;
  PROCEDURE D(READONLY array := ARRAY OF Elem.T{}) =
    BEGIN
      size := NUMBER(array);
      IF result # NIL THEN
        SUBARRAY(result^, totalSize, size) := array;
      END;
      INC(totalSize, size);
    END D;
  PROCEDURE Scan() =
    BEGIN
      CASE index OF
      | 0 => D(a) | 1 => D(b) | 2 => D(c) | 3 => D(d) | 4 => D(e)
      | 5 => D(f) | 6 => D(g) | 7 => D(h) | 8 => D(i) | 9 => D(j)
      |10 => D(k) |11 => D(l) |12 => D(m) |13 => D(n) |14 => D(o)
      |15 => D(p) |16 => D(q) |17 => D(r) |18 => D(s) |19 => D(t)
      |20 => D(u) |21 => D(v) |22 => D(w) |23 => D(x) |24 => D(y)
      |25 => D(z)
      ELSE
        D();
      END;
    END Scan;
  BEGIN
    REPEAT
      Scan();
      INC(index);
    UNTIL size = 0;
    result := NEW(T, totalSize);
    totalSize := 0;
    index := 0;
    REPEAT
      Scan();
      INC(index);
    UNTIL size = 0;
    RETURN result;
  END Cat;

BEGIN
END ArrayRef.
