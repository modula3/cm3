GENERIC MODULE RefinableFunc(R,S,V,M);

PROCEDURE ClearArray(VAR x : V.TBody) =
  BEGIN
    FOR j:=FIRST(x) TO LAST(x) DO
      x[j] := R.Zero;
    END;
  END ClearArray;

PROCEDURE PlaceSubVector(VAR      y      : V.TBody;
                         READONLY x      : V.TBody;
                                  yStart : INTEGER) =
  VAR
    yStop := yStart + NUMBER(x);
    xl, xr, yl, yr : CARDINAL;
  BEGIN
    IF yStart < 0 THEN
      xl := -yStart;
      yl := 0;
    ELSIF yStart < NUMBER(y) THEN
      xl := 0;
      yl := yStart;
    ELSE
      ClearArray(y);
      RETURN;
    END;

    yStop := yStart+NUMBER(x);
    IF yStop < 0 THEN
      ClearArray(y);
      RETURN;
    ELSIF yStop < NUMBER(y) THEN
      xr := NUMBER(x);
      yr := yStop;
    ELSE
      xr := NUMBER(y)-yStart;
      yr := NUMBER(y);
    END;

    ClearArray(SUBARRAY(y,0,yl));
    SUBARRAY(y,yl,yr-yl) := SUBARRAY(x,xl,xr-xl);
    ClearArray(SUBARRAY(y,yr,NUMBER(y)-yr));
  END PlaceSubVector;

PROCEDURE TransitionMatrix(mask : S.T; shift : CARDINAL := 2) : M.T =
  VAR
    (*the size of the matrix is the minimum possible to avoid zeros
      on the diagonal from outside the support of the mask*)
    n := mask.getNumber() DIV (shift-1);
    v := mask.getData();
    z := M.NewZero(n,n);
  BEGIN
    FOR j:=0 TO n-1 DO
      PlaceSubVector(z[j],v^,j*shift-LAST(v^));
    END;
    RETURN z;
  END TransitionMatrix;

BEGIN
END RefinableFunc.
