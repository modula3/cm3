MODULE PhysicalUnit;
(*Arithmetic for Modula-3, see doc for details

1/1/96  <name>    Initial version
*)

IMPORT (*IntIntTbl      AS Tbl,*)
       Integer32Basic AS I,
       RealBasic      AS R,
       RealTrans      AS RT;

FROM NADefinitions IMPORT Error,Err;

<*UNUSED*> CONST Module = "PhysicalUnit.";
(*==========================*)

PROCEDURE New () : T =
  BEGIN
    RETURN NEW(T).init(sizeHint:=10);
  END New;

PROCEDURE FromArray (READONLY x : ARRAY OF ExpType) : T =
  VAR
    y := New();
    replaced:BOOLEAN;
  BEGIN
    FOR j:=0 TO LAST(x) DO
      IF x[j]#0 THEN
        replaced:=y.put(j,x[j]);
        <*ASSERT NOT replaced*>
        (* resist writing
          <*ASSERT NOT y.put(j,x[j])*>
          because ASSERT statements may be removed
          for final executables
        *)
      END;
    END;
    RETURN y;
  END FromArray;

PROCEDURE Copy (x : T) : T =
  VAR
    y := New();
    it := x.iterate();
    unit,exp : INTEGER;
    replaced : BOOLEAN;
  BEGIN
    WHILE it.next(unit,exp) DO
      replaced:=y.put(unit,exp);
      <*ASSERT NOT replaced*>
    END;
    RETURN y;
  END Copy;

PROCEDURE Equal (x, y : T) : BOOLEAN =
  VAR
    it := x.iterate();
    unit,
    xexp,yexp : ExpType;
  BEGIN
    WHILE it.next(unit,xexp) DO
      IF NOT (y.get(unit,yexp) AND xexp=yexp) THEN
        RETURN FALSE;
      END;
    END;
    it := y.iterate();
    WHILE it.next(unit,yexp) DO
      IF NOT x.get(unit,xexp) THEN
        RETURN FALSE;
      END;
    END;
    RETURN TRUE;
  END Equal;

PROCEDURE IsZero (x : T) : BOOLEAN =
  BEGIN
    RETURN x.size()=0;
  END IsZero;

PROCEDURE Put (x : T; unit : INTEGER; exp : ExpType) =
  BEGIN
    IF exp#0 THEN
      VAR
        replaced:=x.put(unit,exp);
      BEGIN
        <*ASSERT NOT replaced*>
      END;
    END;
  END Put;

PROCEDURE Add  (x, y : T) : T =
  VAR
    z := New();
    it := x.iterate();
    unit,
    xexp,yexp : ExpType;
  BEGIN
    WHILE it.next(unit,xexp) DO
      yexp:=0;
      EVAL y.get(unit,yexp);
      Put(z,unit,xexp+yexp);
    END;
    it := y.iterate();
    WHILE it.next(unit,yexp) DO
      IF NOT x.get(unit,xexp) THEN
        Put(z,unit,yexp);
      END;
    END;
    RETURN z;
  END Add;

PROCEDURE Sub  (x, y : T) : T =
  VAR
    z := New();
    it := x.iterate();
    unit,
    xexp,yexp : ExpType;
  BEGIN
    WHILE it.next(unit,xexp) DO
      yexp:=0;
      EVAL y.get(unit,yexp);
      Put(z,unit,xexp-yexp);
    END;
    it := y.iterate();
    WHILE it.next(unit,yexp) DO
      IF NOT x.get(unit,xexp) THEN
        Put(z,unit,-yexp);
      END;
    END;
    RETURN z;
  END Sub;

PROCEDURE Neg  (x : T) : T =
  VAR
    y := New();
    it := x.iterate();
    unit,exp : ExpType;
    replaced:BOOLEAN;
  BEGIN
    WHILE it.next(unit,exp) DO
      replaced:=y.put(unit,-exp);
      <*ASSERT NOT replaced*>
    END;
    RETURN y;
  END Neg;

PROCEDURE Scale     (x : T; y : ExpType) : T =
  VAR
    z := New();
    it := x.iterate();
    unit,exp : ExpType;
    replaced:BOOLEAN;
  BEGIN
    IF y#0 THEN
      WHILE it.next(unit,exp) DO
        replaced:=z.put(unit,exp*y);
        <*ASSERT NOT replaced*>
      END;
    END;
    RETURN z;
  END Scale;

PROCEDURE ScaleDiv  (x : T; y : ExpType) : T RAISES {Error} =
  VAR
    z := New();
    it := x.iterate();
    unit,exp : ExpType;
    replaced:BOOLEAN;
  BEGIN
    WHILE it.next(unit,exp) DO
      replaced:=z.put(unit,I.Div(exp,y));
      <*ASSERT NOT replaced*>
    END;
    RETURN z;
  END ScaleDiv;

PROCEDURE ScaleReal (x : T; y : R.T) : T RAISES {Error} =
  VAR
    z := New();
    it := x.iterate();
    unit,xexp,zexp : ExpType;
    zexpr : R.T;
  BEGIN
    WHILE it.next(unit,xexp) DO
      zexpr := FLOAT(xexp,R.T) * y;
      zexp := ROUND(zexpr);
      IF ABS(FLOAT(zexp,R.T)-zexpr) > RT.Eps THEN
        RAISE Error(Err.indivisible);
      END;
      Put(z,unit,zexp);
    END;
    RETURN z;
  END ScaleReal;

PROCEDURE Norm1   (x : T) : ExpType =
  VAR
    it := x.iterate();
    unit,exp : ExpType;
    sum : ExpType := 0;
  BEGIN
    WHILE it.next(unit,exp) DO
      sum := sum+ABS(exp);
    END;
    RETURN sum;
  END Norm1;

PROCEDURE NormInf (x : T) : ExpType =
  VAR
    it := x.iterate();
    unit,exp : ExpType;
    max : ExpType := 0;
  BEGIN
    WHILE it.next(unit,exp) DO
      max := MAX(max,ABS(exp));
    END;
    RETURN max;
  END NormInf;

(*==========================*)
BEGIN
END PhysicalUnit.
