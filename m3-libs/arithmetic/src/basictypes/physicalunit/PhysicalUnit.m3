MODULE PhysicalUnit;
(*Copyright (c) 1996, m3na project

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
    RETURN NEW(T).init(sizeHint:=5);
  END New;

PROCEDURE Copy (x : T) : T =
  VAR
    y := New();
    it := x.iterate();
    unit,exp : INTEGER;
    success:BOOLEAN;
  BEGIN
    WHILE it.next(unit,exp) DO
      success:=y.put(unit,exp);
      <*ASSERT success*>
      (* resist writing
        <*ASSERT y.put(unit,exp)*>
        because ASSERT statements may be removed
        for final executables
      *)
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
        success:=x.put(unit,exp);
      BEGIN
        <*ASSERT success*>
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
    success:BOOLEAN;
  BEGIN
    WHILE it.next(unit,exp) DO
      success:=y.put(unit,-exp);
      <*ASSERT success*>
    END;
    RETURN y;
  END Neg;

PROCEDURE Scale     (x : T; y : ExpType) : T =
  VAR
    z := New();
    it := x.iterate();
    unit,exp : ExpType;
    success:BOOLEAN;
  BEGIN
    IF y#0 THEN
      WHILE it.next(unit,exp) DO
        success:=z.put(unit,exp*y);
        <*ASSERT success*>
      END;
    END;
    RETURN z;
  END Scale;

PROCEDURE ScaleDiv  (x : T; y : ExpType) : T RAISES {Error} =
  VAR
    z := New();
    it := x.iterate();
    unit,exp : ExpType;
    success:BOOLEAN;
  BEGIN
    WHILE it.next(unit,exp) DO
      success:=z.put(unit,I.Div(exp,y));
      <*ASSERT success*>
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

(*==========================*)
BEGIN
END PhysicalUnit.
