(* $Id$ *)
MODULE FilledArray;
IMPORT IntPair;
IMPORT IntPairRouteIDTbl AS PairIDTbl;
IMPORT RouteID;

(* a quick and dirty implementation of the "filled" table with arrays *)

TYPE
  Status = { Filled, Unfilled, Unknown };

CONST Align = 2; (* SRC Modula-3 restriction *)

TYPE
  BaseIndex = [0..3];
  Base = ARRAY BaseIndex OF BITS Align FOR Status;
  Array = REF ARRAY OF ARRAY OF Base;

REVEAL
  T = Public BRANDED OBJECT
    default := Status.Unknown;
    data : Array;
    xoff, yoff : INTEGER;
    tbl : PairIDTbl.T;
  OVERRIDES
    (* must override all methods *)
    iterate := IteratorAbort;
    size := SizeAbort;
    get  := Get;
    put  := Put;
    init := Init;
    delete := Delete;
    haveDataAt := HaveDataAt;
  END;


PROCEDURE Init(self : T; defaultIsUnfilled : BOOLEAN) : T =
  BEGIN 
    IF defaultIsUnfilled THEN self.default := Status.Unfilled END;
    self.tbl := NEW(PairIDTbl.Default).init();
    RETURN self
  END Init;

PROCEDURE IteratorAbort(<*UNUSED*>self : T) : PairIDTbl.Iterator =
  BEGIN <* ASSERT FALSE *> END IteratorAbort;

PROCEDURE SizeAbort(<*UNUSED*>self : T) : CARDINAL =
  BEGIN <* ASSERT FALSE *> END SizeAbort;

PROCEDURE AllocIfNil(self : T; key : IntPair.T) =
  BEGIN
    <* ASSERT self.data = NIL *>
    self.data := NEW(Array, 1, 1);
    self.xoff := key.k1;
    self.yoff := key.k2 DIV 4;
    FOR k := FIRST(BaseIndex) TO LAST(BaseIndex) DO
      self.data[0,0,k] := self.default
    END
  END AllocIfNil; 

PROCEDURE Get(self : T; READONLY key : IntPair.T; VAR value : RouteID.T) : BOOLEAN =
  VAR
    xid, yid : INTEGER;
    res : BOOLEAN;
  BEGIN
    IF self.data = NIL THEN AllocIfNil(self,key) END;
    xid := key.k1 - self.xoff;
    yid := key.k2 DIV 4 - self.yoff;
    IF xid < 0 OR xid > LAST(self.data^) OR
       yid < 0 OR yid > LAST(self.data[0]) THEN
      res := FALSE
    ELSE
      res := self.data[xid,yid][key.k2 MOD 4] # self.default
    END;
    IF res THEN VAR x := self.tbl.get(key,value); BEGIN <*ASSERT x *> END END;
    RETURN res
  END Get;

PROCEDURE HaveDataAt(self : T; READONLY key : IntPair.T) : BOOLEAN =
  VAR
    xid, yid : INTEGER;
  BEGIN
    IF self.data = NIL THEN AllocIfNil(self,key) END;
    xid := key.k1 - self.xoff;
    yid := key.k2 DIV 4 - self.yoff;
    IF xid < 0 OR xid > LAST(self.data^) OR
       yid < 0 OR yid > LAST(self.data[0]) THEN
      RETURN FALSE
    ELSE
      RETURN self.data[xid,yid][key.k2 MOD 4] # self.default
    END;
  END HaveDataAt;

PROCEDURE Delete(self : T; 
                 READONLY key : IntPair.T; VAR value : RouteID.T) : BOOLEAN =
  VAR
    res := Get(self,key,value);
    xid := key.k1 - self.xoff;
    yid := key.k2 DIV 4 - self.yoff;
  BEGIN
    IF res THEN
      self.data[xid,yid][key.k2 MOD 4] := self.default
    END;

    VAR  
      x:= self.tbl.delete(key,value);
    BEGIN 
      <* ASSERT x = res *>
    END;

    RETURN res
  END Delete;

PROCEDURE Put(self : T; READONLY key : IntPair.T; 
              READONLY value : RouteID.T) : BOOLEAN =
  VAR
    xid, yid : INTEGER;
  BEGIN
    IF self.data = NIL THEN AllocIfNil(self,key) END;
    xid := key.k1 - self.xoff;
    yid := key.k2 DIV 4 - self.yoff;
    IF xid < 0 OR xid > LAST(self.data^) OR
       yid < 0 OR yid > LAST(self.data[0]) THEN
      Extend(self, key);
      xid := key.k1 - self.xoff;
      yid := key.k2 DIV 4 - self.yoff
    END;

    (* this is a bit odd.. *)
    EVAL self.tbl.put(key,value);

    VAR
      res := self.data[xid,yid][key.k2 MOD 4] # self.default;
    BEGIN
      self.data[xid,yid][key.k2 MOD 4] := Status.Filled;
      RETURN res
    END
  END Put;
    
(* still absolute addressing: *)
PROCEDURE Extend(self : T; key : IntPair.T) =
  VAR
    xid := key.k1 - self.xoff;
    yid : INTEGER;
  BEGIN
    IF xid < 0 OR xid > LAST(self.data^) THEN ExtendX(self, xid) END;

    yid := key.k2 DIV 4 - self.yoff;
    IF yid < 0 OR yid > LAST(self.data[0]) THEN ExtendY(self, yid) END;
  END Extend;

(* these things work on relative addressing: *)
PROCEDURE ExtendX(self : T; x : INTEGER) =
  VAR
    new : Array;
    m := NUMBER(self.data[0]);
  BEGIN
    IF x < 0 THEN  
      new := NEW(Array, NUMBER(self.data^) - x, m);

      FOR i := FIRST(new^) TO -x - 1 DO
        FOR j := 0 TO m - 1 DO 
          FOR k := FIRST(BaseIndex) TO LAST(BaseIndex) DO
            new[i,j,k] := self.default
          END
        END
      END;

      FOR i := -x TO LAST(new^) DO
        new[i] := self.data[i + x]
      END;

      self.xoff := self.xoff + x
    ELSE
      <* ASSERT x > LAST(self.data^) *>
      x := x + x DIV 4; (* allocate a bit more than we need *)
      new := NEW(Array, x + 1, m);
      FOR i := FIRST(self.data^) TO LAST(self.data^) DO
        new[i] := self.data[i]
      END;
      FOR i := LAST(self.data^) + 1 TO LAST(new^) DO
        FOR j := 0 TO m - 1 DO 
          FOR k := FIRST(BaseIndex) TO LAST(BaseIndex) DO
            new[i,j,k] := self.default
          END
        END
      END
    END;
    self.data := new;
  END ExtendX;

PROCEDURE ExtendY(self : T; y : INTEGER) =
  VAR
    new : Array;
    n := NUMBER(self.data^);
  BEGIN
    IF y < 0 THEN  
      new := NEW(Array, n, NUMBER(self.data[0]) - y);

      FOR i := 0 TO n - 1 DO
        FOR j := 0 TO -y - 1 DO 
          FOR k := FIRST(BaseIndex) TO LAST(BaseIndex) DO
            new[i,j,k] := self.default
          END
        END;
        FOR j := -y TO LAST(new[0]) DO new[i,j] := self.data[i,j + y] END
      END;

      self.yoff := self.yoff + y
    ELSE
      <* ASSERT y > LAST(self.data[0]) *>
      new := NEW(Array, n, y + 1);
      FOR i := 0 TO n - 1 DO
        FOR j := 0 TO LAST(self.data[0]) DO new[i,j] := self.data[i,j] END;
        FOR j := LAST(self.data[0]) + 1 TO y DO
          FOR k := FIRST(BaseIndex) TO LAST(BaseIndex) DO
            new[i,j,k] := self.default
          END
        END
      END
    END;
    self.data := new;
  END ExtendY;

BEGIN END FilledArray.
