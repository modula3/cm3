(* $Id$ *)

MODULE BoolIntegerImpl EXPORTS BoolInteger;
IMPORT Bool, BoolRemap, BoolSet, Word, BoolSetDef, BoolIntegerTbl, IntBoolIntegerTbl;

TYPE Array = REF ARRAY OF Bool.T;

REVEAL 
  T = Private BRANDED Brand OBJECT
    (* two's complement *)
    bits : Array; (* CONST *)
    id : Word.T;
  METHODS
    (* extend to width *)
    extend(nbits : CARDINAL) := SignExtend;

    (* pack into minimum req'd width; leave a "guard bit" so we know
       the sign of the number *)
    pack() := Pack;
    setId() : T := SetId;
  OVERRIDES
    isConstant := IsConstant;
    extract := Extract;
    repbits := RepBits;
    remap := Remap;
    vars := Vars;
    substitute := Substitute;
  END;

REVEAL
  FreeVariable = PublicFreeVariable BRANDED Brand & "Free Variable" OBJECT
    baseBits : Array;
  OVERRIDES
    init := Init;
    remap := FreeRemap;
    clone := Clone;
    makemap := Makemap;
    isRepBaseBit := FVIsRepBaseBit;
    extractBaseBit := ExtractBaseBit;
  END;
  
PROCEDURE FVIsRepBaseBit(self : FreeVariable; 
                         b : Bool.T; VAR which : CARDINAL) : BOOLEAN =
  BEGIN
    FOR i := FIRST(self.baseBits^) TO LAST(self.baseBits^) DO
      IF b = self.baseBits[i] THEN which := i; RETURN TRUE END
    END;
    RETURN FALSE
  END FVIsRepBaseBit;

VAR
  idmu := NEW(MUTEX);
  id : Word.T := 0;

PROCEDURE SetId(s : T) : T = 
  BEGIN LOCK idmu DO s.id := id; INC(id) END; RETURN s END SetId;

PROCEDURE Makemap(self : FreeVariable; tgt : FreeVariable) : BoolRemap.T =
  VAR 
    map := NEW(BoolRemap.T).init();
  BEGIN
    <* ASSERT NUMBER(self.baseBits^) = NUMBER(tgt.baseBits^) *>
    FOR i := FIRST(self.baseBits^) TO LAST(self.baseBits^) DO
      EVAL map.put(self.baseBits[i],tgt.baseBits[i])
    END;
    RETURN map
  END Makemap;

PROCEDURE Clone(self : FreeVariable; VAR clone : FreeVariable) : BoolRemap.T =
  VAR 
    map : BoolRemap.T;
  BEGIN
    clone := NEW(FreeVariable, 
                 bits := NEW(Array, NUMBER(self.bits^)),
                 baseBits := NEW(Array, NUMBER(self.baseBits^))).setId();
    
    FOR i := FIRST(self.baseBits^) TO LAST(self.baseBits^) DO
      clone.baseBits[i] := Bool.New()
    END;

    map := self.makemap(clone);
    
    WITH o = self.bits^, n = clone.bits^ DO
      FOR i := FIRST(o) TO LAST(o) DO
        n[i] := map.remap(o[i],TRUE)
      END
    END;

    RETURN map
  END Clone;

(* this is ugly... we have to do something special to maintain 
   res's "FreeVariable-ness"... *)
PROCEDURE Init(res : FreeVariable; max, min : INTEGER) : T =
  VAR
    range := max - min;
    initRange : FreeVariable;
  BEGIN
    <* ASSERT max >= min *>
    initRange := InitRange(res,range);
    res.bits := Add(Constant(min),initRange).bits;
    res.baseBits := initRange.baseBits;

    RETURN res
  END Init;

PROCEDURE InitRange(res: FreeVariable; range : CARDINAL) : FreeVariable =
  VAR
    bits : CARDINAL;
    inrange : Bool.T;
  BEGIN
    FOR i := 0 TO Word.Size - 1 DO
      IF Word.Shift(1,i) > range THEN bits := i; EXIT END
    END;
    <* ASSERT bits < Word.Size *>

    (* make room for the sign bit *)
    INC(bits);

    res := NEW(FreeVariable, bits := NEW(Array, bits), baseBits := NEW(Array, bits-1)).setId();
    
    (* a bit goofy...

       first we make a bits-bit number with new bools in it.

       then we build the inrange expression

       then we And the MSB with the inrange expression

       We end up with a number that must be in range!
    *)

    FOR i := FIRST(res.baseBits^) TO LAST(res.baseBits^) DO
      res.baseBits[i] := Bool.New()
    END;

    FOR i := 0 TO bits - 2 DO res.bits[i] := res.baseBits[i] END;

    res.bits[bits-1] := Bool.False();

    inrange := LessThanOrEqual(res,Constant(range));

    (* must check for zero-range datum *)
    IF bits > 1 THEN
      res.bits[bits-2] := Bool.And(res.bits[bits-2],inrange)
    END;
    
    <* ASSERT GreaterThan(res, Constant(range)) = Bool.False() AND
              LessThan(res, Zero) = Bool.False() *>
  
    RETURN res
  END InitRange;

PROCEDURE IsConstant(a : T) : BOOLEAN = 
  BEGIN
    FOR i := FIRST(a.bits^) TO LAST(a.bits^) DO
      IF a.bits[i] # Bool.False() AND a.bits[i] # Bool.True() THEN 
        RETURN FALSE 
      END
    END;
    RETURN TRUE
  END IsConstant;

PROCEDURE Extract(a : T; bit : CARDINAL) : Bool.T = 
  BEGIN
    IF bit < NUMBER(a.bits^) THEN 
      RETURN a.bits[bit]
    ELSE
      RETURN a.bits[LAST(a.bits^)]
    END
  END Extract;

PROCEDURE ExtractBaseBit(a : FreeVariable; bit : CARDINAL) : Bool.T = 
  BEGIN RETURN a.baseBits[bit] END ExtractBaseBit;

PROCEDURE SignExtend(self : T; nbits : CARDINAL) =
  VAR
    newBits := NEW(Array, nbits);
    top := self.bits[LAST(self.bits^)];
  BEGIN
    self.pack();
    <* ASSERT nbits >= NUMBER(self.bits^) *>
    SUBARRAY(newBits^,0,NUMBER(self.bits^)) := self.bits^;
    FOR i := LAST(self.bits^) + 1 TO LAST(newBits^) DO
      newBits[i] := top
    END;
    self.bits := newBits;
    <* ASSERT top = self.bits[LAST(self.bits^)] *>
  END SignExtend;

PROCEDURE Pack(self : T) =
  VAR
    top := self.bits[LAST(self.bits^)];
    msb : CARDINAL := LAST(self.bits^);
  BEGIN
    (* find largest necessary index *)
    FOR i := LAST(self.bits^) - 1 TO FIRST(self.bits^) BY -1 DO
      msb := i + 1;
      IF self.bits[i] # top THEN EXIT END
    END;
    IF msb # LAST(self.bits^) THEN
      VAR
        new := NEW(Array, msb + 1);
      BEGIN
        <* ASSERT NUMBER(new^) < NUMBER(self.bits^) *>
        new^ := SUBARRAY(self.bits^,FIRST(self.bits^), NUMBER(new^));
        <* ASSERT self.bits[LAST(self.bits^)] = new[LAST(new^)] *>
        self.bits := new
      END
    END;
    <* ASSERT top = self.bits[LAST(self.bits^)] *>
  END Pack;

PROCEDURE Add(a, b : T) : T =
  VAR
    bits := MAX(NUMBER(a.bits^),NUMBER(b.bits^)) + 2;
    res := NEW(T, bits := NEW(Array, bits)).setId();
    carry := Bool.False();
  BEGIN
    (* extend operands *)
    a.extend(bits); b.extend(bits);

    (* do the math *)
    FOR i := FIRST(res.bits^) TO LAST(res.bits^) DO
      res.bits[i] := Bool.Xor(Bool.Xor(carry,a.bits[i]),b.bits[i]);
      carry := Bool.Or(Bool.And(carry,a.bits[i]),Bool.Or(
                       Bool.And(carry,b.bits[i]),
                       Bool.And(a.bits[i],b.bits[i])))
    END;

    (* pack results *)
    a.pack(); b.pack(); res.pack();
    RETURN CheckCache(res)
  END Add;

PROCEDURE Neg(a : T) : T =
  VAR
    res := NEW(T, bits := NEW(Array, NUMBER(a.bits^))).setId();
  BEGIN
    FOR i := FIRST(a.bits^) TO LAST(a.bits^) DO
      res.bits[i] := Bool.Not(a.bits[i])
    END;
    RETURN Add(One,res)
  END Neg;
    
PROCEDURE Equals(a, b : T) : Bool.T = 
  VAR
    t := Bool.True();
    max := MAX(NUMBER(a.bits^),NUMBER(b.bits^));
  BEGIN
    a.extend(max); b.extend(max);
    FOR i := 0 TO max - 1 DO
      t := Bool.And(t, Bool.Equivalent(a.bits[i],b.bits[i]))
    END;
    a.pack(); b.pack(); (* why are these necessary? *)

    RETURN t
  END Equals;

PROCEDURE Vars(a : T) : BoolSet.T = 
  VAR
    s := NEW(BoolSetDef.T).init();
  BEGIN
    FOR i := FIRST(a.bits^) TO LAST(a.bits^) DO
      EVAL s.unionD(Bool.Vars(a.bits[i]))
    END;
    RETURN s
  END Vars;

PROCEDURE BitwiseOp(a, b : T; op : PROCEDURE(a, b : Bool.T) : Bool.T) : T =
  VAR
    bits := MAX(NUMBER(a.bits^),NUMBER(b.bits^));
    res := NEW(T, bits := NEW(Array, bits)).setId();
  BEGIN
    a.extend(bits); b.extend(bits);
    FOR i := 0 TO bits - 1 DO
      res.bits[i] := op(a.bits[i],b.bits[i])
    END;
    res.pack(); a.pack(); b.pack();
    RETURN CheckCache(res)
  END BitwiseOp;

PROCEDURE ShiftLeft(a : T; sa : CARDINAL) : T =
  VAR
    bits := NUMBER(a.bits^) + sa;
    res := NEW(T, bits := NEW(Array, bits)).setId();
  BEGIN
    FOR i := 0 TO sa - 1 DO
      res.bits[i] := Bool.False();
    END;
    FOR i := sa TO bits - 1 DO
      res.bits[i] := a.bits[i - sa]
    END;
    RETURN CheckCache(res)
  END ShiftLeft;

PROCEDURE UnsignedShiftRight(a : T; sa : CARDINAL) : T =
  VAR
    bits := NUMBER(a.bits^) - sa + 1;
    res := NEW(T, bits := NEW(Array, bits)).setId();
  BEGIN
    FOR i := 0 TO bits - 2 DO
      res.bits[i] := a.bits[i+sa]
    END;
    res.bits[bits - 1] := Bool.False();
    RETURN CheckCache(res)
  END UnsignedShiftRight;

PROCEDURE SignedShiftRight(a : T; sa : CARDINAL) : T =
  VAR
    bits := NUMBER(a.bits^) - sa;
    res := NEW(T, bits := NEW(Array, bits)).setId();
  BEGIN
    FOR i := 0 TO bits - 1 DO
      res.bits[i] := a.bits[i+sa]
    END;
    RETURN CheckCache(res)
  END SignedShiftRight;

VAR
  cmu := NEW(MUTEX);
  constCache := NEW(IntBoolIntegerTbl.Default).init();

PROCEDURE Constant(c : INTEGER) : T =
  VAR
    w : Word.T := c;
    res : T;
  BEGIN
    LOCK cmu DO
      IF constCache.get(c,res) THEN RETURN res END
    END;

    res := NEW(T, bits := NEW(Array,Word.Size)).setId();
    FOR i := 0 TO Word.Size - 1 DO 
      IF Word.Extract(w,i,1) = 1 THEN
        res.bits[i] := Bool.True()
      ELSE
        res.bits[i] := Bool.False()
      END
    END;
    res.pack();
    res := CheckCache(res);

    LOCK cmu DO
      EVAL constCache.put(c,res)
    END;

    RETURN res
  END Constant;

PROCEDURE Hash(a : T) : Word.T =
  BEGIN RETURN a.id  END Hash;

PROCEDURE Choose(c : Bool.T; it, if : T) : T =
  VAR
    l := MAX(NUMBER(if.bits^),NUMBER(it.bits^));
    res := NEW(T, bits := NEW(Array, l)).setId();
  BEGIN
    if.extend(l); it.extend(l);
    FOR i := 0 TO l - 1 DO
      res.bits[i] := Bool.Choose(c,it.bits[i],if.bits[i])
    END;
    res.pack(); if.pack(); it.pack();
    RETURN CheckCache(res)
  END Choose;

PROCEDURE CheckCache(a : T) : T =
  VAR
    try : T;
  BEGIN
    a.pack();
    LOCK mu DO
      IF cache.get(a,try) THEN 
        RETURN try
      ELSE
        <* ASSERT NUMBER(a.bits^) <= Word.Size *>
        EVAL cache.put(a,a);
        RETURN a
      END
    END
  END CheckCache;

PROCEDURE RepBits(a : T) : CARDINAL = 
  BEGIN RETURN NUMBER(a.bits^) END RepBits;

PROCEDURE Remap(a : T; m : BoolRemap.T; check : BOOLEAN) : T =
  VAR
    res := NEW(T, bits := NEW(Array, NUMBER(a.bits^))).setId();
  BEGIN
    WITH o = a.bits^, n = res.bits^ DO
      FOR i := FIRST(o) TO LAST(o) DO
        n[i] := m.remap(o[i],check)
      END
    END;
    RETURN CheckCache(res)
  END Remap;

PROCEDURE FreeRemap(a : FreeVariable; m : BoolRemap.T; check : BOOLEAN) : T =
  VAR
    res : FreeVariable := NEW(FreeVariable, bits := NEW(Array, NUMBER(a.bits^)), baseBits := NEW(Array, NUMBER(a.baseBits^))).setId();
  BEGIN
    WITH o = a.bits^, n = res.bits^ DO
      FOR i := FIRST(o) TO LAST(o) DO
        n[i] := m.remap(o[i],check)
      END
    END;
    WITH o = a.baseBits^, n = res.baseBits^ DO
      FOR i := FIRST(o) TO LAST(o) DO
        n[i] := m.remap(o[i],check)
      END
    END;
    RETURN CheckCache(res)
  END FreeRemap;

PROCEDURE Substitute(self : T; f : FreeVariable; val : T) : T =
  VAR
    map := NEW(BoolRemap.T).init();
  BEGIN
    <* ASSERT val.getMinValue() >= f.getMinValue() AND
              val.getMaxValue() <= f.getMaxValue() *>
    FOR i := FIRST(f.baseBits^) TO LAST(f.baseBits^) DO
      EVAL map.put(f.baseBits[i],val.extract(i))
    END;
    RETURN self.remap(map)
  END Substitute;

PROCEDURE SubstituteInBool(bool : Bool.T; f : FreeVariable; val : T) : Bool.T =
  VAR
    map := NEW(BoolRemap.T).init();
  BEGIN
    <* ASSERT val.getMinValue() >= f.getMinValue() AND
              val.getMaxValue() <= f.getMaxValue() *>
    FOR i := FIRST(f.baseBits^) TO LAST(f.baseBits^) DO
      EVAL map.put(f.baseBits[i],val.extract(i))
    END;
    RETURN map.remap(bool)
  END SubstituteInBool;

PROCEDURE AbstractEqual(<*UNUSED*>self : BoolIntegerTbl.T; 
                        READONLY a , b : T) : BOOLEAN =
  VAR
    max := MAX(NUMBER(a.bits^),NUMBER(b.bits^));
  BEGIN
    a.extend(max); b.extend(max);
    FOR i := 0 TO max - 1 DO
      IF a.bits[i] # b.bits[i] THEN RETURN FALSE END;
    END;
    a.pack(); b.pack(); (* why are these necessary? *)

    RETURN TRUE
  END  AbstractEqual;

(* The cache is a special BoolIntegerSetDef with the representation-checking
   Equals as its equal method.  It is used by the Cached() procedure to
   make sure that we don't return a different pointer for a value we already
   have represented *)
VAR
  mu := NEW(MUTEX);
  cache := NEW(BoolIntegerTbl.Default, keyEqual := AbstractEqual).init();
BEGIN 
  MinusOne := Constant(-1);
  One := Constant(1);
  Zero := Constant(0);
END BoolIntegerImpl.
