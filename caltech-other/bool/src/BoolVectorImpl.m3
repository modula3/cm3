MODULE BoolVectorImpl EXPORTS BoolVector;
IMPORT Bool;

REVEAL
  T = Public BRANDED "BoolVector" OBJECT
    data : REF ARRAY OF Bool.T := NIL;
  METHODS
    checkIndex(idx : CARDINAL) RAISES { IndexOutOfBounds } := CheckIndex;
  OVERRIDES
    getBit := GetBit;
    setBit := SetBit;
    getWidth := GetWidth;
    toWidth := ToWidth;
  END;

  FixedWidth = FixedWidthPublic BRANDED "Fixed Width BoolVector" OBJECT
    width := -1;
  OVERRIDES
    setMaxWidth := SetMaxWidth;
    getMaxWidth := GetMaxWidth;
  END;

PROCEDURE SetMaxWidth(self : FixedWidth; width : CARDINAL) : FixedWidth =
  BEGIN self.width := width; RETURN self END SetMaxWidth;

PROCEDURE GetWidth(self : T) : CARDINAL =
  BEGIN 
    IF self.data = NIL THEN self.data := False(0).data END;
    RETURN NUMBER(self.data^) 
  END GetWidth;

PROCEDURE GetMaxWidth(self : FixedWidth) : CARDINAL =
  BEGIN RETURN self.width END GetMaxWidth;

PROCEDURE CheckIndex( self : T; idx : CARDINAL) RAISES { IndexOutOfBounds } =
  BEGIN
    TYPECASE self OF
    | FixedWidth (x) => 
      IF idx >= x.width THEN RAISE IndexOutOfBounds END
    | T => (* skip *)
    END;
  END CheckIndex;

PROCEDURE GetBit(x : T; idx : CARDINAL) : Bool.T  
  RAISES { IndexOutOfBounds } =
  BEGIN 
    x.checkIndex(idx);

    IF x.data = NIL THEN x.data := False(0).data END;
    IF idx>LAST(x.data^) THEN RETURN Bool.False() ELSE RETURN x.data[idx] END 
  END GetBit;

PROCEDURE SetBit(x : T; idx : CARDINAL; bool : Bool.T) 
  RAISES { IndexOutOfBounds } = 
  BEGIN
    x.checkIndex(idx);

    IF x.data = NIL THEN x.data := False(0).data END;
    (* extend if necessary *)
    IF idx > LAST(x.data^) THEN VAR newArr := False(idx + 1).data; BEGIN
        SUBARRAY(newArr^,0,NUMBER(x.data^)) := x.data^;
        x.data := newArr
    END END;
    x.data[idx] := bool
  END SetBit;

PROCEDURE ToWidth(self : T; width : CARDINAL) : FixedWidth =
  <* FATAL IndexOutOfBounds *>
  VAR res := NEW(FixedWidth).setMaxWidth(width); BEGIN
    FOR i := 0 TO MIN(width,self.getWidth()) - 1 DO
      res.setBit(i,self.getBit(i))
    END;
    RETURN res
  END ToWidth;

PROCEDURE False(width : CARDINAL) : T =
  VAR res := NEW(T); BEGIN 
    res.data := NEW(REF ARRAY OF Bool.T,width);
    FOR i := FIRST(res.data^) TO LAST(res.data^) DO
      res.data[i] := Bool.False()
    END;
    RETURN res
  END False;

(* end of basic functions *)

BEGIN END BoolVectorImpl.
