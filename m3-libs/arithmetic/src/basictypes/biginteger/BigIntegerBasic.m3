MODULE BigIntegerBasic;
(*Copyright (c) 1996, m3na project
  
Abstract: Integers of arbitrary size

Daniel Beer
*)

IMPORT Word AS W, xWordEx AS Wx;
FROM xUtils IMPORT Error, Err;

<*UNUSED*> CONST Module = "BigIntegerBasic.";
(*==========================*)

<*OBSOLETE*>
CONST
  ModMask = 16_FFFF;

REVEAL
  Value = BRANDED "BigIntegerValue" REF ARRAY OF W.T;

<*UNUSED*>
PROCEDURE FastCopy (READONLY x : T) : T =
VAR
  y := T{NEW(Value,NUMBER(x.data^)),x.size,x.sign};

BEGIN
  y.data^ := x.data^;
  RETURN y;
END FastCopy;

PROCEDURE Copy (READONLY x : T) : T =
VAR
  y := T{NEW(Value,x.size),x.size,x.sign};

BEGIN
  y.data^ := SUBARRAY(x.data^,0,LAST(y.data^));
  RETURN y;
END Copy;


<*INLINE*>
PROCEDURE MinMax (VAR min, max : INTEGER; a, b : INTEGER) =
BEGIN
  IF a<b THEN
    min := a;  max := b;
  ELSE
    min := b;  max := a;
  END;
END MinMax;

PROCEDURE Min (a, b : INTEGER) : INTEGER =
BEGIN
  IF a<b THEN RETURN a ELSE RETURN b END;
END Min;

PROCEDURE Max (a, b : INTEGER) : INTEGER =
BEGIN
  IF a>b THEN RETURN a ELSE RETURN b END;
END Max;

PROCEDURE Swap (VAR x, y : T) =
VAR
  s : T;
BEGIN
  s := x;
  x := y;
  y := s;
END Swap;



PROCEDURE CorrectSize (VAR x : T; start : INTEGER) =
VAR
  j := start;

BEGIN
  WHILE j>=0 AND x.data[j]=0 DO
    DEC (j);
  END;
  x.size := j+1;
END CorrectSize;

(*
PROCEDURE SetZero (VAR r : LIST OF T) =
VAR j : INTEGER;
BEGIN
  FOR j:=0 TO r'MAX DO
    r[j].size := 0;
    r[j].sign := FALSE;
  END;
END SetZero;

PROCEDURE SetOne (VAR r : LIST OF T) =
VAR j : INTEGER;
BEGIN
  FOR j:=0 TO r'MAX DO
    IF r[j].data=NIL THEN
      r[j].data'RANGE := 1;
      memPool.NewPooled (r[j].data);
    END;

    r[j].data[0] := 1;
    r[j].size    := 1;
    r[j].sign    := FALSE;
  END;
END SetOne;
*)

PROCEDURE FromInteger (x : INTEGER) : T =
VAR
  y := T{NEW(Value,1),1,x<0};
BEGIN
  IF y.sign THEN x := -x END;
  y.data[0] := x;
  RETURN y;
END FromInteger;



(*unsigned arithmetic, 'sign' entry is ignored;
  the signed arithmetic is build on the unsigned arithmetic routines*)

PROCEDURE AddU (READONLY x, y : T) : T =
VAR
  carry := FALSE;
  min, max : INTEGER;
  z : T;

BEGIN
  MinMax (min, max, x.size, y.size);

  IF max=0 THEN
    RETURN Zero;
  END;

  z.data := NEW(Value,max+1);

  FOR j:=0 TO min-1 DO
    z.data[j] := Wx.PlusWithCarry(x.data[j], y.data[j], carry);
  END;
  IF x.size>y.size THEN
    FOR j:=min TO max-1 DO
      z.data[j] := Wx.PlusWithCarry(x.data[j], 0, carry);
    END;
  ELSE
    FOR j:=min TO max-1 DO
      z.data[j] := Wx.PlusWithCarry(0, y.data[j], carry);
    END;
  END;
  z.data[max] := Wx.PlusWithCarry(0, 0, carry);

  CorrectSize (z, max);
  RETURN z;
END AddU;

(* You must make sure that x >= y *)
PROCEDURE SubU (READONLY x, y : T) : T =
VAR
  carry := FALSE;
  min, max : INTEGER;
  z : T;

BEGIN
  MinMax (min, max, x.size, y.size);

  IF max=0 THEN
    RETURN Zero;
  END;

  z.data := NEW(Value,max);

  FOR j:=0 TO min-1 DO
    z.data[j] := Wx.MinusWithBorrow(x.data[j], y.data[j], carry);
  END;
  IF x.size>y.size THEN
    FOR j:=min TO max-1 DO
      z.data[j] := Wx.MinusWithBorrow(x.data[j], 0, carry);
    END;
  ELSE
    FOR j:=min TO max-1 DO
      z.data[j] := Wx.MinusWithBorrow(0, y.data[j], carry);
    END;
  END;
  <*ASSERT NOT carry*>  (*otherwise it was x<y*)

  CorrectSize (z, max-1);
  RETURN z;
END SubU;

PROCEDURE CompareU (READONLY x, y : T) : [-1..1] =
BEGIN
  IF x.size < y.size THEN
    RETURN -1
  ELSIF x.size > y.size THEN
    RETURN 1
  ELSE
    FOR j:=x.size-1 TO 0 BY -1 DO
      IF x.data[j] < y.data[j] THEN
        RETURN -1
      ELSIF x.data[j] > y.data[j] THEN
        RETURN 1
      END;
    END;
    RETURN 0;
  END;
END CompareU;


(*signed arithmetic*)

PROCEDURE Add (READONLY x, y : T) : T =
VAR
  z : T;

BEGIN
  IF x.sign = y.sign THEN
    z := AddU (x, y);
    z.sign := x.sign;
  ELSE
    CASE CompareU (x, y) OF
    |  1 =>
      z := SubU (x, y);
      z.sign := x.sign;
    | -1 =>
      z := SubU (y, x);
      z.sign := y.sign;
    |  0 =>
      z := Zero;
    END;
  END;
  RETURN z;
END Add;

PROCEDURE Sub (READONLY x, y : T) : T =
VAR
  z : T;

BEGIN
  IF x.sign # y.sign THEN
    z := AddU (x, y);
    z.sign := x.sign;
  ELSE
    CASE CompareU (x, y) OF
    | 1 =>
      z := SubU (x, y);
      z.sign := x.sign;
    | -1 =>
      z := SubU (y, x);
      z.sign := NOT x.sign;
    | 0 =>
      z := Zero;
    END;
  END;
  RETURN z;
END Sub;

PROCEDURE Neg (READONLY x : T) : T =
VAR
  y := x;
BEGIN
  y.sign := NOT x.sign;
  RETURN y;
END Neg;

PROCEDURE Compare (READONLY x, y : T) : [-1..1] =
BEGIN
  IF x.sign # y.sign THEN
    IF x.sign THEN
      RETURN -1
    ELSE
      RETURN 1
    END
  ELSE
    IF x.sign THEN
      RETURN CompareU (y, x)
    ELSE
      RETURN CompareU (x, y)
    END
  END;
END Compare;

PROCEDURE Equal   (READONLY x, y : T) : BOOLEAN =
BEGIN
  IF x.sign # y.sign OR x.size # y.size THEN
    RETURN FALSE
  ELSE
    FOR j:=x.size-1 TO 0 BY -1 DO
      IF x.data[j] # y.data[j] THEN
        RETURN FALSE
      END;
    END;
    RETURN TRUE;
  END;
END Equal;




PROCEDURE Mul (READONLY x, y : T) : T =
VAR
  m,
  lo, hi, oldhi : W.T;
  carry : BOOLEAN;
  z : T;

BEGIN
  IF (x.size=0) OR (y.size=0) THEN
    RETURN Zero;
  END;

  z.data := NEW(Value,x.size+y.size);

  (*initialize result data*)
  m := x.data[0];
  hi := 0;
  FOR k:=0 TO y.size-1 DO
    oldhi := hi;
    Wx.DoubleLengthMultiply(m,y.data[k],lo,hi);
    carry:=FALSE;
    lo:=Wx.PlusWithCarry(lo,oldhi,carry);
    hi:=Wx.PlusWithCarry(hi,0,carry);
    z.data[k] := lo;
  END;
  z.data[y.size] := hi;

  FOR j:=1 TO x.size-1 DO
    m := x.data[j];
    hi := 0;
    FOR k:=0 TO y.size-1 DO
      oldhi := hi;
      Wx.DoubleLengthMultiply(m,y.data[k],lo,hi);
      carry:=FALSE;
      lo:=Wx.PlusWithCarry(lo,oldhi,carry);
      hi:=Wx.PlusWithCarry(hi,0,carry);
      carry:=FALSE;
      lo:=Wx.PlusWithCarry(lo,z.data[j+k],carry);
      hi:=Wx.PlusWithCarry(hi,0,carry);
      z.data[j+k] := lo;
    END;
    z.data[j+y.size] := hi;
  END;

  z.sign := x.sign # y.sign;
  CorrectSize (z, x.size+y.size-1);
  RETURN z;
END Mul;


(*
General problem with division:
We cannot easily find a digit of the quotient
if we only know the most significant digits of dividend and divisor.
Thus we only determine a close lower estimate,
which may cause several iterations for one digit of the quotient.
*)

(*
If we would use a Word by HalfWord division,
we had to distinguish between more cases.
*)

(*
PROCEDURE DoubleDivMod (lo, hi, div : W.T; VAR r : W.T) : W.T =
CONST
  HalfSize = W.Size DIV 2;
VAR
  sh       : INTEGER;
  divsmall,
  plo, phi,
  carry    : W.T;
  q        : W.T;
  carrybit : BOOLEAN;
BEGIN
  <*ASSERT W.LT(lo,div)*>
  sh  := W.Size-1 - Wx.FindMostSignifBit(div);
  div := W.Shift(div,sh);

  carry := 0;
  lo    := Wx.LeftShiftWithProbscosis(lo, sh, carry);
  hi    := Wx.LeftShiftWithProbscosis(hi, sh, carry);
  <*ASSERT carry=0*>

  divsmall := W.Shift(div,-HalfSize)+1;

  (*first lower estimation of the quotient*)
  q := W.Shift(W.Divide(hi,divsmall),HalfSize);
  Wx.DoubleLengthMultiply(q,div,plo,phi);

  carrybit := FALSE;
  lo := Wx.MinusWithBorrow(lo,plo,carrybit);
  hi := Wx.MinusWithBorrow(hi,phi,carrybit);
  <*ASSERT NOT carrybit*>

  (*second lower estimation of the quotient*)
  carry := 0;
  hi    := Wx.LeftShiftWithProbscosis(hi, HalfSize, carry);
  lo    := Wx.LeftShiftWithProbscosis(lo, HalfSize, carry);
  <*ASSERT carry=0*>

(*???
  q := W.Shift(W.Divide(hi,divsmall),HalfSize);
  Wx.DoubleLengthMultiply(q,div,plo,phi);

  carrybit := FALSE;
  lo := MinusWithBorrow(lo,plo,carrybit);
  hi := MinusWithBorrow(hi,phi,carrybit);
  <*ASSERT NOT carrybit*>
*)

  carry := 0;
  hi    := Wx.RightShiftWithProbscosis(hi, sh, carry);
  lo    := Wx.RightShiftWithProbscosis(lo, sh, carry);
  <*ASSERT carry=0*>
  r := lo;
  RETURN q;
END DoubleDivMod;
*)


(*x := x-SHL(y*z,sh)   (inplace, make sure that x.data has enough space) *)
PROCEDURE SubShiftedProd(VAR x : T; READONLY y : T; z : W.T; sh : INTEGER) =
VAR
  start,
  subsh  :  INTEGER;
  lo, hi,
  oldhi,
  probs,
  loshft :  W.T;
  carry  := FALSE;
  borrow := FALSE;
  j      :  INTEGER;
BEGIN
  start := sh DIV W.Size;
  subsh := sh MOD W.Size;

  hi := 0;
  probs := 0;
  FOR k:=0 TO y.size-1 DO
    oldhi := hi;
    Wx.DoubleLengthMultiply(y.data[k],z,lo,hi);
    carry := FALSE;
    lo := Wx.PlusWithCarry(lo,oldhi,carry);
    hi := Wx.PlusWithCarry(hi,0,carry);
    loshft := Wx.LeftShiftWithProbscosis(lo,subsh,probs);
    x.data[start+k] := Wx.MinusWithBorrow(x.data[start+k],loshft,borrow);
  END;
  j := start+y.size;
  loshft := Wx.LeftShiftWithProbscosis(oldhi,subsh,probs);
  x.data[j] := Wx.MinusWithBorrow(x.data[j],loshft,borrow);
  INC(j);
  x.data[j] := Wx.MinusWithBorrow(x.data[j],probs,borrow);

  WHILE borrow DO
    INC(j);
    x.data[j] := Wx.MinusWithBorrow(x.data[j], 0, borrow);
  END;
END SubShiftedProd;

(*x := x+SHL(y,sh)   (inplace, make sure that x.data has enough space)*)
PROCEDURE AddShifted(VAR x : T; y : W.T; sh : INTEGER) =
VAR
  start,
  subsh :  INTEGER;
  carry := FALSE;
BEGIN
  start := sh DIV W.Size;
  subsh := sh MOD W.Size;
  x.data[start] := Wx.PlusWithCarry(x.data[start], W.Shift(y,sh), carry);
  INC(start);
  x.data[start] := Wx.PlusWithCarry(x.data[start], W.Shift(y,sh-W.Size), carry);
  WHILE carry DO
    INC(start);
    x.data[start] := Wx.PlusWithCarry(x.data[start], 0, carry);
  END;
END AddShifted;



PROCEDURE DivModU (READONLY x, y : T; VAR r : T) : T RAISES {Error} =
VAR
  q : T;
BEGIN
  IF y.size = 0 THEN
    RAISE Error(Err.divide_by_zero);
  END;
  RETURN q;
END DivModU;

(*
PROCEDURE DivModU (READONLY x, y : T; VAR r : T) :  T;
VAR
  sz, sy, move, j : INTEGER;
  a, b, bSml, bBig, carry, d1, d2, t, quot : LONGCARD;
  xRun, yRun, qRun, rRun : CARDPTR;

BEGIN
  IF y.size = 0 THEN
    RAISE Error(divide_by_zero);
  END;
  z := Copy (x);

  sz := z.size-1;
  sy := y.size-1;
  move := sz - sy;

  IF move < 0 THEN
    SetZero (q);
    RETURN
  END;

  IF q.data=NIL THEN
    NUMBER(q.data) := move+1;
    memPool.NewPooled (q.data);
  ELSIF NUMBER(q.data) < move+1 THEN
    memPool.DisposePooled (q.data);
    NUMBER(q.data) := move+1;
    memPool.NewPooled (q.data);
  END;

  WITH qRun, j, move DO
    qRun := q.data[0]'PTR;
    FOR j:=0 TO move DO qRun+^ := 0 END;
    q.size := move+1;

    bSml := y.data[sy];
    IF sy>0 THEN
      bBig := bSml SHL 16 + y.data[sy-1] + 1;
    ELSE
      bBig := bSml SHL 16 + 1;
    END;
    INC (bSml);

    WHILE move>=0 DO
      IF sz>0 THEN
        a := z.data[sz] SHL 16 + z.data[sz-1];
      ELSE
        a := z.data[sz] SHL 16;
      END;

      IF sz = sy + move THEN
        b := bBig
      ELSE
        b := bSml
      END;

      IF a >= b THEN
        quot := a DIV b;
        (* Berechnung von neuem Rest *)
        carry := 0;
        FOR j:=move TO move+sy DO
          d1 := z.data[j];
          d2 := quot * y.data[j-move] + carry;

          IF d1 < d2 THEN
            t := d2 - d1 + 16_FFFF;
            z.data[j] := CARDINAL (16_FFFF - CAST (LONGCARD, (CAST (LONGSET, t) * ModMask)));
            carry := t SHR 16;
          ELSE
            z.data[j] := d1 - d2;
            carry := 0;
          END;
        END;
        IF carry > 0 THEN
          z.data[move+sy+1] := z.data[move+sy+1] - carry;   (* immer =Null ?? *)
        END;

        WHILE (sz>=0) AND (z.data[sz]=0) DO DEC (sz) END;

        (* Berechnung von neuem Quotienten *)
        carry := quot;
        WHILE carry#0 DO
          t := q.data[move] + carry;
          q.data[move] := CARDINAL (CAST (LONGCARD, (CAST (LONGSET, t) * ModMask)));
          carry := t SHR 16;
          INC (move);
        END;

        move := sz - sy;
      ELSE
        DEC (move);
      END;

  (*    WriteT (z); *)
    END;
  END;

  z.size := sz + 1;             (* für Compare *)

  IF CompareU (z, y) >= 0 THEN
(*    WriteString ("Result is not smaller than Modul!"+&10); *)

    (* r := r - y *)
    carry := 0;
    FOR j:=0 TO sy DO
      d1 := z.data[j];
      d2 := y.data[j] + carry;

      IF d1 < d2 THEN
        z.data[j] := 16_10000 + d1 - d2;
        carry := 1;
      ELSE
        z.data[j] := d1 - d2;
        carry := 0;
      END;
    END;
    IF carry = 1 THEN
      z.data[sy+1] := z.data[sy+1] - 1;
      WriteString ("Carry"+&10);
    END;
    WHILE (sz>=0) AND (z.data[sz]=0) DO DEC (sz) END;

    (* q := q + 1 *)
    j := 0;
    WHILE q.data[j] = 16_FFFF DO
      q.data[j] := 0;
      INC (j);
    END;
    INC (q.data[j]);
  END;

  z.size := sz + 1;
  CorrectSize (q, q.size-1);
END DivModU;
*)

PROCEDURE Div (READONLY x, y : T) : T RAISES {Error} =
VAR
  q, r : T;

BEGIN
  q := DivModU (x, y, r);
  q.sign := x.sign#y.sign;
  (*IF NOT Equal(r,Zero) THEN*)
  IF r.size # 0 THEN
    RAISE Error(Err.indivisible);
  END;
  RETURN q;
END Div;

PROCEDURE DivMod (READONLY x, y : T; VAR r : T) : T RAISES {Error} =
VAR
  q : T;

BEGIN
  q := DivModU (x, y, r);
  r.sign := y.sign;
  q.sign := x.sign#y.sign;
  IF q.sign THEN (*means x.sign#y.sign*)
    r := SubU (y, r);
  END;
  RETURN q;
END DivMod;

PROCEDURE Mod (READONLY x, y : T) : T RAISES {Error} =
VAR
  r : T;

BEGIN
  EVAL DivMod (x, y, r);
  RETURN r;
END Mod;

(*==========================*)
BEGIN
  Zero := FromInteger(0);
  One  := FromInteger(1);

(*  billion := FromInteger (1000000000);  *)
END BigIntegerBasic.
