MODULE BigIntegerRep;
(*Copyright (c) 1996, m3na project

Abstract: Integers of arbitrary size

Daniel Beer
*)

IMPORT Word AS W, xWordEx AS Wx;
FROM xUtils IMPORT Error, Err;
FROM BigIntegerBasic IMPORT Value, Zero;

<*UNUSED*> CONST Module = "BigIntegerRep.";
(*==========================*)


PROCEDURE IsZero (READONLY x : T) : BOOLEAN =
BEGIN
  RETURN x.size=0 OR x.size=1 AND x.data[0]=0;
END IsZero;

<*INLINE*>
PROCEDURE MinMax (VAR min, max : INTEGER; a, b : INTEGER) =
BEGIN
  IF a<b THEN
    min := a;  max := b;
  ELSE
    min := b;  max := a;
  END;
END MinMax;



PROCEDURE CorrectSize (VAR x : T; start : INTEGER) =
VAR
  j := start;

BEGIN
  WHILE j>=0 AND x.data[j]=0 DO
    DEC (j);
  END;
  x.size := j+1;
END CorrectSize;


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


PROCEDURE MulU (READONLY x, y : T) : T =
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

  CorrectSize (z, x.size+y.size-1);
  RETURN z;
END MulU;


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


PROCEDURE SubBitPos (READONLY x,y : BitPos) : BitPos =
BEGIN
  IF x.bit >= y.bit THEN
    RETURN BitPos{x.word-y.word,x.bit-y.bit};
  ELSE
    RETURN BitPos{x.word-y.word-1,W.Size-y.bit+x.bit};
  END;
END SubBitPos;

<*INLINE*>
PROCEDURE CompareBitPos (READONLY x,y : BitPos) : [-1..1] =
BEGIN
  IF    x.word < y.word THEN
    RETURN -1;
  ELSIF x.word > y.word THEN
    RETURN 1;
  ELSIF x.bit < y.bit THEN
    RETURN -1;
  ELSIF x.bit > y.bit THEN
    RETURN 1;
  ELSE
    RETURN 0;
  END;
END CompareBitPos;

PROCEDURE BitPosEndToBegin (READONLY x : BitPos) : BitPos =
BEGIN
  IF x.bit < W.Size-1 THEN
    RETURN BitPos{x.word-1,x.bit+1};
  ELSE
    RETURN BitPos{x.word,0};
  END;
END BitPosEndToBegin;

PROCEDURE GetMSBPos (READONLY x : T) : BitPos =
BEGIN
  RETURN BitPos{x.size-1, Wx.FindMostSignifBit(x.data[x.size-1])};
END GetMSBPos;

(*grab bits from sh to sh+W.Size-1*)
PROCEDURE GetSubword (READONLY x : T; sh : BitPos) : W.T =
VAR
  lo : W.T;
BEGIN
  IF sh.word<0 THEN
    lo := 0;
  ELSE
    lo := W.RightShift(x.data[sh.word],sh.bit);
  END;
  RETURN W.Or (lo, W.LeftShift(x.data[sh.word+1],W.Size-sh.bit));
END GetSubword;


(*x := x-SHL(y*z,sh)   (inplace, make sure that x.data has enough space) *)
PROCEDURE SubShiftedProd(VAR x : T; READONLY y : T; z : W.T; sh : BitPos) =
VAR
  lo, hi,
  oldhi,
  probs,
  loshft :  W.T;
  carry  := FALSE;
  borrow := FALSE;
  j      :  INTEGER;
BEGIN
  hi := 0;
  probs := 0;
  FOR k:=0 TO y.size-1 DO
    oldhi := hi;
    Wx.DoubleLengthMultiply(y.data[k],z,lo,hi);
    carry := FALSE;
    lo := Wx.PlusWithCarry(lo,oldhi,carry);
    hi := Wx.PlusWithCarry(hi,0,carry);
    loshft := Wx.LeftShiftWithProbscosis(lo,sh.bit,probs);
    x.data[sh.word+k] := Wx.MinusWithBorrow(x.data[sh.word+k],loshft,borrow);
  END;
  j := sh.word+y.size;
  loshft := Wx.LeftShiftWithProbscosis(oldhi,sh.bit,probs);
  x.data[j] := Wx.MinusWithBorrow(x.data[j],loshft,borrow);
  INC(j);
  x.data[j] := Wx.MinusWithBorrow(x.data[j],probs,borrow);

  WHILE borrow DO
    INC(j);
    x.data[j] := Wx.MinusWithBorrow(x.data[j], 0, borrow);
  END;
  CorrectSize(x,x.size);
END SubShiftedProd;

(*x := x+SHL(y,sh)   (inplace, make sure that x.data has enough space)*)
PROCEDURE AddShifted(VAR x : T; y : W.T; sh : BitPos) =
VAR
  carry := FALSE;
BEGIN
  x.data[sh.word] := Wx.PlusWithCarry(x.data[sh.word], W.LeftShift(y,sh.bit), carry);
  INC(sh.word);
  x.data[sh.word] := Wx.PlusWithCarry(x.data[sh.word], W.RightShift(y,W.Size-sh.bit), carry);
  WHILE carry DO
    INC(sh.word);
    x.data[sh.word] := Wx.PlusWithCarry(x.data[sh.word], 0, carry);
  END;
END AddShifted;

PROCEDURE DivModU (READONLY x, y : T; VAR r : T) : T RAISES {Error} =
VAR
  q : T;
  qmswstartpos : BitPos;
  qmsbpos, rmsbpos, ymsbpos : BitPos;
  qmsw,    rmsw,    ymsw    : W.T;
BEGIN
  IF y.size = 0 THEN
    RAISE Error(Err.divide_by_zero);
  END;

  r.data := NEW(Value,x.size+1);
  q.data := NEW(Value,x.size-y.size+1);
  SUBARRAY(r.data^,0,x.size-1) := SUBARRAY(x.data^,0,x.size-1);

  (*normalize remainder and divisor temporarily
    divide most significant 32 bit of r by the most significant 16 bit of y*)
  ymsbpos := GetMSBPos(y);
  ymsw    := GetSubword(y,BitPosEndToBegin(ymsbpos));
  ymsw    := W.RightShift (ymsw, W.Size DIV 2);  (*the division algorithm is fastest if the divisor is clipped to the half number of bits compared with the approximation of the dividend*)
  INC(ymsw);  (*round up to get a lower estimate for quotient*)

  rmsbpos := GetMSBPos(r);
  WHILE CompareBitPos (rmsbpos, ymsbpos) > 0 DO
    rmsw := GetSubword(r,BitPosEndToBegin(rmsbpos));
    (*round down by neglecting the following bits to get a lower estimate for quotient*)
    qmsw    := rmsw DIV ymsw;
    qmsbpos := SubBitPos(rmsbpos,ymsbpos);
    qmswstartpos := SubBitPos(qmsbpos,BitPos{0,W.Size DIV 2-1});
    AddShifted     (q,    qmsw, qmswstartpos);
    SubShiftedProd (r, y, qmsw, qmswstartpos);
    rmsbpos := GetMSBPos(r);
  END;
  (*CorrectSize (q, LAST(q.data));*)

  (*this loop will run at most three times*)
  WHILE CompareU (r, y) > 0 DO
    (*
    r := SubU (r, y);
    q := AddU (q, One);
    *)
    AddShifted     (q,    1, BitPos{0,0});
    SubShiftedProd (r, y, 1, BitPos{0,0});
  END;

  CorrectSize (q, LAST(q.data^));
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

(*==========================*)
BEGIN
END BigIntegerRep.
