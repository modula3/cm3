MODULE BigIntegerBasic;
(*Copyright (c) 1996, m3na project
  
Abstract: Integers of arbitrary size

Daniel Beer
*)

IMPORT Word AS W, xWordEx AS Wx;

<*UNUSED*> CONST Module = "BigIntegerBasic.";
(*==========================*)

CONST
  ModMask = 16_FFFF;

REVEAL
  Value = BRANDED "BigIntegerValue" REF ARRAY OF W.T;

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
  y.data^ := x.data^;
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


(*
PROCEDURE Mul (READONLY x, y : T) : T =
VAR
  j, k : INTEGER;
  m, t, carry : LONGCARD;
  xRun, yRun, rRun : CARDPTR;

BEGIN
  IF (x.size=0) OR (y.size=0) THEN
    SetZero (r);
    RETURN
  END;

  IF r.data=NIL THEN
    NUMBER(r.data) := x.size+y.size;
    memPool.NewPooled (r.data);
  ELSIF NUMBER(r.data) < x.size+y.size THEN
    memPool.DisposePooled (r.data);
    NUMBER(r.data) := x.size+y.size;
    memPool.NewPooled (r.data);
  END;

  xRun := x.data[0]'PTR;
  yRun := y.data[0]'PTR;
  rRun := r.data[0]'PTR;

  m := x.data[0];
  carry := 0;
  FOR k:=0 TO y.size-1 DO
    t := m * y.data[k] + carry;
    z.data[k] := CARDINAL (CAST (LONGCARD, (CAST (LONGSET, t) * ModMask)));
    carry   := t SHR 16;
  END;
  rRun^ := carry;

  FOR j:=1 TO x.size-1 DO
    yRun := y.data[0]'PTR;
    rRun := r.data[j]'PTR;

    m := x.data[j];
    carry := 0;
    FOR k:=0 TO y.size-1 DO
      t := m * y.data[k] + rRun^ + carry;
      z.data[k] := CARDINAL (CAST (LONGCARD, (CAST (LONGSET, t) * ModMask)));
      carry   := t SHR 16;
    END;
    rRun^ := carry;
  END;

  r.sign := x.sign # y.sign;
  CorrectSize (r, x.size+y.size-1);
END Mul;

PROCEDURE MulS (VAR r : T; READONLY y : T) =
BEGIN
  Assign (tmp, r);
  Mul (r, tmp, y);
END MulS;

PROCEDURE ModU (READONLY x, y : T) : T =
VAR
  sr, sy, move, j : INTEGER;
  a, b, bSml, bBig, carry, d1, d2, t, quot : LONGCARD;

BEGIN
  ASSERT2 (y.size # 0, DivZero);
  Assign (r, x);

  sr := r.size-1;
  sy := y.size-1;
  move := sr - sy;

  bSml := y.data[sy];
  IF sy>0 THEN
    bBig := bSml SHL 16 + y.data[sy-1] + 1;
  ELSE
    bBig := bSml SHL 16 + 1;
  END;
  INC (bSml);

  WHILE move>=0 DO
    IF sr>0 THEN
      a := r.data[sr] SHL 16 + r.data[sr-1];
    ELSE
      a := r.data[sr] SHL 16;
    END;

    IF sr = sy + move THEN
      b := bBig
    ELSE
      b := bSml
    END;

    IF a >= b THEN
      quot := a DIV b;
      (* Berechnung von neuem Rest *)
      carry := 0;
      FOR j:=move TO move+sy DO
        d1 := r.data[j];
        d2 := quot * y.data[j-move] + carry;

        IF d1 < d2 THEN
          t := d2 - d1 + 16_FFFF;
          r.data[j] := CARDINAL (16_FFFF - CAST (LONGCARD, (CAST (LONGSET, t) * ModMask)));
          carry := t SHR 16;
        ELSE
          r.data[j] := d1 - d2;
          carry := 0;
        END;
      END;
      IF carry > 0 THEN
        r.data[move+sy+1] := r.data[move+sy+1] - carry;   (* immer =Null ?? *)
      END;

      WHILE (sr>=0) AND (r.data[sr]=0) DO DEC (sr) END;

      move := sr - sy;
    ELSE
      DEC (move);
    END;

(*    WriteT (r); *)
  END;

  r.size := sr + 1;             (* für Compare *)

  IF CompareU (r, y) # -1 THEN
(*    WriteString ("Result is not -1 than Modul!"+&10); *)

    (* r := r - y *)
    carry := 0;
    FOR j:=0 TO sy DO
      d1 := r.data[j];
      d2 := y.data[j] + carry;

      IF d1 < d2 THEN
        r.data[j] := 16_10000 + d1 - d2;
        carry := 1;
      ELSE
        r.data[j] := d1 - d2;
        carry := 0;
      END;
    END;
    IF carry = 1 THEN
      r.data[sy+1] := r.data[sy+1] - 1;
      WriteString ("Carry"+&10);
    END;

    WHILE (sr>=0) AND (r.data[sr]=0) DO DEC (sr) END;
  END;

  r.size := sr + 1;
END ModU;

PROCEDURE ModUS (VAR r : T = READONLY x : T);
VAR
  sr, sx, move, j : INTEGER;
  a, b, bSml, bBig, carry, d1, d2, t, quot : LONGCARD;

BEGIN
  ASSERT2 (x.size # 0, DivZero);
  sr := r.size-1;
  sx := x.size-1;
  move := sr - sx;

  bSml := x.data[sx];
  IF sx>0 THEN
    bBig := bSml SHL 16 + x.data[sx-1] + 1;
  ELSE
    bBig := bSml SHL 16 + 1;
  END;
  INC (bSml);

  WHILE move>=0 DO
    IF sr>0 THEN
      a := r.data[sr] SHL 16 + r.data[sr-1];
    ELSE
      a := r.data[sr] SHL 16;
    END;

    IF sr = sx + move THEN
      b := bBig
    ELSE
      b := bSml
    END;

    IF a >= b THEN
      quot := a DIV b;
      (* Berechnung von neuem Rest *)
      carry := 0;
      FOR j:=move TO move+sx DO
        d1 := r.data[j];
        d2 := quot * x.data[j-move] + carry;

        IF d1 < d2 THEN
          t := d2 - d1 + 16_FFFF;
          r.data[j] := CARDINAL (16_FFFF - CAST (LONGCARD, (CAST (LONGSET, t) * ModMask)));
          carry := t SHR 16;
        ELSE
          r.data[j] := d1 - d2;
          carry := 0;
        END;
      END;
      IF carry > 0 THEN
        r.data[move+sx+1] := r.data[move+sx+1] - carry;   (* immer =Null ?? *)
      END;

      WHILE (sr>=0) AND (r.data[sr]=0) DO DEC (sr) END;

      move := sr - sx;
    ELSE
      DEC (move);
    END;

(*    WriteT (r); *)
  END;

  r.size := sr + 1;             (* für Compare *)

  IF CompareU (r, x) # -1 THEN
(*    WriteString ("Result is not -1 than Modul!"+&10); *)

    (* r := r - y *)
    carry := 0;
    FOR j:=0 TO sx DO
      d1 := r.data[j];
      d2 := x.data[j] + carry;

      IF d1 < d2 THEN
        r.data[j] := 16_10000 + d1 - d2;
        carry := 1;
      ELSE
        r.data[j] := d1 - d2;
        carry := 0;
      END;
    END;
    IF carry = 1 THEN
      r.data[sx+1] := r.data[sx+1] - 1;
      WriteString ("Carry"+&10);
    END;

    WHILE (sr>=0) AND (r.data[sr]=0) DO DEC (sr) END;
  END;

  r.size := sr + 1;
END ModUS;

PROCEDURE DivModU (VAR q, r : T = READONLY x, y : T);
VAR
  sr, sy, move, j : INTEGER;
  a, b, bSml, bBig, carry, d1, d2, t, quot : LONGCARD;
  xRun, yRun, qRun, rRun : CARDPTR;

BEGIN
  ASSERT2 (y.size # 0, DivZero);
  Assign (r, x);

  sr := r.size-1;
  sy := y.size-1;
  move := sr - sy;

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
      IF sr>0 THEN
        a := r.data[sr] SHL 16 + r.data[sr-1];
      ELSE
        a := r.data[sr] SHL 16;
      END;

      IF sr = sy + move THEN
        b := bBig
      ELSE
        b := bSml
      END;

      IF a >= b THEN
        quot := a DIV b;
        (* Berechnung von neuem Rest *)
        carry := 0;
        FOR j:=move TO move+sy DO
          d1 := r.data[j];
          d2 := quot * y.data[j-move] + carry;

          IF d1 < d2 THEN
            t := d2 - d1 + 16_FFFF;
            r.data[j] := CARDINAL (16_FFFF - CAST (LONGCARD, (CAST (LONGSET, t) * ModMask)));
            carry := t SHR 16;
          ELSE
            r.data[j] := d1 - d2;
            carry := 0;
          END;
        END;
        IF carry > 0 THEN
          r.data[move+sy+1] := r.data[move+sy+1] - carry;   (* immer =Null ?? *)
        END;

        WHILE (sr>=0) AND (r.data[sr]=0) DO DEC (sr) END;

        (* Berechnung von neuem Quotienten *)
        carry := quot;
        WHILE carry#0 DO
          t := q.data[move] + carry;
          q.data[move] := CARDINAL (CAST (LONGCARD, (CAST (LONGSET, t) * ModMask)));
          carry := t SHR 16;
          INC (move);
        END;

        move := sr - sy;
      ELSE
        DEC (move);
      END;

  (*    WriteT (r); *)
    END;
  END;

  r.size := sr + 1;             (* für Compare *)

  IF CompareU (r, y) # -1 THEN
(*    WriteString ("Result is not smaller than Modul!"+&10); *)

    (* r := r - y *)
    carry := 0;
    FOR j:=0 TO sy DO
      d1 := r.data[j];
      d2 := y.data[j] + carry;

      IF d1 < d2 THEN
        r.data[j] := 16_10000 + d1 - d2;
        carry := 1;
      ELSE
        r.data[j] := d1 - d2;
        carry := 0;
      END;
    END;
    IF carry = 1 THEN
      r.data[sy+1] := r.data[sy+1] - 1;
      WriteString ("Carry"+&10);
    END;
    WHILE (sr>=0) AND (r.data[sr]=0) DO DEC (sr) END;

    (* q := q + 1 *)
    j := 0;
    WHILE q.data[j] = 16_FFFF DO
      q.data[j] := 0;
      INC (j);
    END;
    INC (q.data[j]);
  END;

  r.size := sr + 1;
  CorrectSize (q, q.size-1);
END DivModU;

PROCEDURE Div (VAR q : T = READONLY x, y : T);
VAR
  j : INTEGER;

BEGIN
  ASSERT2 (y.size # 0, DivZero);
  DivModU (q, tmp, x, y);

  IF tmp.size # 0 THEN
    IF x.sign THEN       (* INC (q) *)
      j := 0;
      WHILE q.data[j] = 16_FFFF DO
        q.data[j] := 0;
        INC (j);
      END;
      INC (q.data[j]);
    END;
  END;

  q.sign := x.sign # y.sign;
END Div;

PROCEDURE DivS (VAR q : T = READONLY y : T);
BEGIN
  ASSERT2 (y.size # 0, DivZero);
  Assign (tmp, q);
  Div (q, tmp, y);
END DivS;

PROCEDURE Mod (READONLY x, y : T) : T =
VAR
  j : INTEGER;
  t, carry : W.T;

BEGIN
  ASSERT2 (y.size # 0, DivZero);
  ModU (r, x, y);

  IF r.size # 0 THEN
    IF x.sign THEN       (* r := y - r; *)
      carry := 0;
      FOR j:=0 TO r.size-1 DO
        t := y.data[j] - r.data[j] - carry;

        IF t < 0 THEN
          r.data[j] := 16_10000 + t;
          carry := 1;
        ELSE
          r.data[j] := t;
          carry := 0;
        END;
      END;
      IF carry = 1 THEN HALT (66) END;
    END;
  END;

  r.sign := FALSE;
END Mod;

PROCEDURE DivMod (VAR q, r : T = READONLY x, y : T);
VAR
  j : INTEGER;
  t, carry : W.T;

BEGIN
  ASSERT2 (y.size # 0, DivZero);
  DivModU (q, r, x, y);

  IF r.size # 0 THEN
    IF x.sign THEN
      (* INC (q) *)
      j := 0;
      WHILE q.data[j] = 16_FFFF DO
        q.data[j] := 0;
        INC (j);
      END;
      INC (q.data[j]);

      (* r := y - r; *)
      carry := 0;
      FOR j:=0 TO r.size-1 DO
        t := y.data[j] - r.data[j] - carry;

        IF t < 0 THEN
          r.data[j] := 16_10000 + t;
          carry := 1;
        ELSE
          r.data[j] := t;
          carry := 0;
        END;
      END;
      IF carry = 1 THEN HALT (66) END;
    END;
  END;

  q.sign := x.sign # y.sign;
  r.sign := FALSE;
END DivMod;
*)

(*==========================*)
BEGIN
  Zero := FromInteger(0);
  One  := FromInteger(1);

(*  billion := FromInteger (1000000000);  *)
END BigIntegerBasic.
