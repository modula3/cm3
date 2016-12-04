(* Copyright (C) 2016 Peter McKinna. All rights reserved. *)
(* See file COPYRIGHT-BSD for details. *)

UNSAFE MODULE FloatUtils;

IMPORT Word, LongRealRep;
IMPORT DragonInt AS DI;

CONST 
  P = 53;
  Ebits = 11;
  ULbits = 32;
  Exp = 16_3FF00000;
  ExpMask = 16_100000;  
  AllOn = 16_FFFFFFFF; (* mask to grab 32 significant bits *)

TYPE
  LongWord = RECORD
    w1,w0 : Int32;
  END;
  
(* return the sign-extended bottom 32 bits of 'x' *)
PROCEDURE Fix32 (x: Word.T): Int32 =
  CONST
    Sign = 16_80000000;
    SignExtend = Word.LeftShift (Word.Not (0), 31);
  BEGIN
    IF Word.And (x, Sign) = 0
      THEN RETURN Word.And (x, AllOn);
      ELSE RETURN Word.Or (SignExtend, Word.And (x, AllOn));
    END;
  END Fix32;
  
(* return the lowest 32 bit word of a longreal *)
PROCEDURE Word0(r : LONGREAL) : Int32 =
  BEGIN
    RETURN LOOPHOLE(r,LongWord).w0;
  END Word0;
  
(* set the lowest 32 bit word of a longreal *)
PROCEDURE Word0Set(r : LONGREAL; r0 : Int32) : LONGREAL =
  BEGIN
    LOOPHOLE(r,LongWord).w0 := r0; 
    RETURN r;
  END Word0Set;

(* return the highest 32 bit word of a longreal *)
PROCEDURE Word1(r : LONGREAL) : Int32 =
  BEGIN
    RETURN LOOPHOLE(r,LongWord).w1;
  END Word1;
  
(* checks if all bits are set in a bigint *)
PROCEDURE allOn(s : DI.Session; b : DI.T; n : INTEGER) : BOOLEAN =
  VAR
    d,m,len : INTEGER;
    arr : DI.RefInt32;
  BEGIN
    IF n = 0 THEN RETURN FALSE; END;
    DI.ToArr32(s, b, arr, len);
    IF n > len * 32 THEN n := len * 32; END;  
    
    d := n DIV 32;
    m := n MOD 32;
    
    FOR i := 0 TO d - 1 DO
      IF Word.And(arr[i],AllOn) # AllOn THEN RETURN FALSE; END;
    END;
    IF Word.Extract(arr[d],0,m) # Word.Extract(AllOn,0,m) THEN RETURN FALSE; END;
    RETURN TRUE;
  END allOn;
  
(* checks if any bits are set in a bigint *)
PROCEDURE anyOn(s : DI.Session; b : DI.T; n : INTEGER) : BOOLEAN =
  VAR
    d,m,len : INTEGER;
    arr : DI.RefInt32;
  BEGIN
    IF n = 0 THEN RETURN FALSE; END;
    DI.ToArr32(s, b, arr, len);
    IF n > len * 32 THEN n := len * 32; END;  
    
    d := n DIV 32;
    m := n MOD 32;
    
    FOR i := 0 TO d - 1 DO
      IF arr[i] > 0 THEN RETURN TRUE; END;
    END;
    IF Word.Extract(arr[d],0,m) > 0 THEN RETURN TRUE; END;
    RETURN FALSE;
  END anyOn;
  
(* set all bits of a bigint to 1 *)
PROCEDURE setOnes(s : DI.Session; b : DI.T; n : INTEGER) : DI.T =
  VAR 
    ret : DI.T;
    d,m,len : INTEGER;
    arr : DI.RefInt32;
    ext : DI.RefInt;
  BEGIN
    DI.ToArr32(s, b, arr, len);
    IF n > len * 32 THEN n := len * 32; END;

    d := n DIV 32;
    m := n MOD 32;
    FOR i := 0 TO d - 1 DO
      arr[i] := Fix32(Word.Insert(arr[d],AllOn,0,32));
    END;
    IF d < len THEN
     arr[d] := Fix32(Word.Insert(arr[d],AllOn,0,m));
    END;

    ext := NEW(DI.RefInt,len);
    FOR i := 0 TO len - 1 DO
      ext[i] := arr[i];
    END;
    ret := DI.NewFromArr(s,ext);
    RETURN ret;
  END setOnes;

(* removes the lowest zero bits ie shifts all the zeros right
and returns the number shifted and the result of the shift *)
PROCEDURE lo0bits(VAR y : Word.T) : INTEGER =
  VAR x : Word.T := y; k : INTEGER;
  BEGIN
    IF Word.And(x,7) > 0 THEN
      IF Word.And(x,1) > 0 THEN
        RETURN 0;
      END;
      IF Word.And(x,2) > 0 THEN
        y := Word.RightShift(x,1);
        RETURN 1;
      END;
      y := Word.RightShift(x,2);
      RETURN 2;
    END;
    k := 0;
    IF Word.And(x,16_FFFF) = 0 THEN
      k := 16;
      x := Word.RightShift(x,16);
    END;
    IF Word.And(x,16_FF) = 0 THEN
      INC(k,8);
      x := Word.RightShift(x,8);
    END;
    IF Word.And(x,16_F) = 0 THEN
      INC(k,4);
      x := Word.RightShift(x,4);
    END;
    IF Word.And(x,16_3) = 0 THEN
      INC(k,2);
      x := Word.RightShift(x,2);
    END;
    IF Word.And(x,1) = 0 THEN
      INC(k);
      x := Word.RightShift(x,1);
      IF x = 0 THEN RETURN 32; END;
    END;
    y := x;
    RETURN k;
  END lo0bits;

(* removes the highest zero bits ie shifts all the zeros left
and returns the number shifted and the result of the shift *)
PROCEDURE hi0bits(x : Word.T) : INTEGER =
  VAR k : INTEGER;
  BEGIN
    k := 0;
    IF Word.And(x,16_FFFF0000) = 0 THEN
      k := 16;
      x := Word.LeftShift(x,16);
    END;
    IF Word.And(x,16_FF000000) = 0 THEN
      INC(k,8);
      x := Word.LeftShift(x,8);
    END;
    IF Word.And(x,16_F0000000) = 0 THEN
      INC(k,4);
      x := Word.LeftShift(x,4);
    END;    
    IF Word.And(x,16_C0000000) = 0 THEN
      INC(k,2);
      x := Word.LeftShift(x,2);
    END;    
    IF Word.And(x,16_80000000) = 0 THEN
      INC(k);
      IF Word.And(x,16_40000000) = 0 THEN
        RETURN 32;
      END;
    END;    
    RETURN k;
  END hi0bits;

(* return mantissa bits *)
PROCEDURE mantbits(d : LONGREAL) : INTEGER =
  VAR L : Word.T;
  BEGIN
    L := Word1(d);
    IF L # 0 THEN
      RETURN P - lo0bits(L);
    END;
    L := Word.Or(Word0(d), ExpMask);
    RETURN P - 32 - lo0bits(L);
  END mantbits;

(* bigint diff ensuring a > b swapping if needed *)
PROCEDURE diff(s : DI.Session; a,b : DI.T; VAR dsign : [-1..1]) : DI.T =
  VAR tmp : DI.T;
  BEGIN
    dsign := DI.compare(s,a,b);
    IF dsign < 0 THEN
      tmp := a; a := b; b := tmp;
      dsign := -dsign;
    ELSE
      dsign := 0;
    END;
    RETURN DI.diff(s, a, b);
  END diff;
  
(* multiply a by the kth power of 5 *)
PROCEDURE pow5mult(s : DI.Session; a : DI.T; k : INTEGER) : DI.T =
  VAR
    five,b : DI.T;
  BEGIN
    five := DI.New(s,0,5);
    b := DI.copy(s,a);
    FOR i := 1 TO k DO
      b := DI.mult(s,b,five);
    END;
    RETURN b;
  END pow5mult;

(* returns a longreal as a bigint *)
PROCEDURE d2b(s : DI.Session; l : LONGREAL; VAR exp,bits : INTEGER) : DI.T =
  VAR
    b : DI.T;
    rep : LongRealRep.T;
    f0,f1,de,i,k : INTEGER;
    x,y,z : Word.T;
  BEGIN
    rep := LOOPHOLE(l,LongRealRep.T);
    
    f1 := Word.And (rep.significand1, AllOn); (* ensure 32 bits *)
    f0 := Word.And (rep.significand0, 16_fffff); (* frac_mask *)

    z := f0;
    de := rep.exponent;
    IF de # 0 THEN
      z := Word.Or(z,ExpMask); (* same as adding implied 53 bit *)
    END;

    y := f1;
    IF y # 0 THEN
      k := lo0bits(y);
      IF k # 0 THEN
        y := Word.Or(y, Word.LeftShift(z,32 - k));
        z := Word.RightShift(z, k);
      END;
      IF z # 0 THEN 
        i := 2; x := y; 
      ELSE 
        i := 1; x := z; 
      END;
      b := DI.New (s, z, y); 
    ELSE
      k := lo0bits(z);
      b := DI.New (s, 0, z);
      i := 1; 
      x := z;
      INC(k,32);
    END;

    IF de > 0 THEN
      exp := de - Bias - (P - 1) + k;
      bits := P - k;
    ELSE
      exp := de - Bias - (P - 1) + 1 + k;
      bits := i * 32 - hi0bits(x); 
    END;
    RETURN b;
  END d2b;
  
(* returns a string as a bigint *)
PROCEDURE s2b(s : DI.Session; READONLY a : ARRAY OF CHAR; nd0,nd : INTEGER; y9 : INTEGER) : DI.T =
  VAR
    b : DI.T;
    i,j,num : INTEGER; 
  BEGIN
    j := 0; i := 9;
    b := DI.New(s,0,y9);

    IF nd0 > 9 THEN
      REPEAT
        num := ORD(a[i]) - ORD('0');
        b := DI.timesTenInPlace(s,b);
        b := DI.add(s,b,DI.New(s,0,num));
        INC(i);
      UNTIL nd0 <= i;
      j := i + 1;
    ELSE
      j := 10;
    END;

    WHILE i < nd DO
      num := ORD(a[j]) - ORD('0');
      b := DI.timesTenInPlace(s,b);
      b := DI.add(s,b,DI.New(s,0,num));
      INC(i); INC(j);
    END;
    RETURN b;
  END s2b;

(* returns ratio of a to b *)
PROCEDURE ratio(s : DI.Session; a,b : DI.T) : LONGREAL =
  VAR 
    ret,da,db : LONGREAL;
    ka,kb,k,lena,lenb : INTEGER;
    w0 : Int32;
  BEGIN
    da := b2d(s,a,ka,lena);
    db := b2d(s,b,kb,lenb);

    k := ka - kb + ULbits * (lena - lenb);

    IF k > 0 THEN
      w0 := Word0(da);
      INC(w0,k * ExpMask);
      da := Word0Set(da,w0);
    ELSE
      k := -k;
      w0 := Word0(db);
      INC(w0,k * ExpMask);
      db := Word0Set(db,w0);
    END;   
    ret := da / db;
    RETURN ret;
  END ratio;

(* returns number of trailing zeros of a bigint *)
PROCEDURE trailz(s : DI.Session; b : DI.T) : INTEGER =
  VAR 
    len,i,j,n : INTEGER := 0;
    L : Word.T;
    arr : DI.RefInt32;
  BEGIN
    DI.ToArr32(s,b,arr,len);
    
    WHILE i < len DO
      IF arr[i] = 0 THEN
        INC(n,ULbits);
      ELSE
        EXIT;
      END;
      INC(i);
    END;
    IF i < len THEN
      L := arr[i];
      j := lo0bits(L);
      INC(n,j);
    END;
    RETURN n;
  END trailz;

  
(*  Internal functions *)

(* returns a bigint as a LONGREAL *)
PROCEDURE b2d(s : DI.Session; READONLY a : DI.T; VAR e : INTEGER; VAR len : INTEGER) : LONGREAL =
  VAR
    arr : DI.RefInt32;
    k,ind,w,y,z,d0,d1 : INTEGER;
    d : LONGREAL;
    lw : LongWord;

  PROCEDURE Next() : INTEGER =
    VAR x : INTEGER;
    BEGIN
      IF ind >= 0 THEN
        x := arr[ind];
      ELSE
        x := 0;
      END;  
      DEC(ind);
      RETURN Word.And(x,AllOn);
    END Next;
  
  BEGIN
    DI.ToArr32(s,a,arr,len);

    ind := len - 1;
    y := Next();
    k := hi0bits(y);
    e := 32 - k;
  
    IF k < Ebits THEN
      d0 := Word.Or(Exp, Word.RightShift(y, Ebits - k));
      w := Next();
      d1 := Word.LeftShift(y, 32 - Ebits + k);
      d1 := Word.Or(d1, Word.RightShift(w, Ebits - k));
    ELSE
      z := Next();
      DEC(k,Ebits);
      IF k > 0 THEN
        d0 := Word.Or(Exp, Word.Or(Word.LeftShift(y,k), Word.RightShift(z, 32 - k)));
        y := Next();  
        d1 := Word.LeftShift(z, k);
        d1 := Word.Or(d1, Word.RightShift(y, 32 - k));        
      ELSE (* k <= 0 *)
        d0 := Word.Or(Exp, y);
        d1 := z;
      END;
    END;
    lw.w0 := Fix32(d0);
    lw.w1 := Fix32(d1);
    d := LOOPHOLE(lw,LONGREAL); <*NOWARN*>
    RETURN d;
  END b2d;
  
BEGIN
END FloatUtils.
