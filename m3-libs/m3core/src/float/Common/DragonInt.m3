(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Jul  7 13:26:16 PDT 1995 by heydon                   *)
(*      modified on Fri Jun 17 16:30:50 PDT 1994 by kalsow                   *)
(*      modified on Thu May  6 16:25:50 PDT 1993 by muller                   *)

UNSAFE MODULE DragonInt;

IMPORT Word,Long;

(* Internal computations of Dragon require the use of bignums.
   Also, the fraction of a floating-point number can occupy more bits
   than there are in a Word.T.  We use an ad-hoc implementation of
   bignums, because only few of the arithmetic operations are needed,
   and only positive numbers are needed.

   A DragonInt.T, b, represent the value:

      sigma (0, b.s - 1,  b.w[i] * 2 ^ Used * i)

   that is, b.w[0] contains the Used least significant bits, 
   b.w[1] the next Used significant bits and so on.

   The most significant word of a DragonInt.T must be non-null.

   The value Used is chosen to facilitate the implementation of
   the arithmetic operations needed by the Dragon.F routine:
   intermediate computations on individual Word.T of a DragonInt.T won't 
   overflow. *)

(*-------------------------------------------------------------- sessions ---*)

VAR
  mu := NEW(MUTEX);
  availList: Session := NIL;
  availListSz: CARDINAL := 0;

(* Since "Session"'s are large, they are returned to "availList" at the end
   of each session. The variable "availListSz" counts the size of the
   "availList". Both "availList" and "availListSz" are protected by the mutex
   "mu". *)

CONST
  MaxAvailListSz = 10;
  InitBuffSz = 340;  

(* At most "MaxAvailListSz" sessions are stored on "availList" at once. This
   prevents a program from getting too bloated. Past that size, "Session"
   objects are simply dropped on the floor.

   "InitBuffSz" is the initial size (in "Word.T"'s) of the "Words" buffer in
   each "Session" object. Buffers are automatically expanded as necessary.
   This value was chosen by running a test program that converted various real
   numbers to decimal representation and examining the distribution of
   required buffer sizes. *)
    
TYPE
  Words = REF ARRAY OF Word.T;

REVEAL
  Session = BRANDED "DragonInt.Session" REF RECORD
    next: INTEGER;
    w: Words;
    nextSession: Session;
  END;

(* A "Session" "s" is in use iff "s.nextSession = s". *)

PROCEDURE NewSession (): Session =
  VAR s: Session; BEGIN
    LOCK mu DO
      IF availList = NIL THEN
        s := NEW (Session);
        s.w := NEW (Words, InitBuffSz);
        s.w[0] := 0; (* Zero *)
        s.w[1] := 1  (* One  *)
      ELSE
        s := availList;
        availList := s.nextSession;
        DEC(availListSz)
      END
    END;
    s.next := 2;
    s.nextSession := s;
    RETURN s;
  END NewSession;

PROCEDURE EndSession (s: Session) =
  BEGIN
    <* ASSERT s.nextSession = s *>
    LOCK mu DO
      IF availListSz < MaxAvailListSz THEN
        s.nextSession := availList;
        availList := s;
        INC(availListSz)
      ELSE
        s.nextSession := NIL
      END
    END;
    (***
    maxHighWater := MAX (maxHighWater, s.next);
    INC (highWater [MIN (s.next DIV 10, LAST (highWater))]);
    INC (nSessions);
    ***)
  END EndSession;

(*---------------------------------------------------------------- values ---*)

CONST 
  Used     = 28;
  Reserved = Word.Size - Used;
  SigBits  = Word.RightShift (Word.Not (0), Reserved);
  TopBit   = 16_08000000;

CONST
  WS = 32;
  M = WS - Used;
  
CONST (* masks to assemble 28-bit units from 32-bit words *)
  Mask1  = 16_0fffffff; (* bottom 28 *)
  Mask2B = 16_f0000000; (* top 4 of bottom 32 *)
  Mask2A = 16_00ffffff; (* bottom 24 of top 32 *)
  Mask3  = 16_ff000000; (* top 8 of top 32 *)

TYPE
  Ptr = UNTRACED REF Word.T;

PROCEDURE New (s: Session;  a, b: INTEGER): T =
  VAR
    res: T;
    x0, x1, x2: INTEGER;
    p: UNTRACED REF ARRAY [0..2] OF Word.T;
  BEGIN
    IF (a # 0) THEN
      x0 := Word.And (b, Mask1);
      x1 := Word.Or (Word.RightShift (Word.And (b, Mask2B), 28),
                     Word.LeftShift (Word.And (a, Mask2A), 4));
      x2 := Word.RightShift (Word.And (a, Mask3), 24);
      IF (x2 # 0)
        THEN p := InitValue (s, 3, res);  p[0] := x0; p[1] := x1; p[2] := x2;
        ELSE p := InitValue (s, 2, res);  p[0] := x0; p[1] := x1;
      END;
    ELSE (* a = 0 *)
      x0 := Word.And (b, Mask1);

      x1 := Word.RightShift (Word.And (b, Mask2B), 28);
      IF (x1 # 0)
        THEN p := InitValue (s, 2, res);  p[0] := x0; p[1] := x1;
        ELSE p := InitValue (s, 1, res);  p[0] := x0;
      END;
    END;
    RETURN res;
  END New;

(* Create a new bigint from array in *)
PROCEDURE NewFromArr (s: Session; in : RefInt) : T =
  VAR
    res : T;
    p : Ptr;
    out : RefInt;
    xl,xz,xo,x,leftExtract,n,len,olen,N : INTEGER := 0;
  BEGIN
    N := NUMBER(in^);

    olen := N * WS DIV Used;
    IF N * WS MOD Used > 0 THEN INC(olen); END;
    out := NEW(RefInt, olen);

    FOR i := 0 TO N - 1 DO
      x := in[i];
      xz := Word.Insert(x, 0, WS - (n+M), n+M); 
      xl := Word.LeftShift(xz, n);
      xo := Word.Or(xl, leftExtract);
      out[len] := xo;
      INC(len); 
      INC(n,M);
      leftExtract := Word.Extract(x, WS - n, n);
      IF n = Used THEN
        out[len] := leftExtract; INC(len); n := 0; leftExtract := 0;
      END;
    END;
    IF leftExtract > 0 THEN
      out[len] := leftExtract; INC(len);
    END;
    
    p := InitValue (s, len, res);
    
    FOR j := 0 TO len - 1 DO
      p^ := out[j];
      INC (p, ADRSIZE (p^));
    END;
    FixSize(s,res);
    RETURN res;
  END NewFromArr;
  
PROCEDURE copy (s: Session;  READONLY a: T): T =
  VAR res: T;
  BEGIN
    EVAL InitValue (s, a.s, res);
    SUBARRAY (s.w^, res.w, a.s) := SUBARRAY (s.w^, a.w, a.s);
    RETURN res;
  END copy;
 
PROCEDURE add (s: Session;  READONLY a,b: T): T =
  VAR
    res: T;
    carry := 0;
    x: Word.T;
    ap, bp, cp: Ptr;
    a_w, b_w, a_s, b_s: INTEGER;
  BEGIN
    IF a.s < b.s THEN (* swap a & b *)
      a_w := b.w;  a_s := b.s;
      b_w := a.w;  b_s := a.s;
    ELSE
      a_w := a.w;  a_s := a.s;
      b_w := b.w;  b_s := b.s;
    END;
    (* INV: a.s >= b.s *)

    cp := InitValue (s, a_s + 1, res);
    VAR s_base := ADR (s.w[0]); BEGIN
      ap := s_base + a_w * ADRSIZE (Word.T);  (* = ADR (s.w[a.w]) *)
      bp := s_base + b_w * ADRSIZE (Word.T);  (* = ADR (s.w[b.w]) *)
    END;

    FOR i := 0 TO b_s - 1 DO
      x := Word.Plus (Word.Plus (ap^, bp^), carry);
      cp^ := Word.And (x, SigBits);
      carry := Word.RightShift (x, Used);
      INC (ap, ADRSIZE (ap^));
      INC (bp, ADRSIZE (bp^));
      INC (cp, ADRSIZE (cp^));
    END;

    FOR i := b_s TO a_s - 1 DO
      x := Word.Plus (ap^, carry);
      cp^ := Word.And (x, SigBits);
      carry := Word.RightShift (x, Used);
      INC (ap, ADRSIZE (ap^));
      INC (cp, ADRSIZE (cp^));
    END;

    cp^ := carry;

    FixSize (s, res);
    RETURN res;
  END add;

PROCEDURE diff (s: Session;  READONLY a,b: T): T =
  VAR
    res: T;
    borrow := 0;
    a_s    := a.s;
    b_s    := b.s;
    ap, bp, cp: Ptr;
    x: Word.T;
  BEGIN
    <* ASSERT a_s >= b_s *>
    cp := InitValue (s, a_s, res);
    VAR s_base := ADR (s.w[0]); BEGIN
      ap := s_base + a.w * ADRSIZE (Word.T);  (* = ADR (s.w[a.w]) *)
      bp := s_base + b.w * ADRSIZE (Word.T);  (* = ADR (s.w[b.w]) *)
    END;

    FOR i := 0 TO b_s - 1 DO
      x := Word.Minus (Word.Minus (ap^, bp^), borrow);
      cp^ := Word.And (x, SigBits);
      borrow := Word.And (Word.RightShift (x, Used), 1);
      INC (ap, ADRSIZE (ap^));
      INC (bp, ADRSIZE (bp^));
      INC (cp, ADRSIZE(cp^));
    END;

    FOR i := b_s TO a_s - 1 DO
      x := Word.Minus (ap^, borrow);
      cp^ := Word.And (x, SigBits);
      borrow := Word.And (Word.RightShift (x, Used), 1);
      INC (ap, ADRSIZE (ap^));
      INC (cp, ADRSIZE(cp^));
    END;

    <*ASSERT borrow = 0*>
    FixSize (s, res);
    RETURN res;
  END diff;

PROCEDURE compare (s: Session;  READONLY a, b: T): [-1..1] =
  VAR
    ap, bp : Ptr;
    a_s    := a.s;
    b_s    := b.s;
  BEGIN
    IF    a_s < b_s THEN  RETURN -1;
    ELSIF a_s > b_s THEN  RETURN +1; 
    END;

    VAR s_base := ADR (s.w[0]) + (a_s - 1) * ADRSIZE (Word.T); BEGIN
      ap := s_base + a.w * ADRSIZE (Word.T);  (* = ADR (s.w[a.w + a.s - 1]) *)
      bp := s_base + b.w * ADRSIZE (Word.T);  (* = ADR (s.w[b.w + a.s - 1]) *)
    END;

    FOR i := a_s - 1 TO 0 BY -1 DO
      IF    Word.LT (ap^, bp^) THEN  RETURN -1;
      ELSIF Word.GT (ap^, bp^) THEN  RETURN +1;
      END;
      DEC (ap, ADRSIZE (ap^));
      DEC (bp, ADRSIZE (bp^));
    END;

    RETURN 0;
  END compare;

PROCEDURE max (s: Session;  READONLY a, b: T): T =
  BEGIN
    IF compare (s, a, b) < 0
      THEN RETURN copy (s, b);
      ELSE RETURN copy (s, a);
    END;
  END max;

PROCEDURE shift (s: Session;  READONLY a: T; n: INTEGER): T =
  (* to the left for positive n, to the right for negative n  ==> a*2^n *)
  VAR res: T;  k: INTEGER;  carry := 0;  ap, cp: Ptr;
  BEGIN
    IF n = 0 OR a.s = 0 THEN  RETURN copy (s, a);  END;

    IF n > 0 THEN
      (* shift left  *)
      k := n DIV (Used);
      n := n MOD (Used);
      cp := InitValue (s, a.s + k + 1, res);
      ap := ADR (s.w[a.w]);

      FOR i := 0 TO k - 1 DO
        cp^ := 0;
        INC (cp, ADRSIZE (cp^));
      END;
      FOR i := 0 TO a.s - 1 DO
        cp^ := Word.Or (Word.And (Word.LeftShift (ap^, n), SigBits), carry);
        carry := Word.RightShift (ap^, Used - n);
        INC (ap, ADRSIZE (ap^));
        INC (cp, ADRSIZE (cp^));
      END;
      cp^ := carry;

    ELSE (* n < 0 *)
      (* shift right *)
      k := (-n) DIV (Used);
      n := (-n) MOD (Used);
      EVAL InitValue (s, a.s - k, res);

      WITH w = s.w^ DO
        ap := ADR (w[a.w + a.s - 1]);
        cp := ADR (w[res.w + res.s - 1]);
      END;

      FOR i := a.s - k - 1 TO 0 BY -1 DO
        cp^ := Word.Or (carry, Word.RightShift (ap^, n));
        carry := Word.And (Word.LeftShift (ap^, Used - n), SigBits);
        DEC (ap, ADRSIZE (ap^));
        DEC (cp, ADRSIZE (cp^));
      END;
    END;

    FixSize (s, res);
    RETURN res;
  END shift;

PROCEDURE times2 (s: Session;  READONLY a: T): T =
  (* ==> shift left 1 bit *)
  VAR
    res: T;
    new_sz: INTEGER;
    carry := 0;
    ap, cp: Ptr;
    a_s := a.s;
    a_w := a.w;
  BEGIN
    IF a_s = 0 THEN  RETURN copy (s, a);  END;

    new_sz := a_s;
    IF Word.And (s.w[a_w + a_s - 1], TopBit) # 0 THEN INC (new_sz); END;

    cp := InitValue (s, new_sz, res);
    ap := ADR (s.w[a_w]);

    FOR i := 0 TO a_s - 1 DO
      cp^ := Word.Or (Word.And (Word.LeftShift (ap^, 1), SigBits), carry);
      carry := Word.RightShift (ap^, Used - 1);
      INC (ap, ADRSIZE (ap^));
      INC (cp, ADRSIZE (cp^));
    END;
    IF (carry # 0) THEN cp^ := carry; END;

    RETURN res;
  END times2;

PROCEDURE timesTenInPlace (s: Session;  VAR a: T): T =
  VAR res: T;  carry := 0;  x: Word.T;  VAR ap: Ptr := ADR (s.w[a.w]);
  BEGIN
    FOR i := 0 TO a.s - 1 DO
      x := Word.Plus (Word.Times (ap^, 10), carry);
      ap^ := Word.And (x, SigBits);
      carry := Word.RightShift (x, Used);
      INC (ap, ADRSIZE (ap^));
    END;

    IF carry = 0 THEN RETURN a; END;

    EVAL InitValue (s, a.s+1, res);
    SUBARRAY (s.w^, res.w, a.s) := SUBARRAY (s.w^, a.w, a.s);
    s.w[res.w+a.s] := carry;
    a := res;
    RETURN a;
  END timesTenInPlace;

PROCEDURE divideTen (s: Session;  READONLY a: T): T =
  (* upper *)
  VAR res: T;  carry := 0;  x: INTEGER;  ap, cp: Ptr;
  BEGIN
    EVAL InitValue (s, a.s, res);

    WITH w = s.w^, dw = a.s - 1 DO
      ap := ADR (w[a.w + dw]);
      cp := ADR (w[res.w + dw]);
    END;

    FOR i := a.s - 1 TO 0 BY -1 DO
      x := Word.Or (Word.LeftShift (carry, Used), ap^);
      cp^ := Word.Divide (x, 10);
      carry := Word.Mod (x, 10);
      DEC (ap, ADRSIZE (ap^));
      DEC (cp, ADRSIZE (cp^));
    END;

    FixSize (s, res);
    IF carry # 0 THEN res := add (s, res, One); END;
    RETURN  res;
  END divideTen;

PROCEDURE divmod (s: Session;  READONLY a, b : T;  VAR(*OUT*) d: INTEGER): T =
  (* The div is known to be a base B digit *)
  VAR n := 1;  nb := b;  n1b := Zero;
  BEGIN
    WHILE compare (s, a, nb) >= 0 DO
      n1b := nb;
      INC (n);
      nb := add (s, nb, b);
    END;
    d := n - 1;
    RETURN diff (s, a, n1b);
  END divmod;
  
PROCEDURE mult (s: Session;  READONLY a, b: T) : T =
  CONST
    SigBitsL = VAL(SigBits,Long.T);
  VAR 
    res : T;
    x,y,z,c,carry : Long.T;  
    ap, bp, cp, cp0 : Ptr ;
  BEGIN
    cp := InitValue (s, a.s + b.s + 1, res);
  
    cp0 := ADR (s.w[res.w]);
    FOR i := 0 TO res.s - 1 DO
      cp0^ := 0;
      INC (cp0, ADRSIZE (cp0^));
    END;

    bp := ADR (s.w[b.w]);
    cp0 := ADR (s.w[res.w]);

    FOR i := 0 TO b.s - 1 DO
      ap := ADR (s.w[a.w]);
      IF bp^ # 0 THEN
        carry := 0L;
        cp := cp0;
        FOR j := 0 TO a.s - 1 DO
          x := VAL(ap^,Long.T); y := VAL(bp^,Long.T); c := VAL(cp^,Long.T);
          z := Long.Plus(Long.Times(x, y), Long.Plus(c, carry));
          carry := Long.RightShift (z, Used);
          cp^ := VAL(Long.And(z,SigBitsL), Word.T);
          INC (ap, ADRSIZE (ap^));
          INC (cp, ADRSIZE (cp^));
        END;
        cp^ := VAL(carry,Word.T);
      END;
      INC (cp0, ADRSIZE (cp0^));      
      INC (bp, ADRSIZE (bp^));
    END;
    FixSize (s, res);
    RETURN res;
  END mult;

PROCEDURE ToArr32(s : Session; a : T; VAR out : RefInt32; VAR len : INTEGER) =
  VAR 
    y,z : Word.T := 0;
    n,j : INTEGER := 0;
    yp,zp : Ptr := ADR (s.w[a.w]);
    ep : Ptr;
  BEGIN
    (* assume the allocator zeros this array, min size 128 bytes for quads *)
    out := NEW(RefInt32,MAX(a.s, 4));
    IF a.s = 0 THEN
      len := 0;
      RETURN;
    END;
    INC (zp, ADRSIZE (yp^));
    ep := ADR (s.w[a.w + a.s - 1]);
    y := Word.Extract(yp^, n, Used - n);

    WHILE yp < ep  DO
      z := Word.LeftShift(zp^,Used - n);
      out[j] := Fix32(Word.Or(y,z)); INC(j);
      n := (n + M ) MOD Used;
      yp := zp; INC (zp, ADRSIZE (yp^));
      IF n = 0 THEN
        yp := zp; INC (zp, ADRSIZE (yp^));
      END;
      y := Word.Extract(yp^, n, Used - n);
    END; 
    IF y > 0 THEN out[j] := y; INC(j); END;
    len := j;  
  END ToArr32;
  
(*---------------------------------------------------- internal utilities ---*)

PROCEDURE InitValue (s: Session;  n_words: INTEGER;  VAR(*OUT*) t: T): ADDRESS=
  (* allocates space in s.w, initializes t, and returns the address
     of the first word of t *)
  BEGIN
    (***
    INC (allocates [MIN (n_words, LAST (allocates))]);
    INC (nAllocates);
    ***)
    t.s := n_words;
    t.w := s.next;
    INC (s.next, n_words);
    IF (s.next >= NUMBER (s.w^)) THEN Expand (s); END;
    RETURN ADR (s.w[t.w]);
  END InitValue;

PROCEDURE Expand (s: Session) =
  VAR
    n   := NUMBER (s.w^);
    new := NEW (Words, n + n);
  BEGIN
    SUBARRAY (new^, 0, n) := s.w^;
    s.w := new;
  END Expand;

PROCEDURE FixSize (s: Session;  VAR a: T) =
  (* computes a.s from a.w, by discarding the null most significant words *)
  VAR ap: Ptr := ADR (s.w[a.w + a.s - 1]);
  BEGIN
    WHILE a.s > 0 AND ap^ = 0 DO
      DEC (a.s);
      DEC (ap, ADRSIZE (ap^));
    END;
  END FixSize;

PROCEDURE Fix32 (x: Word.T): Int32 =
  (* return the sign-extended bottom 32 bits of 'x' *)
  CONST
    SigBits = 16_ffffffff; (* mask to grab 32 significant bits *)
    Sign = 16_80000000;
    SignExtend = Word.LeftShift (Word.Not (0), 31);
  BEGIN
    IF Word.And (x, Sign) = 0
      THEN RETURN Word.And (x, SigBits);
      ELSE RETURN Word.Or (SignExtend, Word.And (x, SigBits));
    END;
  END Fix32;
  
BEGIN
END DragonInt.
