MODULE BigIntegerRep;
(*Arithmetic for Modula-3, see doc for details

   Abstract: Integers of arbitrary size

   Daniel Beer *)

IMPORT Word AS W;
IMPORT WordEx AS Wx;
FROM NADefinitions IMPORT Error, Err;
FROM BigInteger IMPORT Zero;
(*
IMPORT IO,Fmt,BigIntegerFmtLex AS BF;
CONST base16Style = BF.FmtStyle{base:=16};
*)

<*UNUSED*>
CONST Module = "BigIntegerRep.";
(*==========================*)


PROCEDURE Clear ( (*OUT*)v: Value) =
  BEGIN
    FOR j := FIRST(v^) TO LAST(v^) DO v[j] := 0; END;
  END Clear;


<*INLINE*>
PROCEDURE MinMax (VAR min, max: INTEGER; a, b: INTEGER) =
  BEGIN
    IF a < b THEN min := a; max := b; ELSE min := b; max := a; END;
  END MinMax;



PROCEDURE CorrectSize (VAR x: T; start: INTEGER) =
  VAR j := start;

  BEGIN
    WHILE j >= 0 AND x.data[j] = 0 DO DEC(j); END;
    x.size := j + 1;
  END CorrectSize;


(*unsigned arithmetic, 'sign' entry is ignored; the signed arithmetic is
   build on the unsigned arithmetic routines*)

PROCEDURE AddU (READONLY x, y: T): T =
  VAR
    carry             := FALSE;
    min, max: INTEGER;
    z       : T;

  BEGIN
    MinMax(min, max, x.size, y.size);

    IF max = 0 THEN RETURN Zero; END;

    z.data := NEW(Value, max + 1);

    FOR j := 0 TO min - 1 DO
      z.data[j] := Wx.PlusWithCarry(x.data[j], y.data[j], carry);
    END;
    IF x.size > y.size THEN
      FOR j := min TO max - 1 DO
        z.data[j] := Wx.PlusWithCarry(x.data[j], 0, carry);
      END;
    ELSE
      FOR j := min TO max - 1 DO
        z.data[j] := Wx.PlusWithCarry(0, y.data[j], carry);
      END;
    END;
    z.data[max] := Wx.PlusWithCarry(0, 0, carry);

    CorrectSize(z, max);
    RETURN z;
  END AddU;

(* You must make sure that x >= y *)
PROCEDURE SubU (READONLY x, y: T): T =
  VAR
    carry             := FALSE;
    min, max: INTEGER;
    z       : T;

  BEGIN
    MinMax(min, max, x.size, y.size);

    IF max = 0 THEN RETURN Zero; END;

    z.data := NEW(Value, max);

    FOR j := 0 TO min - 1 DO
      z.data[j] := Wx.MinusWithBorrow(x.data[j], y.data[j], carry);
    END;
    IF x.size > y.size THEN
      FOR j := min TO max - 1 DO
        z.data[j] := Wx.MinusWithBorrow(x.data[j], 0, carry);
      END;
    ELSE
      FOR j := min TO max - 1 DO
        z.data[j] := Wx.MinusWithBorrow(0, y.data[j], carry);
      END;
    END;
    <*ASSERT NOT carry*>(*otherwise it was x<y*)

    CorrectSize(z, max - 1);
    RETURN z;
  END SubU;

PROCEDURE CompareU (READONLY x, y: T): [-1 .. 1] =
  BEGIN
    IF x.size < y.size THEN
      RETURN -1
    ELSIF x.size > y.size THEN
      RETURN 1
    ELSE
      FOR j := x.size - 1 TO 0 BY -1 DO
        IF W.LT(x.data[j], y.data[j]) THEN
          RETURN -1
        ELSIF W.GT(x.data[j], y.data[j]) THEN
          RETURN 1
        END;
      END;
      RETURN 0;
    END;
  END CompareU;


PROCEDURE MulU (READONLY x, y: T): T =
  VAR
    m, lo, hi, oldhi: W.T;
    carry           : BOOLEAN;
    z               : T;

  BEGIN
    IF (x.size = 0) OR (y.size = 0) THEN RETURN Zero; END;

    z.data := NEW(Value, x.size + y.size);

    (*initialize result data*)
    m := x.data[0];
    hi := 0;
    FOR k := 0 TO y.size - 1 DO
      oldhi := hi;
      Wx.DoubleLengthMultiply(m, y.data[k], lo, hi);
      carry := FALSE;
      lo := Wx.PlusWithCarry(lo, oldhi, carry);
      hi := Wx.PlusWithCarry(hi, 0, carry);
      z.data[k] := lo;
    END;
    z.data[y.size] := hi;
    FOR k := y.size + 1 TO z.size - 1 DO z.data[k] := 0; END;

    FOR j := 1 TO x.size - 1 DO
      m := x.data[j];
      hi := 0;
      FOR k := 0 TO y.size - 1 DO
        oldhi := hi;
        Wx.DoubleLengthMultiply(m, y.data[k], lo, hi);
        carry := FALSE;
        lo := Wx.PlusWithCarry(lo, oldhi, carry);
        hi := Wx.PlusWithCarry(hi, 0, carry);
        carry := FALSE;
        lo := Wx.PlusWithCarry(lo, z.data[j + k], carry);
        hi := Wx.PlusWithCarry(hi, 0, carry);
        z.data[j + k] := lo;
      END;
      z.data[j + y.size] := hi;
    END;

    CorrectSize(z, x.size + y.size - 1);
    RETURN z;
  END MulU;


(**
General problem with division:
We cannot easily find a digit (word) of the quotient
if we only know the most significant digits of dividend and divisor.
Thus we only determine a close lower estimate
and we don't try to obtain the quotient digit by digit
but we move with a varying number of bits
and accumulate the quotient.

That's the theory:

We want to determine the division of x by y, that is
for given x and y we search for q und r with:

x = q*y + r  and  0<=r  and  r<y

But instead of working with full precision x and y
we use approximations x' and y', respectively.
For these x' and y' we determine q' and r' with

x' = q'*y' + r'  and  0<=r' (preserving r'<y' is a problem)

If we round x down to x' and y up to y'
then q' <= q. We will multiply the approximative quotient q'
by the exact divisor y and then we will subtract this product
off x. This way we reduce x to x-y*q' and thus simplify the problem.
This procedure is iterated until the remaining x is smaller than y,
this will be the remainder r.

Will the x always be shortened, i.e. does the algorithm terminate?

What we want to estimate is x-y*q',
what we know are some estimates for x' and y'.
Say that our rounding has maximal relative errors a and b
(a and b are positive but hopefully small)
in this way (it looks strange but simplifies calculation)

x  >= x' >= (1-a)*x
y' >= y  >= (1-b)*y'

then it holds

x-y*q'
  = x - y*(x'-r')/y'
  = x - x'*y/y' + y*r'/y'
 <= x - x'*(1-b) + y*r'/y'   | y<=y'
 <= x - x'*(1-b) + r'
 <= x - x*(1-a)*(1-b) + r'
  = x*(a+b-ab) + r'
 <= x*(a+b) + r'

You see that the smaller a and b the faster the algorithm converges.
This is nothing surprising.
The problem is that if y' is precise the approximative division
will loose precision due to the fixed precision of the hardware division.
The loss of precision of the result will increase r'.
I still cannot model this satisfyingly.

So what are the values for a and b?

a=2^-i   b=2^-j

*)


(**
PROCEDURE FmtBitPos (sh : BitPos) : TEXT =
BEGIN
  RETURN Fmt.FN("{%s,%s}",
            ARRAY OF TEXT {Fmt.Int(sh.word),Fmt.Int(sh.bit)});
END FmtBitPos;

PROCEDURE FmtBig (x : T) : TEXT =
BEGIN
  RETURN Fmt.FN("(size %s) 16_%s",
       ARRAY OF TEXT {Fmt.Int(x.size),BF.Fmt(x,style:=base16Style)});
END FmtBig;
*)


PROCEDURE SubBitPos (READONLY x, y: BitPos): BitPos =
  BEGIN
    IF x.bit >= y.bit THEN
      RETURN BitPos{x.word - y.word, x.bit - y.bit};
    ELSE
      RETURN BitPos{x.word - y.word - 1, W.Size - y.bit + x.bit};
    END;
  END SubBitPos;

PROCEDURE AddBitPos (READONLY x, y: BitPos): BitPos =
  BEGIN
    IF x.bit + y.bit < W.Size THEN
      RETURN BitPos{x.word + y.word, x.bit + y.bit};
    ELSE
      RETURN BitPos{x.word + y.word + 1, x.bit + y.bit - W.Size};
    END;
  END AddBitPos;

<*INLINE*>
PROCEDURE CompareBitPos (READONLY x, y: BitPos): [-1 .. 1] =
  BEGIN
    IF x.word < y.word THEN
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

PROCEDURE BitPosEndToBegin (READONLY x: BitPos): BitPos =
  BEGIN
    IF x.bit < W.Size - 1 THEN
      RETURN BitPos{x.word - 1, x.bit + 1};
    ELSE
      RETURN BitPos{x.word, 0};
    END;
  END BitPosEndToBegin;

PROCEDURE GetMSBPos (READONLY x: T): BitPos =
  BEGIN
    (*
    IO.Put(Fmt.FN("GetMSBPos (size %s) 16_%s\t",
           ARRAY OF TEXT {Fmt.Int(x.size),BF.Fmt(x,style:=base16Style)}));
    IO.Put(Fmt.FN("MSB of %s: %s\n",
           ARRAY OF TEXT {Fmt.Unsigned(x.data[x.size-1]),
                          Fmt.Int(Wx.FindMostSignifBit(x.data[x.size-1]))}));
    *)
    RETURN BitPos{x.size - 1, Wx.FindMostSignifBit(x.data[x.size - 1])};
  END GetMSBPos;

(*grab bits from sh to sh+W.Size-1*)
PROCEDURE GetSubword (READONLY x: T; sh: BitPos): W.T =
  VAR probs: W.T := 0;
  BEGIN
    (*
    IO.Put(Fmt.FN("GetSubword of %s at %s\n",ARRAY OF TEXT{FmtBig(x),FmtBitPos(sh)}));
    *)
    IF sh.bit > 0 THEN           (*avoid access to non-existing fields*)
      EVAL Wx.RightShiftWithProbscosis(x.data[sh.word + 1], sh.bit, probs);
    END;
    IF sh.word < 0 THEN
      RETURN probs;
    ELSE
      RETURN Wx.RightShiftWithProbscosis(x.data[sh.word], sh.bit, probs);
    END;
  END GetSubword;


(*x := x-SHL(y*z,sh) (inplace, make sure that x.data has enough space) *)
PROCEDURE SubShiftedProd (VAR x: T; READONLY y: T; z: W.T; sh: BitPos) =
  VAR
    lo, hi, oldhi, probs, loshft: W.T;
    carry                                 := FALSE;
    borrow                                := FALSE;
    j                           : INTEGER;
  BEGIN
    (*
    IO.Put(Fmt.FN("x 16_%s - SHL (y 16_%s * z 16_%s, {%s,%s})\n",
                ARRAY OF TEXT {
                  BF.Fmt(x,base16Style),BF.Fmt(y,base16Style),
                  Fmt.Unsigned(z),
                  Fmt.Int(sh.word),Fmt.Int(sh.bit)}));
    *)
    hi := 0;
    probs := 0;
    FOR k := 0 TO y.size - 1 DO
      oldhi := hi;
      Wx.DoubleLengthMultiply(y.data[k], z, lo, hi);
      carry := FALSE;
      lo := Wx.PlusWithCarry(lo, oldhi, carry);
      hi := Wx.PlusWithCarry(hi, 0, carry);
      <*ASSERT NOT carry*>
      loshft := Wx.LeftShiftWithProbscosis(lo, sh.bit, probs);
      x.data[sh.word + k] :=
        Wx.MinusWithBorrow(x.data[sh.word + k], loshft, borrow);
    END;
    j := sh.word + y.size;
    loshft := Wx.LeftShiftWithProbscosis(hi, sh.bit, probs);
    x.data[j] := Wx.MinusWithBorrow(x.data[j], loshft, borrow);
    INC(j);
    x.data[j] := Wx.MinusWithBorrow(x.data[j], probs, borrow);

    WHILE borrow DO
      INC(j);
      x.data[j] := Wx.MinusWithBorrow(x.data[j], 0, borrow);
    END;
    CorrectSize(x, x.size);
    (*
    IO.Put(Fmt.FN("x 16_%s\n", ARRAY OF TEXT {BF.Fmt(x,16)}));
    *)
  END SubShiftedProd;

(*x := x+SHL(y,sh) (inplace, make sure that x.data has enough space)*)
PROCEDURE AddShifted (VAR x: T; y: W.T; sh: BitPos) =
  VAR
    carry      := FALSE;
    probs: W.T := 0;
  BEGIN
    (*
    IO.Put(Fmt.FN("q 16_%s (size %s) + SHL (q' 16_%s, {%s,%s})\n",
                ARRAY OF TEXT {BF.Fmt(x,16),Fmt.Int(NUMBER(x.data^)),Fmt.Unsigned(y),
                Fmt.Int(sh.word),Fmt.Int(sh.bit)}));
    *)
    x.data[sh.word] := Wx.PlusWithCarry(x.data[sh.word],
                                        Wx.LeftShiftWithProbscosis(
                                          y, sh.bit, probs), carry);
    (*don't access fields that we need not really because they may not
       exist*)
    IF probs # 0 OR carry THEN
      INC(sh.word);
      x.data[sh.word] := Wx.PlusWithCarry(x.data[sh.word], probs, carry);
    END;
    WHILE carry DO
      INC(sh.word);
      x.data[sh.word] := Wx.PlusWithCarry(x.data[sh.word], 0, carry);
    END;
  END AddShifted;

(*x and y cannot be READONLY parameters, otherwise conflicts arise, when
   someone passes the same variable to x and r*)
PROCEDURE DivModU (x, y: T): QuotRem RAISES {Error} =
  VAR
    q, r                     : T;
    qmswstartpos             : BitPos;
    qmsbpos, rmsbpos, ymsbpos: BitPos;
    qmsw, rmsw, ymsw         : W.T;
  BEGIN
    (*
    IO.Put(Fmt.FN("DivModU (size %s) 16_%s by (size %s) 16_%s\n",
           ARRAY OF TEXT {Fmt.Int(x.size),BF.Fmt(x,style:=base16Style),
                          Fmt.Int(y.size),BF.Fmt(y,style:=base16Style)}));
    *)
    IF y.size = 0 THEN RAISE Error(Err.divide_by_zero); END;

    (*this check is necessary, we would access non-existing data
       otherwise*)
    IF x.size = 0 THEN
      RETURN QuotRem{x, x};      (*the quotient and remainder are zero,
                                    too*)
    END;

    r.data := NEW(Value, x.size + 2);
    q.data := NEW(Value, x.size - y.size + 1);
    r.size := x.size;
    SUBARRAY(r.data^, 0, r.size) := SUBARRAY(x.data^, 0, r.size);
    r.data[r.size] := 0;
    r.data[r.size + 1] := 0;
    Clear(q.data);

    (*normalize remainder and divisor temporarily divide most significant
       32 bit of r by the most significant 16 bit of y*)
    (*IO.Put("GetMSBPos (y)\t");*)
    ymsbpos := GetMSBPos(y);
    ymsw := GetSubword(y, BitPosEndToBegin(ymsbpos));
    ymsw :=
      W.RightShift(ymsw, W.Size DIV 2); (*the division algorithm is fastest
                                           if the divisor is clipped to the
                                           half number of bits compared
                                           with the approximation of the
                                           dividend*)
    INC(ymsw);                   (*round up to get a lower estimate for
                                    quotient*)

    (*IO.Put("GetMSBPos (r)\t");*)
    rmsbpos := GetMSBPos(r);
    WHILE CompareBitPos(rmsbpos, ymsbpos) > 0 DO
      rmsw := GetSubword(r, BitPosEndToBegin(rmsbpos));
      (*round down by neglecting the following bits to get a lower estimate
         for quotient*)
      qmsw := W.Divide(rmsw, ymsw);
      (*
      IO.Put(Fmt.FN("rmsw %s, ymsw %s, qmsw %s, ymsw*qmsw %s\n",
        ARRAY OF TEXT{Fmt.Unsigned(rmsw),Fmt.Unsigned(ymsw),Fmt.Unsigned(qmsw),
                      Fmt.Unsigned(W.Times(ymsw,qmsw))}));
      *)
      qmsbpos := SubBitPos(rmsbpos, ymsbpos);
      qmswstartpos := SubBitPos(qmsbpos, BitPos{0, W.Size DIV 2});
      IF qmswstartpos.word = -1 THEN
        qmsw := W.RightShift(qmsw, W.Size - qmswstartpos.bit);
        qmswstartpos.word := 0;
        qmswstartpos.bit := 0;
      ELSE
        <*ASSERT qmswstartpos.word>=0 *>
      END;
      AddShifted(q, qmsw, qmswstartpos);
      SubShiftedProd(r, y, qmsw, qmswstartpos);
      rmsbpos := GetMSBPos(r);
    END;
    (*CorrectSize (q, LAST(q.data));*)

    (*this loop will run at most three times*)
    WHILE CompareU(r, y) >= 0 DO
      (*
      r := SubU (r, y);
      q := AddU (q, One);
      *)
      AddShifted(q, 1, BitPos{0, 0});
      SubShiftedProd(r, y, 1, BitPos{0, 0});
    END;

    CorrectSize(q, LAST(q.data^));
    RETURN QuotRem{q, r};
  END DivModU;

(*==========================*)
BEGIN
END BigIntegerRep.
