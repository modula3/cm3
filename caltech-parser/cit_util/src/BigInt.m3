(* $Id$ *)

MODULE BigInt;
IMPORT CardSeq, Integer;
IMPORT CharSeq;
IMPORT TextWr;
IMPORT Wr, Thread;
IMPORT Word;

CONST Base = 1000; (* must be less than or equal to sqrt(LAST(CARDINAL)) *)

(********************* sequence of cardinals *********************)

TYPE 
  Seq = CardSeq.T OBJECT METHODS
    clearTop() := ClearTop;
    copy() : Seq := CopySeq;
  OVERRIDES
    put := MyPut; 
    get := MyGet
  END;

PROCEDURE CopySeq(s : Seq) : Seq = 
  VAR 
    res := NEW(Seq).init();
  BEGIN
    FOR i := 0 TO s.size() - 1 DO
      res.put(i,s.get(i))
    END;
    RETURN res
  END CopySeq;

PROCEDURE ClearTop(s : Seq) =
  BEGIN
    FOR i := s.size() - 1 TO 0 BY -1 DO
      IF CardSeq.T.get(s,i) # 0 THEN RETURN END;
      EVAL s.remhi()
    END
  END ClearTop;

PROCEDURE MyPut(s : Seq; i : CARDINAL; READONLY x : CARDINAL) =
  BEGIN
    FOR j := 1 TO i - s.size() + 1 DO s.addhi(0) END;
    CardSeq.T.put(s,i,x);
    s.clearTop();
  END MyPut;

PROCEDURE MyGet(s : Seq; i : CARDINAL) : CARDINAL =
  BEGIN
    TRY
      FOR j := 1 TO i - s.size() + 1 DO s.addhi(0) END;
      RETURN CardSeq.T.get(s,i)
    FINALLY
      s.clearTop()
    END
  END MyGet;

(* other sequence impl *)

TYPE 
  NArry = REF ARRAY OF CARDINAL;

  NSeq = OBJECT
    siz : CARDINAL; (* # of significant digits *)
    a : NArry;
  METHODS
    init(hintSize : CARDINAL := 5) : NSeq := InitN;
    shift(sa : CARDINAL) := ShiftN;
    clearTop() := CTN;
    extend(toBits : CARDINAL) := ExtendN;
    copy() : NSeq := CopyN;
    size() : CARDINAL := SizeN;
  END;

PROCEDURE InitN(s : NSeq; hintSize : CARDINAL) : NSeq =
  BEGIN
    s.siz := 0;
    s.a := NEW(NArry, hintSize);
    FOR i := FIRST(s.a^) TO LAST(s.a^) DO s.a[i] := 0 END;
    RETURN s
  END InitN;

PROCEDURE ShiftN(s : NSeq; sa : CARDINAL) =
  VAR
    os := s.siz;
  BEGIN
    INC(s.siz,sa);
    s.extend(s.siz);
    SUBARRAY(s.a^,sa,os) := SUBARRAY(s.a^,0,os);
    FOR i := 0 TO sa - 1 DO s.a[i] := 0 END;
  END ShiftN;

PROCEDURE ExtendN(s : NSeq; toDigits : CARDINAL) =
  BEGIN
    IF NUMBER(s.a^) < toDigits THEN
      VAR 
        na := NEW(NArry,toDigits);
      BEGIN
        SUBARRAY(na^,0,NUMBER(s.a^)) := s.a^;

        FOR i := NUMBER(s.a^) TO LAST(na^) DO na[i] := 0 END;

        s.a := na
      END
    END
  END ExtendN;

PROCEDURE SizeN(s : NSeq) : CARDINAL = BEGIN RETURN s.siz END SizeN;

PROCEDURE CopyN(s : NSeq) : NSeq =
  VAR
    na := NEW(NArry, NUMBER(s.a^));
  BEGIN 
    na^ := s.a^;
    RETURN NEW(NSeq, siz := s.siz, a := na) 
  END CopyN;

PROCEDURE CTN(s : NSeq) = 
  BEGIN 
    FOR i := LAST(s.a^) TO 0 BY -1 DO
      IF s.a[i] = 0 THEN s.siz := i ELSE EXIT END
    END
  END CTN;

(********************* bignum type *********************)

REVEAL 
  T = Public BRANDED Brand OBJECT
    sign : [-1..1];
    rep : NSeq;
  END;

PROCEDURE Compare(a, b : T) : CompRet =
  BEGIN
    IF a.sign < b.sign THEN RETURN -1
    ELSIF a.sign > b.sign THEN RETURN 1
    END;

    IF a.rep.size() > b.rep.size() THEN RETURN a.sign 
    ELSIF a.rep.size() < b.rep.size() THEN RETURN -a.sign
    END;

    FOR i := a.rep.size() - 1 TO 0 BY -1 DO
      VAR
        c := Integer.Compare(a.rep.a[i],b.rep.a[i]);
      BEGIN
        IF c # 0 THEN RETURN a.sign * c END
      END
    END;

    RETURN 0
  END Compare;

PROCEDURE Equal(a, b : T) : BOOLEAN = BEGIN RETURN Compare(a,b) = 0 END Equal;

PROCEDURE NewInternal(x : [FIRST(INTEGER)+1..LAST(INTEGER)]) : T =
  VAR
    c := ABS(x);
    s := NEW(NSeq).init(1);
  BEGIN 
    s.a[0] := c;
    s.siz := 1;

    Renormalize(s);

    IF x >= 0 THEN
      RETURN NEW(T, sign := 1, rep := s)
    ELSE
      RETURN NEW(T, sign := -1, rep := s)
    END
  END NewInternal;

PROCEDURE New(x : INTEGER) : T =
  BEGIN
    IF x = FIRST(INTEGER) THEN
      RETURN Sub(New(FIRST(INTEGER)+1),One)
    ELSE
      RETURN NewInternal(x)
    END
  END New;

PROCEDURE Sign(a : T) : CompRet =
  BEGIN
    IF a.rep.siz = 0 THEN RETURN 0; END;
    RETURN a.sign;
  END Sign;

PROCEDURE Divide(a, b : T; VAR q, r : T) = 
  (* first do a C-style divide *)
  VAR
    sign := a.sign * b.sign;
    aa := Abs(a);
    bb := Abs(b);
  BEGIN
    DivideUnsigned(aa,bb,q,r);
    IF sign < 0 THEN q := Neg(q); r := Neg(r) END;

    (* if the remainder is not 0 we need to fix up the result *)
    IF r.rep.siz # 0 THEN

      (* r in [0,b) *)
      IF sign # b.sign THEN r := Add(r, b); END;

      (* a-r=b*q *)
      IF sign = -1 THEN q := Sub(q, One); END;

    END;
  END Divide;

PROCEDURE Div(a, b : T) : T =
  VAR
    q, r : T;
  BEGIN
    Divide(a,b,q,r);
    RETURN q
  END Div;

PROCEDURE Mod(a, b : T) : T =
  VAR
    q, r : T;
  BEGIN
    Divide(a,b,q,r);
    RETURN r
  END Mod;
    
PROCEDURE Shift(a : T; sa : CARDINAL) : T =
  VAR
    seq := a.rep.copy();
  BEGIN
    seq.shift(sa);
    RETURN NEW(T, sign := a.sign, rep := seq)
  END Shift;

PROCEDURE DivideUnsigned(aparm, b : T; VAR q, r : T) =
  VAR
    s : T;
    lo, hi : CARDINAL;
  BEGIN
    q := Zero;
    r := aparm;
    <* ASSERT aparm.sign = 1 AND b.sign = 1 *>
    
    FOR sa := aparm.rep.size() - b.rep.size() TO 0 BY -1 DO
      s := Shift(b,sa);
      
      (* search for digit ... *)
      lo := 0;
      hi := Base;

      WHILE lo < hi - 1 DO
        VAR
          mid  := (lo + hi) DIV 2;

          (*
            loM  := Mul(New(lo),s);
            hiM  := Mul(New(hi),s);
          *)

          midM := Mul(New(mid),s);
          midC := Compare(midM,r);
        BEGIN
          (*<* ASSERT Compare(loM,r) <= 0 AND Compare(hiM,r) >= 0 *>*)
          CASE midC OF
            -1 => lo := mid
          |
             0 => q := Add(Shift(New(mid),sa),q); r := Zero; RETURN
          |
             1 => hi := mid
          END
        END
      END;

      q := Add(Shift(New(lo),sa),q);
      r := Sub(r, Mul(New(lo),s))
    END;
    <* ASSERT Equal(aparm,Add(r,Mul(q,b))) *>
  END DivideUnsigned;

PROCEDURE Mul(a, b : T) : T =
  BEGIN
    RETURN NEW(T, sign := a.sign * b.sign, rep := MulSeqs(a.rep,b.rep))
  END Mul;

PROCEDURE MulSeqs(a, b : NSeq) : NSeq =
  VAR 
    res := NEW(NSeq).init(a.size()+b.size()+1);
    idx : CARDINAL;
    s : CARDINAL;
  BEGIN
    FOR i := 0 TO a.size() - 1 DO
      FOR j := 0 TO b.size() - 1 DO
        idx := i+j;
        s := a.a[i] * b.a[j];
        WHILE s # 0 DO
          VAR
            o := res.a[idx];
          BEGIN
            s := s + o;
            res.a[idx] := s MOD Base;
            s := s DIV Base;
            INC(idx)
          END
        END
      END
    END;
    Renormalize(res);
    RETURN res
  END MulSeqs;

PROCEDURE Add(a, b : T) : T =
  VAR
    res : T;
  BEGIN
    IF a.sign = 1 AND b.sign = 1 THEN
      res := NEW(T, sign := 1, rep := AddSeqs(a.rep,b.rep))
    ELSIF a.sign = -1 AND b.sign = -1 THEN
      res := NEW(T, sign := -1, rep := AddSeqs(a.rep,b.rep))
    ELSIF a.sign = -1 AND b.sign = 1 THEN
      res := Sub(b,Neg(a))
    ELSIF a.sign = 1 AND b.sign = -1 THEN
      res := Sub(a,Neg(b))
    ELSE
      <* ASSERT FALSE *>
    END;
    
    Renormalize(res.rep);
    RETURN res
  END Add;

PROCEDURE Sub(a, b : T) : T =
  VAR
    res : T;
  BEGIN
    IF a.sign = 1 AND b.sign = 1 AND Compare(a,b) > -1 THEN
      res := NEW(T, sign := 1, rep := SubSeqs(a.rep,b.rep))
    ELSIF a.sign = 1 AND b.sign = 1 AND Compare(a,b) = -1 THEN
      res := NEW(T, sign := -1, rep := SubSeqs(b.rep,a.rep))
    ELSIF a.sign = -1 AND b.sign = -1 THEN
      res := Neg(Sub(Neg(a),Neg(b)))
    ELSIF a.sign = -1 AND b.sign = 1 THEN
      res := Neg(Add(Neg(a),b))
    ELSIF a.sign = 1 AND b.sign = -1 THEN
      res := Add(a,b)
    ELSE
      <* ASSERT FALSE *>
    END;
    
    Renormalize(res.rep);
    RETURN res
  END Sub;

(* unsigned addition of underlying sequences *)
PROCEDURE AddSeqs(s, t : NSeq) : NSeq =
  VAR
    m := MAX(s.size(),t.size());
    r := NEW(NSeq).init(m+1);
  BEGIN
    s.extend(m); t.extend(m);
    FOR i := 0 TO m - 1 DO
      r.a[i] := s.a[i] + t.a[i]
    END;
    s.clearTop(); t.clearTop();
    RETURN r
  END AddSeqs;

(* unsigned subtraction of underlying sequences *)
(* s must be .ge. t *)
PROCEDURE SubSeqs(s, t : NSeq) : NSeq =
  VAR
    tsiz := t.size();
    m := MAX(s.size(),tsiz);
    r := NEW(NSeq).init(m+1);
    borrow := 0;
  BEGIN
    FOR i := 0 TO m - 1 DO
      VAR
        diff := s.a[i] + borrow;
      BEGIN
        IF i<tsiz THEN DEC(diff, t.a[i]); END;
        borrow := 0;

        WHILE diff < 0 DO
          diff := diff + Base;
          borrow := borrow - 1
        END;
        r.a[i] := diff
      END
    END;
    <* ASSERT borrow = 0 *>
    RETURN r
  END SubSeqs;

PROCEDURE Abs(a : T) : T = 
  BEGIN RETURN NEW(T, sign := 1, rep := a.rep) END Abs;

PROCEDURE Renormalize(a : NSeq) = 
  VAR
    carry := 0;
    o : CARDINAL;
    i := 0;
  BEGIN
    a.clearTop();

    WHILE i < a.siz OR carry # 0 DO
      
      IF i >= NUMBER(a.a^) THEN a.extend(NUMBER(a.a^)+4) END;
      o := a.a[i] + carry;
      a.a[i] := o MOD Base;

      IF i >= a.siz AND a.a[i] # 0 THEN a.siz := i + 1 END;

      carry := o DIV Base;
      INC(i)
    END;
  END Renormalize;

PROCEDURE Neg(a : T) : T = 
  BEGIN RETURN NEW(T, rep := a.rep, sign := -a.sign) END Neg;

PROCEDURE Format(a : T; base : CARDINAL) : TEXT = 
  <* FATAL Wr.Failure, Thread.Alerted *>
  CONST
    HexChars = ARRAY OF CHAR{'0','1','2','3','4','5','6','7','8','9',
                             'A','B','C','D','E','F'};
  VAR
    c := NEW(CharSeq.T).init();
    s := Sign(a);
    wr := NEW(TextWr.T).init();
    MyBase := New(base);
  BEGIN
    <* ASSERT base <= 16 *>
    a := Abs(a);

    IF Equal(a,Zero) THEN RETURN "0" END;

    WHILE NOT Equal(a,Zero) DO
      VAR
        d : T;
      BEGIN
        DivideUnsigned(a,MyBase,a,d);
        <* ASSERT Compare(d,Zero) >= 0 AND Compare(d,MyBase) < 1 *>
        c.addlo(HexChars[d.rep.a[0]]);
      END
    END;
    IF s = -1 THEN c.addlo('-') END;

    FOR i := 0 TO c.size() - 1 DO
      Wr.PutChar(wr, c.get(i))
    END;

    RETURN TextWr.ToText(wr)
  END Format;

PROCEDURE Hash(a : T) : Word.T = 
  VAR
    res : Word.T := 0;
  BEGIN
    FOR i := 0 TO a.rep.size() - 1 DO
      res := Word.Plus(res,a.rep.a[i])
    END;
    RETURN res
  END Hash;

PROCEDURE Max(a, b : T) : T =
  BEGIN IF Compare(a,b) = 1 THEN RETURN a ELSE RETURN b END END Max;

PROCEDURE Min(a, b : T) : T = 
  BEGIN IF Compare(a,b) = -1 THEN RETURN a ELSE RETURN b END END Min;

PROCEDURE ToLongReal(a : T) : LONGREAL = 
  BEGIN
    IF a.rep.siz = 0 THEN RETURN 0.0D0; END;
    VAR
      res := FLOAT(a.rep.a[a.rep.siz-1],LONGREAL);
    BEGIN
      FOR i := a.rep.siz - 2 TO 0 BY -1 DO
        res := res * FLOAT(Base,LONGREAL);
        res := res + FLOAT(a.rep.a[i],LONGREAL)
      END;
      RETURN res;
    END;
  END ToLongReal;

PROCEDURE ToInt(a : T) : INTEGER RAISES { OutOfRange } =
  BEGIN 
    IF Compare(a, IntFirst) = -1 THEN 
      RAISE OutOfRange
    ELSIF Compare(a, IntLast) = +1 THEN 
      RAISE OutOfRange
    ELSE
      WITH lr = ToLongReal(a) DO
        RETURN ROUND(lr)
      END
    END
  END ToInt;

VAR
  IntLast, IntFirst : T;
BEGIN 
  Zero := New(0);
  One := New(1);
  IntLast  := New(LAST(INTEGER));
  IntFirst := New(FIRST(INTEGER)); (* depends on One *)
END BigInt.
