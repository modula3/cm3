(* $Id: BigInt.m3,v 1.9 2003/08/21 03:28:30 kp Exp $ *)

MODULE BigInt EXPORTS BigInt, BigIntRep;

  (*
    what we should probably do is restructure this module as follows.

    make a T a simple wrapper around a RECORD that is used internally
    for all the work.

    in fact the RECORD could be exporting a different interface and
    we just use the RECORD here.  Hmm...

    The point of the T would be to provide referential transparency.

    That could just be an external generic.... working on any RECORD type!
  *)

IMPORT Integer;
IMPORT CharSeq;
IMPORT Word;
IMPORT Wx;
IMPORT Text;
IMPORT Lex, FloatMode;
IMPORT Scan AS M3Scan;
IMPORT BigIntBigIntTbl;
IMPORT Debug;
FROM Fmt IMPORT Int, F, Unsigned;


CONST doDebug = FALSE;
      
PROCEDURE InitN(VAR s : NSeq; hintSize : CARDINAL) =
  BEGIN
    s.siz := 0;
    s.a := NEW(NArry, hintSize);
    FOR i := FIRST(s.a^) TO LAST(s.a^) DO s.a[i] := 0 END
  END InitN;

PROCEDURE ShiftLeftInternalN(VAR s : NSeq; sa : CARDINAL) =
  VAR
    os := s.siz;
  BEGIN
    INC(s.siz,sa);
    ExtendN(s, s.siz);
    SUBARRAY(s.a^,sa,os) := SUBARRAY(s.a^,0,os);
    FOR i := 0 TO sa - 1 DO s.a[i] := 0 END;
  END ShiftLeftInternalN;

PROCEDURE ShiftRightInternalN(VAR s : NSeq; sa : CARDINAL) =
  VAR
    os := s.siz;
  BEGIN
    s.siz := MAX(os - sa, 0);
    IF doDebug THEN Debug.Out("s.siz = " & Int(s.siz)) END;
    SUBARRAY(s.a^, 0, s.siz) := SUBARRAY(s.a^, sa, s.siz);
    FOR i := s.siz TO os - 1 DO s.a[i] := 0 END;
  END ShiftRightInternalN;

PROCEDURE ExtendN(VAR s : NSeq; toDigits : CARDINAL) =
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

PROCEDURE CopyN(READONLY s : NSeq) : NSeq =
  VAR
    na := NEW(NArry, NUMBER(s.a^));
  BEGIN 
    na^ := s.a^;
    RETURN NSeq { siz := s.siz, a := na }
  END CopyN;

PROCEDURE ClearTop(VAR s : NSeq) = 
  BEGIN
    FOR i := LAST(s.a^) TO 0 BY -1 DO
      IF s.a[i] = 0 THEN s.siz := i ELSE EXIT END
    END;
  END ClearTop;

(********************* bignum type *********************)



PROCEDURE Compare(a, b : T) : CompRet =
  BEGIN
    IF a.sign < b.sign THEN RETURN -1
    ELSIF a.sign > b.sign THEN RETURN 1
    END;

    IF    a.rep.siz > b.rep.siz THEN RETURN  a.sign 
    ELSIF a.rep.siz < b.rep.siz THEN RETURN -a.sign
    END;

    FOR i := a.rep.siz - 1 TO 0 BY -1 DO
      VAR
        c := Integer.Compare(a.rep.a[i],b.rep.a[i]);
      BEGIN
        IF c # 0 THEN RETURN a.sign * c END
      END
    END;

    RETURN 0
  END Compare;

PROCEDURE Equal(a, b : T) : BOOLEAN = BEGIN RETURN Compare(a,b) = 0 END Equal;

PROCEDURE IsZero(a : T) : BOOLEAN = BEGIN RETURN Compare(a, Zero) = 0 END IsZero;

PROCEDURE IsOne(a : T) : BOOLEAN = BEGIN RETURN Compare(a, One) = 0 END IsOne;

PROCEDURE Copy(t : T) : T =
  (* Uniq makes no sense here really... *)
  BEGIN RETURN t END Copy;

PROCEDURE New(x : INTEGER) : T =
  VAR
    c : CARDINAL;
    s : NSeq;
    corr : [-1..0] := 0;
    res : T;
  BEGIN
    InitN(s, 1);
    IF doDebug THEN Debug.Out("New x : " & Int(x)) END;
    
    IF x = FIRST(INTEGER) THEN
      (* very special case! can't represent ABS(FIRST(INTEGER)) as a CARDINAL *)
      corr := -1; (* remember correction for later *)
      x := x + 1; (* bring it in range *)
    END;
    
    c := ABS(x);
    s.a[0] := c;
    s.siz := 1;

    Renormalize(s);

    IF x >= 0 THEN
      res := NEW(T, sign := +1, rep := s)
    ELSE
      res := NEW(T, sign := -1, rep := s)
    END;

    IF corr = -1 THEN (* fix up special case of FIRST(INTEGER) *)
      RETURN Uniq(Sub(res, One))
    ELSE
      RETURN Uniq(res)
    END
  END New;

PROCEDURE Sign(a : T) : CompRet =
  BEGIN
    IF a.rep.siz = 0 THEN RETURN 0; END;
    RETURN a.sign;
  END Sign;

PROCEDURE Divide(a, b : T; VAR q, r : T)  RAISES { DivisionByZero } = 
  (* first do a C-style divide *)
  VAR
    sign := a.sign * b.sign;
    aa := Abs(a);
    bb := Abs(b);
  BEGIN
    IF IsZero(b) THEN RAISE DivisionByZero END;
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

PROCEDURE Div(a, b : T) : T  RAISES { DivisionByZero } =
  VAR
    q, r : T;
  BEGIN
    Divide(a,b,q,r);
    RETURN q
  END Div;

PROCEDURE Mod(a, b : T) : T  RAISES { DivisionByZero } =
  VAR
    q, r : T;
  BEGIN
    Divide(a,b,q,r);
    RETURN r
  END Mod;
    
PROCEDURE ShiftLeftInternal(a : T; sa : CARDINAL) : T =
  VAR
    seq := CopyN(a.rep);
  BEGIN
    <*ASSERT Zero = Uniq(Zero)*>
    ShiftLeftInternalN(seq, sa);
    <*ASSERT Zero = Uniq(Zero)*>

    WITH res = Uniq(NEW(T, sign := a.sign, rep := seq)) DO
          <*ASSERT Zero = Uniq(Zero)*>
      RETURN res
    END
  END ShiftLeftInternal;

PROCEDURE DebugSeq(READONLY seq : NSeq) : TEXT =
  VAR
    res := F("{ siz=%s; ", Unsigned(seq.siz));
  BEGIN
    FOR i := NUMBER(seq.a^) - 1 TO 0 BY -1 DO
      res := res & Int(seq.a[i]) & " "
    END;
    RETURN res & "}"
  END DebugSeq;

PROCEDURE DebugT(t : T) : TEXT =
  BEGIN
    RETURN F("[ sgn %s %s]", Int(t.sign), DebugSeq(t.rep))
  END DebugT;
  
PROCEDURE DivideUnsigned(aparm, b : T; VAR q, r : T) =
  VAR
    s : T;
    lo, hi : CARDINAL;
  BEGIN
    IF doDebug THEN Debug.Out(F("DivideUnsigned aparm  = " & DebugT(aparm))) END;
    IF doDebug THEN Debug.Out(F("DivideUnsigned b      = " & DebugT(b))) END;

    <*ASSERT RepOK(aparm)*>
    <*ASSERT RepOK(b)*>

    q := Zero;
    r := aparm;
    <* ASSERT aparm.sign = 1 AND b.sign = 1 *>

    FOR sa := aparm.rep.siz - b.rep.siz TO 0 BY -1 DO
      s := ShiftLeftInternal(b,sa);

      IF doDebug THEN Debug.Out(F("sa = %s ; s = %s", Int(sa), DebugT(s))) END;
      
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

          IF doDebug THEN Debug.Out(F("mid = %s, midM = %s, midC = %s",
                      Int(mid),
                      DebugT(midM),
                      Int(midC))) END;

          (*<* ASSERT Compare(loM,r) <= 0 AND Compare(hiM,r) >= 0 *>*)
          CASE midC OF
            -1 => lo := mid
          |
            0 =>
            q := Add(ShiftLeftInternal(New(mid),sa),q);
            r := Zero;
            <* ASSERT Equal(aparm, Add(r, Mul(q,b))) *>
            RETURN
          |
             1 => hi := mid
          END
        END;
      END;

      WITH nl = New(lo) DO
        WITH z = ShiftLeftInternal(nl, sa) DO
          IF doDebug THEN Debug.Out(F("lo = %s ; sa = %s; nl = %s; z = %s, q = %s",
                      Int(lo), Int(sa), DebugT(nl), DebugT(z), DebugT(q)))END;
          q := Add(z,q);
        END
      END;
      r := Sub(r, Mul(New(lo),s));
      IF doDebug THEN Debug.Out(F("q = %s ; r = %s", DebugT(q), DebugT(r))) END;
    END;
    <*ASSERT RepOK(q)*>
    <*ASSERT RepOK(r)*>

    WITH m  = Mul(q, b),
         aa = Add(r, m) DO
      IF NOT  Equal(aparm, aa) THEN
        Debug.Error("Division failed!",
                    exit := FALSE);
        Debug.Error(F("aparm = %s, b         = %s", DebugT(aparm), DebugT(b)),
                    exit := FALSE);
        Debug.Error(F("q     = %s, r         = %s", DebugT(q), DebugT(r)),
                    exit := FALSE);
        Debug.Error(F("q x b = %s, q x b + 4 = %s", DebugT(m), DebugT(aa)),
                    exit := TRUE);
      END
    END
    
  END DivideUnsigned;

PROCEDURE RepOK(a : T) : BOOLEAN =
  VAR
    ok := TRUE;
  BEGIN
    IF a.rep.siz > NUMBER(a.rep.a^) THEN
      Debug.Error(F("a.rep.siz = %s, NUMBER(a.rep.a^) = %s",
                    Int(a.rep.siz), Int(NUMBER(a.rep.a^))),
                  exit := FALSE);
      ok := FALSE
    END;

    FOR i := FIRST(a.rep.a^) TO LAST(a.rep.a^) DO
      IF a.rep.a[i] >= Base THEN
        Debug.Error(F("a.rep.a[%s] = 16_%s >= Base = 16_%s",
                      Int(i), Unsigned(a.rep.a[i]), Unsigned(Base)),
                    exit := FALSE);
        ok := FALSE
      END
    END;
    IF NUMBER(a.rep.a^) = 0 THEN
      Debug.Error(F("NUMBER(a.rep.a^) = 0"));
      ok := FALSE
    END;
    IF a.rep.siz > 1 AND a.rep.a[a.rep.siz - 1] = 0 THEN
      Debug.Error(F("a.rep.a[a.rep.siz - 1] = %s # 0",
                    Unsigned(a.rep.a[a.rep.siz - 1])),
                  exit := FALSE);
      ok := FALSE
    END;

    IF a.rep.siz = 0 AND a.sign = -1 THEN
      Debug.Error(F("negative zero doesn't work with sign-magnitude!"),
                  exit := FALSE);
      ok := FALSE
    END;
    
    IF NOT ok THEN
      Debug.Error(F("a = %s", DebugT(a)), exit := FALSE)
    END;
    RETURN ok
  END RepOK;

PROCEDURE CheckRep(a : T) : T =
  BEGIN
    <*ASSERT RepOK(a)*>
    RETURN a
  END CheckRep;
  
PROCEDURE Mul(a, b : T) : T =
  BEGIN
    RETURN Uniq(NEW(T, sign := a.sign * b.sign, rep := MulSeqs(a.rep,b.rep)))
  END Mul;

PROCEDURE Pow(b, x : T) : T  RAISES { DivisionByZero } =
  VAR
    r : T;
    result := One;
  BEGIN
    IF IsZero(b) AND IsZero(x) THEN RAISE DivisionByZero END;

    IF IsOne(b) THEN RETURN One END;
    
    IF x.sign = -1 THEN RETURN Zero END;

    <* ASSERT x.sign = 1 *>

    WHILE NOT IsZero(x) DO
      Divide(x, Two, x, r);
      IF NOT IsZero(r) THEN
        result := Mul(result, b)
      END;
      b := Mul(b, b)
    END;
    RETURN result
  END Pow;

PROCEDURE MulSeqs(READONLY a, b : NSeq) : NSeq =
  VAR 
    res : NSeq;
    idx : CARDINAL;
    s   : CARDINAL;
  BEGIN
    InitN(res, a.siz + b.siz + 1 );
    FOR i := 0 TO a.siz - 1 DO
      FOR j := 0 TO b.siz - 1 DO
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

(**********************************************************************)

PROCEDURE Shift(a : T; sa : INTEGER) : T =
  BEGIN
    IF    sa < 0 THEN
      RETURN RightShift(a, -sa)
    ELSIF sa > 0 THEN
      RETURN LeftShift(a, sa)
    ELSE
      RETURN a
    END
  END Shift;

PROCEDURE RightShiftNonneg(a : T; sa : CARDINAL) : T =
  VAR
    s   := CopyN(a.rep);
    saw := MIN(sa DIV BaseLog2, s.siz);
    sab := sa MOD BaseLog2;
  BEGIN
    <*ASSERT a.sign = +1*>
    ShiftRightInternalN(s, saw);

    (* our approach will be to copy the half-words down, then we will
       shift each word, and finally mask *)

    IF doDebug THEN
      Debug.Out(F("RightShift(%s , %s)", DebugT(a), Int(sa)));
      Debug.Out(F("saw = %s , sab = %s", Int(saw), Int(sab)));
    END;
    FOR tgt := 0 TO s.siz - 2 DO
      WITH x = Word.Or(s.a[tgt], Word.LeftShift(s.a[tgt + 1], BaseLog2)),
           y = Word.RightShift(x, sab),
           z = Word.And(y, WordMask) DO
        IF doDebug THEN
          Debug.Out("tgt = " & Int(tgt));
          Debug.Out("x   = " & Unsigned(x));
          Debug.Out("y   = " & Unsigned(y));
          Debug.Out("z   = " & Unsigned(z));
        END;
        s.a[tgt] := z
      END
    END;
    IF s.siz = 0 THEN
      s.a[0] := 0
    ELSE
      WITH z =  Word.RightShift(s.a[s.siz - 1] , sab) DO
        IF doDebug THEN
          Debug.Out("tgt = " & Int(s.siz-1));
          Debug.Out("z   = " & Unsigned(z));
        END;
        s.a[s.siz - 1] := z
      END
    END;

    Renormalize(s);
    WITH pres = NEW(T, sign := a.sign, rep := s),
         res  = Uniq(pres) DO
        <*ASSERT RepOK(pres)*>
        <*ASSERT RepOK(res)*>
      RETURN res
    END
  END RightShiftNonneg;

PROCEDURE RightShift(a : T; sa : CARDINAL) : T =
  BEGIN
    IF    a.sign = -1 THEN
      RETURN Neg(Add(RightShiftNonneg(Sub(Neg(a), One), sa), One))
    ELSIF a.sign = +1 THEN
      RETURN RightShiftNonneg(a, sa)
    ELSE
      <*ASSERT FALSE*>
    END
  END RightShift;

PROCEDURE Odd(a : T) : BOOLEAN =
  BEGIN RETURN Word.Extract(a.rep.a[0], 0, 1) = 1 END Odd;

PROCEDURE Even(a : T) : BOOLEAN =
  BEGIN RETURN Word.Extract(a.rep.a[0], 0, 1) = 0 END Even;

PROCEDURE LeftShift(a : T; sa : CARDINAL) : T =
  VAR
    s   := CopyN(a.rep);

    (* we shift left by an extra word, then shift right! *)

    saw := sa DIV BaseLog2 + 1;
    sab := -(sa MOD BaseLog2 - BaseLog2);
  BEGIN
    ShiftLeftInternalN(s, saw);

    (* our approach will be to copy the half-words down, then we will
       shift each word, and finally mask *)

    FOR tgt := 0 TO s.siz - 2 DO
      WITH x = Word.Or(s.a[tgt], Word.LeftShift(s.a[tgt + 1], BaseLog2)),
           y = Word.RightShift(x, sab),
           z = Word.And(y, WordMask) DO
        s.a[tgt] := z
      END
    END;
    s.a[s.siz - 1] := Word.RightShift(s.a[s.siz - 1] , sab);
    Renormalize(s);
    WITH pres = NEW(T, sign := a.sign, rep := s),
         res  = Uniq(pres) DO
      <*ASSERT RepOK(pres)*>
      <*ASSERT RepOK(res)*>
      RETURN res
    END
  END LeftShift;


PROCEDURE Add(a, b : T) : T =
  VAR
    res : T;
  BEGIN
    IF doDebug THEN Debug.Out(F("Add( a = %s , b = %s )", DebugT(a), DebugT(b))) END;
    
   IF a.sign = 1 AND b.sign = 1 THEN
      res := NEW(T, sign := 1, rep := AddSeqs(a.rep,b.rep));
    ELSIF a.sign = -1 AND b.sign = -1 THEN
      res := NEW(T, sign := -1, rep := AddSeqs(a.rep,b.rep));
    ELSIF a.sign = -1 AND b.sign = 1 THEN
      res := Sub(b,Neg(a));
    ELSIF a.sign = 1 AND b.sign = -1 THEN
      res := Sub(a,Neg(b));
    ELSE
      <* ASSERT FALSE *>
    END;

    Renormalize(res.rep);

    IF doDebug THEN Debug.Out(F("Add( a = %s , b = %s ) ->  %s", DebugT(a), DebugT(b), DebugT(res))) END;
    RETURN Uniq(res)
  END Add;

PROCEDURE Sub(a, b : T) : T =
  VAR
    res : T;
  BEGIN
    IF    a.sign = 1 AND b.sign = 1 AND Compare(a,b) > -1 THEN
      res := NEW(T, sign := 1, rep := SubSeqs(a.rep,b.rep))
    ELSIF a.sign = 1 AND b.sign = 1 AND Compare(a,b) = -1 THEN
      res := NEW(T, sign := -1, rep := SubSeqs(b.rep,a.rep))
    ELSIF a.sign = -1 AND b.sign = -1 THEN
      res := Neg(Sub(Neg(a),Neg(b)))
    ELSIF a.sign = -1 AND b.sign = 1 THEN
      res := Neg(Add(Neg(a),b))
    ELSIF a.sign = 1 AND b.sign = -1 THEN
      res := Add(a,Neg(b))
    ELSE
      <* ASSERT FALSE *>
    END;
    
    Renormalize(res.rep);
    RETURN Uniq(res)
  END Sub;

(* unsigned addition of underlying sequences *)
PROCEDURE AddSeqs(READONLY s, t : NSeq) : NSeq =
  VAR
    m := MAX(s.siz, t.siz);
    r : NSeq;
  BEGIN
    InitN(r, m + 1);
    SUBARRAY(r.a^, 0, s.siz) := SUBARRAY(s.a^, 0, s.siz);
    FOR i := 0 TO MIN(m - 1, t.siz - 1) DO
      INC(r.a[i], t.a[i]);
    END;
    RETURN r
  END AddSeqs;

(* unsigned subtraction of underlying sequences *)
(* s must be .ge. t *)
PROCEDURE SubSeqs(READONLY s, t : NSeq) : NSeq =
  VAR
    m := MAX(s.siz, t.siz);
    r : NSeq;
    borrow := 0;
  BEGIN
    <*ASSERT s.siz >= t.siz*>

    InitN(r, m + 1);
    FOR i := 0 TO m - 1 DO
      VAR
        diff := s.a[i] + borrow;
      BEGIN
        IF i < t.siz THEN DEC(diff, t.a[i]); END;
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
  BEGIN RETURN Uniq(NEW(T, sign := 1, rep := a.rep)) END Abs;

PROCEDURE Renormalize(VAR a : NSeq) = 
  VAR
    carry := 0;
    o : CARDINAL;
    i := 0;
  BEGIN
    ClearTop(a);

    WHILE i < a.siz OR carry # 0 DO
      
      IF i > LAST(a.a^) THEN ExtendN(a, NUMBER(a.a^) + 4) END;

      <*ASSERT i <= LAST(a.a^)*>
      
      o := a.a[i] + carry;
      a.a[i] := o MOD Base;

      IF i >= a.siz AND a.a[i] # 0 THEN a.siz := i + 1 END;

      carry := o DIV Base;
      INC(i)
    END;
  END Renormalize;

PROCEDURE Neg(a : T) : T = 
  BEGIN RETURN Uniq(NEW(T, rep := a.rep, sign := -a.sign)) END Neg;

PROCEDURE ScanBased(txt : TEXT; defaultBase : PrintBase) : T
  RAISES { Lex.Error, FloatMode.Trap } =
  VAR
    neg : BOOLEAN;
  BEGIN
    <*ASSERT Zero=Uniq(Zero)*>
    IF Text.GetChar(txt, 0) = '-' THEN
      neg := TRUE;
      txt := Text.Sub(txt, 1, Text.Length(txt) - 1)
    ELSE
      neg := FALSE
    END;

    (* "" and "-" are not legal numbers *)
    IF Text.Length(txt) = 0 THEN
      RAISE Lex.Error
    END;

    <*ASSERT Zero=Uniq(Zero)*>
    
    WITH usIndex = Text.FindChar(txt, '_') DO
      IF usIndex = -1 THEN
        RETURN Scan(txt, defaultBase, neg)
      ELSE
        WITH baseTxt = Text.Sub(txt, 0, usIndex),
             base    = M3Scan.Int(baseTxt),
             mantTxt = Text.Sub(txt, usIndex + 1) DO
          RETURN Scan(mantTxt, base, neg)
        END
      END
    END
  END ScanBased;
  
PROCEDURE Scan(txt : TEXT; base : PrintBase; neg : BOOLEAN) : T
  RAISES { Lex.Error } =
  VAR
    accum := Zero;
    baseT := small[base];
  BEGIN
    IF Text.GetChar(txt, 0) = '-' THEN
      neg := NOT neg;
      txt := Text.Sub(txt, 1, Text.Length(txt) - 1)
    END;

    <*ASSERT Zero=Uniq(Zero)*>

    (* "" and "-" are not legal numbers *)
    IF Text.Length(txt) = 0 THEN
      RAISE Lex.Error
    END;
     
    FOR i := 0 TO Text.Length(txt) - 1 DO
      WITH c = Text.GetChar(txt, i) DO
          WITH val = CharVal[c] DO
            IF val < 0 OR val > base - 1 THEN
              RAISE Lex.Error
            END;
            accum := Add(Mul(baseT, accum), CharValT[c])
          END
      END
    END;

    <*ASSERT Zero=Uniq(Zero)*>

    IF neg THEN
      RETURN Neg(accum)
    ELSE
      RETURN accum
    END
  END Scan;

PROCEDURE ScanDelimited(txt : TEXT; base : PrintBase) : T
  RAISES { Lex.Error } =
  VAR
    accum := Zero;
    baseT := small[base];
  BEGIN
    <* ASSERT base <= NUMBER(HexChars) *>
    FOR i := 0 TO Text.Length(txt) - 1 DO
      WITH c = Text.GetChar(txt, i) DO
        IF c IN DelimChars THEN
          (* skip *)
        ELSE
          WITH val = CharVal[c] DO
            IF val < 0 OR val > base - 1 THEN
              RAISE Lex.Error
            END;
            accum := Add(Mul(baseT, accum), CharValT[c])
          END
        END
      END
    END;
    RETURN accum
  END ScanDelimited;

PROCEDURE FormatLiteral(a : T; base : PrintBase) : TEXT =
  BEGIN RETURN FormatInternal(a, base, TRUE) END FormatLiteral;
  
PROCEDURE Format(a : T; base : PrintBase) : TEXT =
  BEGIN RETURN FormatInternal(a, base, FALSE) END Format;
  
PROCEDURE FormatInternal(a : T; base : PrintBase; literal : BOOLEAN) : TEXT =
  VAR
    c := NEW(CharSeq.T).init();
    s := Sign(a);
    wx := Wx.New();
    MyExtract := extractbase[base];
  BEGIN
    a := Abs(a);

    WHILE NOT Equal(a,Zero) DO
      VAR
        d : T;
      BEGIN
        
        DivideUnsigned(a, MyExtract, a, d);

        <* ASSERT Compare(d,Zero) >= 0 AND Compare(d,MyExtract) < 1 *>
        
        VAR
          toPrint := d.rep.a[0];
        BEGIN
          FOR i := 0 TO chunkdigits[base] - 1 DO
            IF toPrint = 0 AND a = Zero THEN
              EXIT
            END;
            c.addlo(HexChars[toPrint MOD base]);
            toPrint := toPrint DIV base;
          END
        END
      END
    END;

    IF c.size() = 0 THEN
      CASE s OF
        0, 1 => RETURN "0"
      |
        -1   => RETURN "-0"
      END
    END;
      
    IF s = -1 THEN
      Wx.PutChar(wx, '-')
    END;
    
    IF literal THEN
      Wx.PutText(wx, Int(base));
      Wx.PutChar(wx, '_')
    END;
    
    FOR i := 0 TO c.size() - 1 DO
      Wx.PutChar(wx, c.get(i))
    END;

    RETURN Wx.ToText(wx)
  END FormatInternal;

PROCEDURE FormatOld(a : T; base : PrintBase) : TEXT =
  VAR
    c := NEW(CharSeq.T).init();
    s := Sign(a);
    wx := Wx.New();
    MyBase := small[base];
  BEGIN

    WHILE NOT Equal(a,Zero) DO
      VAR
        d : T;
      BEGIN
        DivideUnsigned(a, MyBase, a, d);
        <* ASSERT Compare(d,Zero) >= 0 AND Compare(d,MyBase) < 1 *>
        c.addlo(HexChars[d.rep.a[0]]);
      END
    END;

    IF c.size() = 0 THEN
      CASE s OF
        0, 1 => RETURN "0"
      |
        -1   => RETURN "-0"
      END
    END;
      
    IF s = -1 THEN c.addlo('-') END;
    
    FOR i := 0 TO c.size() - 1 DO
      Wx.PutChar(wx, c.get(i))
    END;

    RETURN Wx.ToText(wx)
  END FormatOld;

PROCEDURE Hash(a : T) : Word.T = 
  VAR
    res : Word.T := 0;
  BEGIN
    FOR i := 0 TO a.rep.siz - 1 DO
      res := Word.Plus(res, a.rep.a[i])
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
      RETURN FLOAT(a.sign, LONGREAL) * res;
    END;
  END ToLongReal;

PROCEDURE ToInteger(a : T) : INTEGER RAISES { OutOfRange } =
  BEGIN
    <*ASSERT a # NIL*>
    IF Compare(a, FirstInt) = -1 OR
      Compare(a, LastInt) = +1 THEN
      RAISE OutOfRange
    END;
    IF a.rep.siz = 0 THEN RETURN 0; END;
    VAR
      res := a.rep.a[a.rep.siz-1];
    BEGIN
      FOR i := a.rep.siz - 2 TO 0 BY -1 DO
        res := res * Base;
        res := res + a.rep.a[i]
      END;
      RETURN a.sign * res;
    END;
  END ToInteger;

PROCEDURE GetRepBase() : T =
  BEGIN
    RETURN RepBase
  END GetRepBase;

PROCEDURE GetBit(t : T; bit : CARDINAL) : [ 0 .. 1 ] =
  VAR
    unsignedBit : [ 0 .. 1 ];
  BEGIN
    WITH word = bit DIV BaseLog2,
         pos  = bit MOD BaseLog2 DO
      IF word > LAST(t.rep.a^) THEN
        unsignedBit := 0
      ELSE
        unsignedBit := Word.Extract(t.rep.a[word], pos, 1)
      END;

      IF t.sign = 1 THEN
        RETURN unsignedBit
      END;
      
      <*ASSERT t.sign = -1*>

      IF word > LAST(t.rep.a^) THEN
        RETURN 1 (* leading 1 *)
      END;
      
      VAR
        carry : [ 0 .. 1 ] := 1;
      BEGIN
        FOR i := 0 TO word - 1 DO
          IF t.rep.a[i] # 0 THEN
            carry := 0;
            EXIT
          END
        END;
        
        IF Word.Extract(t.rep.a[word], 0, pos) # 0 THEN
          carry := 0
        END;
        
        RETURN Word.And(1 - unsignedBit + carry, 1)
        
      END
    END      
  END GetBit;

(**********************************************************************)

TYPE BitProc = PROCEDURE(x, y : Word.T) : Word.T;

PROCEDURE GetTheBits(a : T; VAR res : ARRAY OF Word.T) =
  (* assume res is at least big enough to hold a and a guard bit,
     the sign bit  *)
  CONST
    ws = BITSIZE(Word.T); (* machine word size *)
    rs = BaseLog2;        (* representation size *)
  VAR
    bi := 0; (* bit index *)
    wi := 0; (* word index *)
  BEGIN
    (* clear res *)
    FOR i := FIRST(res) TO LAST(res) DO
      res[i] := 0
    END;
    
    (* start out by putting the magnitude *)
    FOR i := 0 TO a.rep.siz - 1 DO
      WITH space     = ws - bi,
           thisChunk = MIN(rs, space),
           remaining = rs - thisChunk DO
        res[wi] := Word.Or(res[wi], Word.Shift(a.rep.a[i], bi));
        INC(bi, thisChunk);
        IF bi = ws THEN
          bi := 0;
          INC(wi);
          IF remaining # 0 THEN
            res[wi] := Word.Or(res[wi], Word.Shift(a.rep.a[i], -thisChunk));
            INC(bi, remaining)
          END
        END
      END
    END;

    IF doDebug THEN
      Debug.Out("GetTheBits : before sign adj res = " & FmtWordArr(res))
    END;

    IF a.sign = -1 THEN
      FOR i := FIRST(res) TO LAST(res) DO
        res[i] := Word.Not(res[i])
      END;
      FOR i := FIRST(res) TO LAST(res) DO
        res[i] := Word.Plus(res[i], 1);
        
        IF res[i] # 0 THEN EXIT END (* carry chain stopped *)
      END
    END;

    IF doDebug THEN
      Debug.Out("GetTheBits : after  sign adj res = " & FmtWordArr(res))
    END
  END GetTheBits;

PROCEDURE StuffTheBits((*MODIFIES*)VAR wa : ARRAY OF Word.T) : T =
  CONST
    ws = BITSIZE(Word.T);
    rs = BaseLog2;     
  VAR
    bits := NUMBER(wa) * ws;
    siz  := bits DIV rs; (* note that we can't necessarily represent all of wa,
                            but that should be OK *)
    sgn  := +1;
  BEGIN
    IF doDebug THEN
      Debug.Out(F("StuffTheBits : NUMBER(wa) = %s, siz = %s",
                  Int(NUMBER(wa)), Int(siz)));
      Debug.Out("StuffTheBits : before sign adj wa = " & FmtWordArr(wa))
    END;
    
    (* start by computing the sign-magnitude form *)
    IF Word.Extract(wa[LAST(wa)], ws - 1, 1) = 1 THEN
      sgn := -1;
      FOR i := FIRST(wa) TO LAST(wa) DO
        wa[i] := wa[i] - 1;
        IF (wa[i]) # -1 THEN EXIT END (*hmm, ugly!*)
      END;
      FOR i := FIRST(wa) TO LAST(wa) DO
        wa[i] := Word.Not(wa[i])
      END
    END;

    IF doDebug THEN
      Debug.Out("StuffTheBits : after  sign adj wa = " & FmtWordArr(wa))
    END;

    VAR
      seq := NSeq { siz := siz, a := NEW(NArry, siz) };
      bi := 0; (* bit index *)
      wi := 0; (* word index *)
    BEGIN

      FOR i := 0 TO seq.siz - 1 DO
        WITH left      = ws - bi,
             thisChunk = MIN(rs, left),
             remaining = rs - thisChunk DO
          IF doDebug THEN
            Debug.Out(F("i = %s, wi = %s, left = %s, thisChunk = %s, remaining = %s",
                        Int(i), Int(wi), Int(left), Int(thisChunk), Int(remaining)));
            Debug.Out(F("NUMBER(seq.a^) = %s ; NUMBER(wa) = %s; -bi = %s",
                        Int(NUMBER(seq.a^)), Int(NUMBER(wa)), Int(-bi)))
          END;
          seq.a[i] := Word.Shift(wa[wi], -bi); 
          INC(bi, thisChunk);

          IF doDebug THEN
            Debug.Out(F("wi=%s bi=%s thisChunk=%s seq.a[%s] = %s",
                        Int(wi),
                        Int(bi),
                        Unsigned(thisChunk),
                        Int(i),
                        Unsigned(seq.a[i])))
          END;
            
          IF bi = ws THEN
            bi := 0;
            INC(wi);
            IF remaining # 0 THEN
              seq.a[i] := Word.Or(seq.a[i], Word.Shift(wa[wi], thisChunk));
              INC(bi, remaining)
            END
          END
        END;
        seq.a[i] := Word.And(seq.a[i], WordMask)
      END;
      Renormalize(seq);
      RETURN Uniq(CheckRep(NEW(T, sign := sgn, rep := seq)))
    END
  END StuffTheBits;

PROCEDURE FmtWordArr(READONLY a : ARRAY OF Word.T) : TEXT =
  VAR
    res := "{ ";
  BEGIN
    FOR i := LAST(a) TO FIRST(a) BY -1 DO
      res := res & "16_" & Unsigned(a[i]) & " "
    END;
    RETURN res & "}"
  END FmtWordArr;
  
PROCEDURE DoBitwiseOp(op : BitProc; a, b : T) : T =
  (* the way we do it is we fill an array with all the bits, plus
     guard bits.  We perform the operations on the arrays, using the
     Word ops, then build a new number with the correct layout *)

  VAR
    repBits  := MAX(GetAbsMsb(a), GetAbsMsb(b)) + 3;
    (* do we need two extra? *)

    repWords := (repBits - 1) DIV BITSIZE(Word.T) + 2;
    (* we need an extra word for not.. for some reason!!! *)
    
    aa, ba, ca := NEW(REF ARRAY OF Word.T, repWords);
  BEGIN
    GetTheBits(a, aa^);
    GetTheBits(b, ba^);

    FOR i := 0 TO repWords - 1 DO
      ca[i] := op(aa[i], ba[i])
    END;

    RETURN StuffTheBits(ca^)
  END DoBitwiseOp;

PROCEDURE WordNotFirst(a : Word.T; <*UNUSED*> b : Word.T) : Word.T =
  BEGIN
    RETURN Word.Not(a)
  END WordNotFirst;

PROCEDURE Or(a, b : T) : T =
  BEGIN
    RETURN DoBitwiseOp(Word.Or, a, b)
  END Or;
  
PROCEDURE And(a, b : T) : T =
  BEGIN
    RETURN DoBitwiseOp(Word.And, a, b)
  END And;
  
PROCEDURE Xor(a, b : T) : T =
  BEGIN
    RETURN DoBitwiseOp(Word.Xor, a, b)
  END Xor;
  
PROCEDURE Not(a : T) : T =
  BEGIN
    RETURN DoBitwiseOp(WordNotFirst, a, Zero)
  END Not;

PROCEDURE IsT(ref : REFANY) : BOOLEAN =
  BEGIN RETURN ISTYPE(ref, T) END IsT;

VAR
  mu   := NEW(MUTEX);
  uniq := TRUE;
  tbl  := NEW(BigIntBigIntTbl.Default).init();
  
PROCEDURE UniqReferences(to : BOOLEAN) : BOOLEAN =
  VAR
    old : BOOLEAN;
  BEGIN
    LOCK mu DO
      old := uniq;
      uniq := to;
      RETURN old
    END
  END UniqReferences;

PROCEDURE Uniq(t : T) : T =
  BEGIN
    IF NOT uniq THEN
      RETURN t
    END;
    <*ASSERT t.sign # 0*>

    IF t.rep.siz = 0 THEN
      <*ASSERT t.rep.a[0] = 0*>
      RETURN Zero
    ELSIF t.rep.siz = 1 AND t.rep.a[0] = 0 THEN
      RETURN Zero
    END;
      

    LOCK mu DO
      VAR
        q : T;
      BEGIN
        IF tbl.get(t, q) THEN
          RETURN q
        ELSE
          EVAL tbl.put(t, t);
          RETURN t
        END
      END
    END
  END Uniq;


CONST WordSize = BITSIZE(Word.T);

TYPE  BitPos = [ 0 .. WordSize - 1 ];

CONST Mask_1  : Word.T = 1;
      Mask_2  : Word.T = Word.Or(Word.Shift(Mask_1, 1), Mask_1);
      Mask_4  : Word.T = Word.Or(Word.Shift(Mask_2, 2), Mask_2);
      Mask_8  : Word.T = Word.Or(Word.Shift(Mask_4, 4), Mask_4);
      Mask16  : Word.T = Word.Or(Word.Shift(Mask_8, 8), Mask_8);
      Mask32  : Word.T = Word.Or(Word.Shift(Mask16,16), Mask16);

      Mask_1H : Word.T = Word.Shift(Mask_1, 1);
      Mask_2H : Word.T = Word.Shift(Mask_2, 2);
      Mask_4H : Word.T = Word.Shift(Mask_4, 4);
      Mask_8H : Word.T = Word.Shift(Mask_8, 8);
      Mask16H : Word.T = Word.Shift(Mask16,16);
      Mask32H : Word.T = Word.Shift(Mask32,32);
      
PROCEDURE FindMsb(w : Word.T) : [ -1..LAST(BitPos) ] =
  VAR
    pos : BitPos := 0;
  BEGIN
    IF w = 0 THEN RETURN -1 END;
    
    IF WordSize = 64 THEN
      IF Word.And(Mask32H, w) # 0 THEN
        INC(pos, 32);
        w := Word.RightShift(w, 32)
      END
    END;
    
    IF Word.And(Mask16H, w) # 0 THEN
      INC(pos, 16);
      w := Word.RightShift(w, 16)
    END;
    IF Word.And(Mask_8H, w) # 0 THEN
      INC(pos,  8);
      w := Word.RightShift(w,  8)
    END;
    IF Word.And(Mask_4H, w) # 0 THEN
      INC(pos,  4);
      w := Word.RightShift(w,  4)
    END;
    IF Word.And(Mask_2H, w) # 0 THEN
      INC(pos,  2);
      w := Word.RightShift(w,  2)
    END;
    IF Word.And(Mask_1H, w) # 0 THEN
      INC(pos,  1);
    END;
    RETURN pos
  END FindMsb;

PROCEDURE GetAbsMsb(t : T) : [ -1 .. LAST(CARDINAL) ] =
  BEGIN
    IF t = Zero THEN
      RETURN -1
    ELSE
      WITH szm1 = t.rep.siz - 1 DO
        RETURN  szm1 * BaseLog2 + FindMsb(t.rep.a[szm1])
      END
    END
  END GetAbsMsb;

PROCEDURE GetMsb(t : T) : [-1..LAST(CARDINAL)] =
  VAR
    sgn := Sign(t);
  BEGIN
    IF    sgn = 1 THEN
      RETURN GetAbsMsb(t)
    ELSIF sgn = -1 THEN
      WITH absMsb = GetAbsMsb(t) DO
        IF GetBit(t, absMsb) = 0 THEN
          RETURN absMsb
        ELSE
          <*ASSERT absMsb = 0 (* t = -1 *) OR GetBit(t, absMsb - 1) = 0*>
          RETURN absMsb - 1
        END
      END
    ELSE
      RETURN -1
    END
  END GetMsb;
  
VAR
  chunkdigits : ARRAY PrintBase OF CARDINAL;
  extractbase : ARRAY PrintBase OF T;

PROCEDURE InitializeFormatHelp() =
  BEGIN
    FOR pb := FIRST(PrintBase) TO LAST(PrintBase) DO
      VAR
        a := 1;
        p := pb;
        op : CARDINAL;
      BEGIN
        REPEAT
          op := p;
          p := p * pb;
          INC(a)
        UNTIL p > Base;
        DEC(a);
        IF doDebug THEN Debug.Out(F("InitializeFormatHelp : printbase %s : p = %s  a = %s",
                    Int(pb), Int(op), Int(a))) END;

        chunkdigits[pb] := a;
        extractbase[pb] := New(op)
      END
    END
  END InitializeFormatHelp;

PROCEDURE GetInitialized() : BOOLEAN =
  BEGIN RETURN initialized END GetInitialized;
  
VAR
  FirstInt, LastInt : T;
  CharVal : ARRAY CHAR OF INTEGER;
  CharValT : ARRAY CHAR OF T;
  small : ARRAY PrintBase OF T;

  RepBase : T;

  Zero := NEW(T, sign := 1, rep := NSeq { 0, NEW(NArry, 1) });
  One  := NEW(T, sign := 1, rep := NSeq { 1, NEW(NArry, 1) });
  Two  := NEW(T, sign := 1, rep := NSeq { 1, NEW(NArry, 1) });

      

  initialized : BOOLEAN;

BEGIN
  <*ASSERT WordSize = 32 OR WordSize = 64*>

  One.rep.a[0] := 1;
  Two.rep.a[0] := 2;

  EVAL Uniq(Zero);
  <*ASSERT Zero = Uniq(Zero)*>
  EVAL Uniq(One);
  <*ASSERT One = Uniq(One)*>
  EVAL Uniq(Two);
  <*ASSERT Two = Uniq(Two)*>

  RepBase  := New(Base);
  FirstInt := New(FIRST(INTEGER));
  LastInt  := New(LAST(INTEGER));
  <*ASSERT Zero = Uniq(Zero)*>

  FOR c := FIRST(CHAR) TO LAST(CHAR) DO
    VAR
      val : INTEGER;
    BEGIN
      CASE c OF
        '0'..'9' => val := ORD(c) - ORD('0')
      |
        'A'..'Z' => val := ORD(c) - ORD('A') + 10
      |
        'a'..'z' => val := ORD(c) - ORD('a') + 10
      ELSE
        val := -1
      END;
      CharVal[c] := val;
      IF val = -1 THEN
        CharValT[c] := NIL
      ELSE
        CharValT[c] := New(val)
      END
    END
  END;
  <*ASSERT Zero = Uniq(Zero)*>

  FOR i := FIRST(PrintBase) TO LAST(PrintBase) DO
    small[i] := New(i)
  END;

  InitializeFormatHelp();
  <*ASSERT Zero = Uniq(Zero)*>

  initialized := TRUE;
END BigInt.
