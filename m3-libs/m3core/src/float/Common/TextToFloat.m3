(* Copyright (C) 2016 Peter McKinna. All rights reserved. *)
(* See file COPYRIGHT-BSD for details. *)

UNSAFE MODULE TextToFloat;

(* Conversion of a string to REAL, LONGREAL or quad precision floating point.
   (representing 32, 64 and 128 bit floats)
   Based on David Gay's strtodg.c 
   https://github.com/jwiegley/gdtoa
   
   which are generalisations of the routines described in
   
   David M. Gay, "Correctly Rounded Binary-Decimal and
   Decimal-Binary Conversions", Numerical Analysis Manuscript
   No. 90-10, Bell Labs, Murray Hill, 1990;
   http://cm.bell-labs.com/cm/cs/what/ampl/REFS/rounding.ps.gz   
   
*)

IMPORT Word,Text;
IMPORT DragonInt AS DI;
IMPORT FloatUtils AS FU;
IMPORT RealRep,LongRealRep;

CONST
  MaxString = 100;
  P = 53; (* from FloatUtils *)
  ULbits = 32;
  BigTens = 5;
  KMask = 31;
  DBL_DIG = 15;
  ExpShift = 20;
  KShift = 5;
  ExpMask = 16_7FF00000;
  AllOn = 16_FFFFFFFF;
  FivesBits = ARRAY [0..22] OF INTEGER{0,  3,  5,  7, 10, 12, 14, 17, 19, 21, 24, 26, 28, 31, 33, 35, 38, 40, 42, 45, 47, 49, 52};
  
TYPE
  CharMap = ARRAY CHAR OF BOOLEAN;
  StringBufferIndex =  [0..MaxString];
  StringBuffer =  ARRAY StringBufferIndex OF CHAR;
  TensArr = ARRAY[0..BigTens-1] OF LONGREAL;
  FPI = OBJECT
    emin,emax,nbits : INTEGER;
    rounding : RoundingModes;
    suddenUnderflow : BOOLEAN;
  END;
  
VAR (* CONST *)
  Digits : CharMap := CharMap { FALSE, .. };
  bigtens := TensArr{ 1.0D16, 1.0D32, 1.0D64, 1.0D128, 1.0D256 };
  tinytens := TensArr{ 1.0D-16, 1.0D-32, 1.0D-64, 1.0D-128, 1.0D-256};  
  tens := ARRAY[0..22] OF LONGREAL{1.0D00, 1.0D1, 1.0D2, 1.0D3, 1.0D4, 1.0D5, 1.0D6, 1.0D7, 1.0D8, 1.0D9,1.0D10, 1.0D11, 1.0D12, 1.0D13, 1.0D14, 1.0D15, 1.0D16, 1.0D17, 1.0D18, 1.0D19,1.0D20, 1.0D21, 1.0D22};

EXCEPTION exitLoop;
EXCEPTION adjJump;
EXCEPTION ret;
EXCEPTION huge;
EXCEPTION illegalLit;
  
PROCEDURE ScanNumber (str : TEXT; 
                      VAR buf : StringBuffer; 
                      VAR y,z : Word.T; 
                      VAR nd,nd0,nf,exp : INTEGER; 
                      VAR sign : BOOLEAN) RAISES {illegalLit} =
  CONST
    ORD0 = ORD('0');
    EOFChar = '@';
  VAR
    esign : BOOLEAN;
    ed,nz,len0,inputPtr : INTEGER := 0;
    bufLen : StringBufferIndex; 
    ch : CHAR;

  PROCEDURE GetCh () =
    VAR inputLen : INTEGER;
    BEGIN
      inputLen := Text.Length(str);
      IF inputLen <= inputPtr THEN
        ch := EOFChar;
      ELSE
       ch := Text.GetChar(str,inputPtr);
       INC(inputPtr);
      END;
    END GetCh;
  
  BEGIN
    y := 0; z := 0; nd := 0; nf := 0; nz := 0;  

    inputPtr := 0;
    GetCh();

    IF ch = '-' THEN sign := TRUE; GetCh(); END;
    
    (* trim leading zeros *)
    WHILE ch = '0' DO GetCh(); END;
    
    (* scan the decimal digits *)
    bufLen := 0;
    WHILE Digits[ch] DO
      IF nd < 9 THEN
        y := 10 * y + ORD(ch) - ORD0;
      ELSIF nd < 16 THEN
        z := 10 * z + ORD(ch) - ORD0;
      END;
      INC(nd);    
      buf[bufLen] := ch;  INC (bufLen);
      GetCh ();
    END;

    nd0 := nd;
    
    IF (ch = '.') THEN
      IF nd > 0 THEN
        buf[bufLen] := '.';  INC (bufLen);
      END;
      GetCh (); (* eat the '.' *)

      IF NOT Digits[ch] THEN
        RAISE illegalLit;
      END;
      
      (* scan the fractional digits *)
      IF nd = 0 THEN
        WHILE ch = '0' DO GetCh(); INC(nz); END;
        IF Digits[ch] THEN
          INC(nf,nz);
          nz := 0;
        END;
      END;

      WHILE Digits[ch] DO
        INC(nz);
        (* trim trailing zeros *)
        IF ch # '0' THEN
          INC(nf,nz);
          FOR i := 1 TO nz - 1 DO
            IF nd < 9 THEN 
              y := y * 10;
            ELSIF nd < DBL_DIG + 1 THEN
              z := z * 10;
            END;
            INC(nd);
          END;      
            
          IF nd < 9 THEN
            y := 10 * y + ORD(ch) - ORD0;
          ELSIF nd < DBL_DIG + 1 THEN
            z := 10 * z + ORD(ch) - ORD0;
          END;
          INC(nd);
          nz := 0;
        END; (*if *)
        buf[bufLen] := ch; INC (bufLen); 
        GetCh ();        
      END; (* while *)
      
      (* check for the exponent *)
      IF (ch = 'x') OR (ch = 'X') OR (ch = 'd') OR (ch = 'D') OR (ch = 'e') OR (ch = 'E') THEN
        buf[bufLen] := 'e';  INC (bufLen);
      ELSE (* real constant with no exponent *)
        RAISE illegalLit;
      END;
      GetCh (); (* eat the exponent entry char *)

      esign := FALSE;      
      (* get the exponent sign *)
      IF (ch = '+') THEN
        buf[bufLen] := '+';  INC (bufLen);
        GetCh ();
      ELSIF (ch = '-') THEN
        buf[bufLen] := '-';  INC (bufLen); esign := TRUE;
        GetCh ();
      ELSE
        buf[bufLen] := '+'; INC(bufLen);
      END;

      IF NOT Digits[ch] THEN
        RAISE illegalLit;
      END;

      (* finally, get the exponent digits *)
      ed := 0;
      len0 := bufLen;
      WHILE Digits[ch] DO  
        ed := 10 * ed + ORD(ch) - ORD0;
        buf[bufLen] := ch; INC (bufLen); 
        GetCh ();  
      END;
      IF (bufLen - len0 > 8) OR ed > 19999 THEN
          (** Avoid confusion from exponents
            * so large that e might overflow.*)
        exp := 19999;
      ELSE
        exp := ed;
        IF esign THEN exp := -exp; END;
      END;
    ELSE
      RAISE illegalLit;
    END;
  END ScanNumber;

PROCEDURE RvOK(sess: DI.Session; d : LONGREAL; fpi : FPI; VAR exp : INTEGER; VAR bits : DI.RefInt32; exact : BOOLEAN; roundMode : INTEGER; VAR irv : INTEGER) : BOOLEAN =
  VAR
    b : DI.T;
    inex, lostbits : Word.T := 0;
    bdif, e, j, k, k1,rv, len : INTEGER := 0;
    arr : DI.RefInt32;
    
  PROCEDURE Trunc(carry : Word.T) =
    VAR any : BOOLEAN;
    BEGIN
      inex := 0; lostbits := 0;
      IF bdif > 0 THEN
        any := FU.anyOn(sess, b, bdif);      
        lostbits := ORD(any);
        IF lostbits # 0 THEN
          inex := STRTOG_Inexlo;
        END;
        b := DI.shift(sess, b, -bdif);
        IF carry # 0 THEN
          inex := STRTOG_Inexhi;
          b := DI.add(sess, b, DI.One);
          j := Word.And(fpi.nbits, KMask);
          IF j # 0 THEN
            j := ULbits - j;
          END;  
          DI.ToArr32(sess,b,arr,len);
          IF FU.hi0bits(arr[len - 1]) # j THEN
            IF lostbits = 0 THEN
              lostbits := Word.And(arr[0], 1);
            END;
            b := DI.shift(sess,b,-1);
            INC(e);
          END;
        END;
      ELSIF bdif < 0 THEN
        b := DI.shift(sess,b, -bdif);
      END;
      
      IF e < fpi.emin THEN
        k := fpi.emin - e;
        e := fpi.emin;
        IF k > fpi.nbits OR fpi.suddenUnderflow THEN
          inex := 0;
          b := DI.Zero;
          irv := Word.Or(STRTOG_Underflow, STRTOG_Inexlo);
        ELSE
          k1 := k - 1;
          IF k1 > 0 AND lostbits = 0 THEN
            any := FU.anyOn(sess, b, k1);
            lostbits := ORD(any);
            IF lostbits = 0 AND exact = FALSE THEN
              RETURN;
            ELSE
              DI.ToArr32(sess,b,arr,len);
              carry := Word.And(arr[Word.RightShift(k1,KShift)],
                                Word.LeftShift(1,Word.And(k1, KMask)));
              lostbits := Word.Or(lostbits,carry);
              b := DI.shift(sess,b,-k);
              irv := STRTOG_Denormal;
              IF carry # 0 THEN
                b := DI.add(sess, b, DI.One);
                inex := Word.Or(STRTOG_Inexhi, STRTOG_Underflow);
              ELSIF lostbits # 0 THEN
                inex := Word.Or(STRTOG_Inexlo, STRTOG_Underflow);
              END;
            END;
          END;
        END;
      ELSIF e > fpi.emax THEN
        e := fpi.emax + 1;
        irv := Word.Or(STRTOG_Infinite, Word.Or(STRTOG_Overflow, STRTOG_Inexhi));
        inex := 0;
        b := DI.Zero;
      END;
      exp := e;
      
      DI.ToArr32(sess,b,bits,len);
      irv := Word.Or(irv, inex);
      rv := 1;
    END Trunc;
    
  BEGIN
    b := FU.d2b(sess,d,e,bdif);

    DEC(bdif, fpi.nbits);
    INC(e, bdif);
    IF bdif <= 0 THEN
      IF exact THEN
        Trunc(0);
      END;
    ELSIF fpi.nbits = P THEN
      IF fpi.rounding = RoundingModes.RoundNear THEN
        Trunc(0);
      END;
    ELSE
      IF roundMode = 1 THEN
        Trunc(0);
      ELSIF roundMode = 2 THEN 
        Trunc(1);
      ELSE (* round near *)
        k := bdif - 1;
        DI.ToArr32(sess,b,arr,len);
        IF k < 0 THEN
          Trunc(0);
        ELSIF k = 0 THEN
          IF NOT exact THEN
             RETURN FALSE;
          ELSIF Word.And(arr[0], 2) > 0 THEN 
            Trunc(1);
          ELSE
            Trunc(0);
          END;
        ELSIF Word.And(arr[Word.RightShift(k,KShift)],
                       Word.LeftShift(1,Word.And(k, KMask))) > 0 THEN
          Trunc(1);
        ELSE
          Trunc(0);
        END;
      END;
    END;
    
    RETURN rv # 0;
  END RvOK;

PROCEDURE StrToArr32(str : TEXT; fpi : FPI; VAR exp : INTEGER; VAR bits : DI.RefInt32) : INTEGER =
  VAR
    bbbits, e, e2, inex, irv : INTEGER := 0;
    nd, nd0, nf, roundMode, rvbits, rve, rve1 : INTEGER := 0;
    asub, denorm, sign : BOOLEAN := FALSE;
    suddenUnderflow : BOOLEAN;
    ab, bb, bd, bd0, bs, delta, rvb : DI.T := DI.T{-1,-1}; 
    adj, adj0, rv : LONGREAL := 0.0D0;
    dsign : [-1..1];    
    y,z : Word.T;
    buf : StringBuffer;
    sess : DI.Session;

PROCEDURE AdjustRv(VAR rv : LONGREAL; VAR e2 : INTEGER) =
  VAR
    rv0 : INTEGER;
    eshift : INTEGER;
  BEGIN
    rv0 := FU.Word0(rv);
    eshift := Word.RightShift(Word.And(rv0, ExpMask), ExpShift) - FU.Bias; 
    INC(e2,eshift);
    
    rv0 := Word.And(rv0, Word.Not(ExpMask));
    rv0 := Word.Or(rv0, Word.LeftShift(FU.Bias, ExpShift));
    rv := FU.Word0Set(rv,rv0);  
  END AdjustRv;
  
PROCEDURE CheckRv(VAR e1 : INTEGER) RAISES{ret} =
  CONST
    TenPmax = 22;
  VAR
    i : INTEGER; exact : BOOLEAN;
  BEGIN
    IF e = 0 THEN
      IF RvOK(sess, rv, fpi, exp, bits, TRUE, roundMode, irv) THEN
        RAISE ret;
      END;
    ELSIF e > 0 THEN
      IF e <= TenPmax THEN
        exact := FivesBits[e] + FU.mantbits(rv) <= P;
        rv := rv * tens[e];
        IF RvOK(sess, rv, fpi, exp, bits, exact, roundMode, irv) THEN
          RAISE ret;
        END;
        DEC(e1,e);
      ELSE
        i := DBL_DIG - nd;
        IF e <= TenPmax + i THEN
          (* A fancier test would sometimes let us do this for larger i values.*)
          e2 := e - i;
          DEC(e1, i);
          rv := rv * tens[i];
          rv := rv * tens[e2];
          IF RvOK(sess, rv, fpi, exp, bits, FALSE, roundMode, irv) THEN
            RAISE ret;
          END;
          DEC(e1,e2);
        END;
      END;
    ELSIF e >= -TenPmax THEN
      rv := rv / tens[-e];
      IF RvOK(sess, rv, fpi, exp, bits, FALSE, roundMode, irv) THEN
        RAISE ret;
      END;
      DEC(e1, e);
    END;
  END CheckRv;
  
PROCEDURE InitApprox() RAISES {ret} =
  CONST
    NBIG = Word.LeftShift(1, BigTens - 1);
  VAR
    i,j,k,e1,abse1 : INTEGER := 0;
    tensarr : TensArr;
  BEGIN
    e1 := e;
    IF nd0 = 0 THEN nd0 := nd; END;
    IF nd < DBL_DIG + 1 THEN k := nd; ELSE k := DBL_DIG + 1; END;
    rv := FLOAT(y,LONGREAL);
    IF k > 9 THEN rv := tens[k - 9] * rv + FLOAT(z,LONGREAL); END;

    (* real and longreal speed up *)
    IF (fpi.nbits <= P AND nd <= DBL_DIG) THEN
      CheckRv(e1);
    END;    

    (* Get starting approximation = rv * 10**e1 *)
    INC(e1,nd - k);
    
    (* e2 is a correction to the (base 2) exponent of the return
       value, reflecting adjustments above to avoid overflow in the
       native arithmetic. *)     
    e2 := 0;
    abse1 := ABS(e1);
    i := Word.And(abse1, 15);

    IF e1 > 0 THEN
      tensarr := bigtens;
      IF i # 0 THEN rv := rv * tens[i]; END;
    ELSIF e1 < 0 THEN
      tensarr := tinytens;
      IF i # 0 THEN rv := rv / tens[i]; END;
    END;

    e1 := Word.And(abse1, Word.Not(15));

    IF e1 # 0 THEN
      e1 := Word.RightShift(e1, 4);
      WHILE e1 >= NBIG DO
        AdjustRv(rv,e2);
        rv := rv * tensarr[BigTens-1];
        DEC(e1, Word.LeftShift(1, BigTens-1));
      END; (* while *)

      AdjustRv(rv,e2);

      j := 0;  
      WHILE e1 > 0 DO
        IF Word.And(e1, 1) # 0 THEN
          rv := rv * tensarr[j];
        END;
        e1 := Word.RightShift(e1,1);
        INC(j);
      END;
    END; (* e1 # 0 *)
  END InitApprox;
  
PROCEDURE InitRvb() =
  VAR j : INTEGER;
  BEGIN
    rvb := FU.d2b(sess, rv, rve, rvbits);  (* rv = rvb * 2^rve *)

    INC(rve, e2);
    j := rvbits - fpi.nbits;
    IF j > 0 THEN
      rvb := DI.shift(sess, rvb, -j);
      rvbits := fpi.nbits;
      INC(rve, j);
    END;
    e2 := rve + rvbits - fpi.nbits;
  END InitRvb;
  
PROCEDURE ZeroRvb() =
  BEGIN
    rvb := DI.Zero;
    exp := fpi.emin;
    irv := Word.Or(STRTOG_Underflow, STRTOG_Inexlo);
  END ZeroRvb;

PROCEDURE CheckDenormal() RAISES {huge,ret} =
  VAR j : INTEGER;
  BEGIN
    IF e2 > fpi.emax + 1 THEN
      RAISE huge;
    END;

    rve1 := rve + rvbits - fpi.nbits;
    
    IF e2 < fpi.emin THEN
      denorm := TRUE;
      j := rve - fpi.emin;
      INC(rvbits, j);
      IF j > 0 THEN
        rvb := DI.shift(sess, rvb, j);
      ELSIF j < 0 THEN
        IF rvbits <= 0 THEN
          IF rvbits < -1 THEN
            ZeroRvb();
            RAISE ret;
          END;
          rvb := DI.One;
          rvbits := 1;
        ELSE
          rvb := DI.shift(sess, rvb, j);
        END;
      END; (* j < 0 *)
      rve := fpi.emin;
      rve1 := rve;
      IF suddenUnderflow AND (e2 + 1 < fpi.emin) THEN
        ZeroRvb();
        RAISE ret;
      END;
    END; (* e2 < fpi.emin ... *)
  END CheckDenormal;
  
PROCEDURE Underflow() RAISES {exitLoop} =    
  BEGIN
    rvb := DI.Zero;
    rve := fpi.emin;
    irv := Word.Or(STRTOG_Underflow, STRTOG_Inexlo);
    RAISE exitLoop;
  END Underflow;

PROCEDURE Adj( ) RAISES {adjJump,exitLoop} =    
  BEGIN
    inex := STRTOG_Inexlo;
    IF dsign > 0 THEN
      asub := FALSE;
      inex := STRTOG_Inexhi;
    ELSIF denorm AND bbbits <= 1 THEN
      Underflow();
    END;
    adj0 := 1.0D0;
    adj := adj0;
    RAISE adjJump;
  END Adj;
  
PROCEDURE IncL(VAR adjL : INTEGER) =
  BEGIN
    INC(adjL);
    inex := STRTOG_Inexact - inex;
  END IncL;

PROCEDURE InitBigs(bb0 : INTEGER) =
  VAR 
    i,j,bb2,bd2,bb5,bd5,bbe,bs2 : INTEGER;
  BEGIN
    bd := DI.copy(sess, bd0);
    bb := DI.copy(sess, rvb);
    bbbits := rvbits - bb0;
    bbe := rve + bb0;
    bs := DI.One;

    IF e >= 0 THEN
      bb2 := 0; bb5 := 0;
      bd2 := e; bd5 := e;
    ELSE
      bb2 := -e; bb5 := -e;
      bd2 := 0; bd5 := 0;
    END;
    
    IF bbe >= 0 THEN INC(bb2, bbe); ELSE DEC(bd2, bbe); END;

    bs2 := bb2;
    j := fpi.nbits + 1 - bbbits;
    i := bbe + bbbits - fpi.nbits;

    IF i < fpi.emin THEN INC(j, i - fpi.emin); END; (* denormal *)
    
    INC(bb2, j); INC(bd2, j);
    IF bb2 < bd2 THEN i := bb2; ELSE i := bd2; END;

    IF i > bs2 THEN i := bs2; END;
    IF i > 0 THEN DEC(bb2, i); DEC(bd2, i); DEC(bs2, i); END;

    IF bb5 > 0 THEN
      bs := FU.pow5mult(sess, bs, bb5);
      bb := DI.mult(sess, bs, bb);
    END;
  
    DEC(bb2, bb0);
    bb := DI.shift(sess, bb, bb2);

    IF bd5 > 0 THEN bd := FU.pow5mult(sess, bd, bd5); END;
    IF bd2 > 0 THEN bd := DI.shift(sess, bd, bd2); END;
    IF bs2 > 0 THEN bs := DI.shift(sess, bs, bs2); END;
  END InitBigs;
  
PROCEDURE InitDelta() RAISES {exitLoop} =
  VAR
    len : INTEGER;
    arr : DI.RefInt32;
  BEGIN
    asub := TRUE;
    inex := STRTOG_Inexhi;

    delta := FU.diff(sess, bb, bd, dsign);

    DI.ToArr32(sess,delta,arr,len);
    IF len <= 1 AND arr[0] = 0 THEN
      (* exit loop for simple whole numbers like 123 *)
      RAISE exitLoop;
    END;
  END InitDelta;  
  
PROCEDURE DropDown() RAISES {exitLoop} =
  VAR
    arr : DI.RefInt32;
    len : INTEGER;
  BEGIN
    (* boundary case -- decrement exponent *)
    IF rve1 = fpi.emin THEN
      irv := Word.Or(STRTOG_Normal, STRTOG_Inexhi);
      DI.ToArr32(sess,rvb,arr,len);
      IF len = 1 AND arr[0] = 1 THEN
        suddenUnderflow := TRUE;
      END;
      RAISE exitLoop;
    END;
    DEC(rve, fpi.nbits);
    rvbits := fpi.nbits;
    rvb := FU.setOnes(sess,rvb, rvbits);
    RAISE exitLoop;
  END DropDown;  

PROCEDURE ErrorNotPos(VAR finished : BOOLEAN) RAISES {adjJump,exitLoop} =
  VAR 
    i,j,len : INTEGER;
    w : Word.T;
    arr : DI.RefInt32;
  BEGIN
    irv := STRTOG_Normal;
    finished := Word.Xor(dsign,Word.And(roundMode,1)) # 0;
    IF finished THEN
      IF dsign # 0 THEN      
        irv := Word.Or(irv, STRTOG_Inexhi);
        Adj();
      END;
      irv := Word.Or(irv, STRTOG_Inexlo);
      IF rve1 = fpi.emin THEN    
        Adj();
      END;
      i := 0;
      j := fpi.nbits;
      DI.ToArr32(sess,rvb,arr,len);

      WHILE j >= ULbits DO
        IF Word.And(arr[i], AllOn) > 0 THEN      
          Adj();
        END;
        INC(i);
        DEC(j, ULbits);
      END;
      w := arr[i];
      IF j > 1 AND FU.lo0bits(w) < j - 1  THEN
        Adj();
      END;        
      rve := rve1 - 1;
      rvbits := fpi.nbits;
      rvb := FU.setOnes(sess, rvb, rvbits);
      RAISE exitLoop;
    END; (* if *)
    IF dsign > 0 THEN
      irv := Word.Or(irv,STRTOG_Inexlo);
    ELSE
      irv := Word.Or(irv,STRTOG_Inexhi);
    END;
    RAISE exitLoop;
  END ErrorNotPos;

PROCEDURE ErrorNeg() RAISES {exitLoop} =
  BEGIN
    (* Error is less than half an ulp -- check for
     * special case of mantissa a power of two. *)
    IF dsign > 0 THEN
      irv := Word.Or(STRTOG_Normal, STRTOG_Inexlo);
    ELSE
      irv := Word.Or(STRTOG_Normal, STRTOG_Inexhi);
    END;
        
    IF dsign > 0 OR bbbits > 1 OR denorm OR rve1 = fpi.emin THEN
      RAISE exitLoop;
    END;

    delta := DI.shift(sess, delta, 1);
    IF DI.compare(sess, delta, bs) > 0 THEN
      irv := Word.Or(STRTOG_Normal, STRTOG_Inexlo);
      DropDown(); (* exits loop *)
    END;
    RAISE exitLoop;
  END ErrorNeg;

PROCEDURE ErrorZero() RAISES {exitLoop} =
  VAR 
    j,len : INTEGER;
    sign : [-1..1];
    rvbarr : DI.RefInt32;
  BEGIN
    (* exactly half-way between *)
    IF dsign > 0 THEN
      IF denorm AND FU.allOn(sess, rvb, rvbits) THEN
        (*boundary case -- increment exponent*)
        rvb := DI.One;
        rvbits := 1;
        rve := fpi.emin + fpi.nbits - rvbits;
        irv := Word.Or(STRTOG_Normal, STRTOG_Inexhi);
        denorm := FALSE;
        RAISE exitLoop;
      END;
      irv := Word.Or(STRTOG_Normal, STRTOG_Inexlo);
    ELSIF bbbits = 1 THEN  
      irv := STRTOG_Normal;
      DropDown();
    ELSE
      irv := Word.Or(STRTOG_Normal, STRTOG_Inexhi);
    END; (* dsign *)
    
    DI.ToArr32(sess,rvb,rvbarr,len);    

    IF bbbits < fpi.nbits AND NOT denorm OR Word.And(rvbarr[0], 1) = 0 THEN
      RAISE exitLoop;
    END;

    IF dsign > 0 THEN  
      rvb := DI.add(sess, rvb, DI.One);
      j := Word.And(KMask, (ULbits - Word.And(rvbits, KMask)));
      DI.ToArr32(sess,rvb,rvbarr,len);
      
      IF FU.hi0bits(rvbarr[len - 1]) # j THEN 
        INC(rvbits);
      END;
      irv := Word.Or(STRTOG_Normal, STRTOG_Inexhi);
    ELSE
      IF bbbits = 1 THEN
        Underflow();
      END;
      rvb := FU.diff(sess, rvb, DI.One, sign);
      irv := Word.Or(STRTOG_Normal, STRTOG_Inexlo);
    END;
    RAISE exitLoop;
  END ErrorZero;
          
PROCEDURE SetAdj(VAR adjL : INTEGER) RAISES {adjJump,exitLoop} =
  BEGIN
    adj := FU.ratio(sess, delta, bs);

    IF adj <= 2.0D0 THEN
      Adj();
    ELSE
      adj := adj * 0.5D0;
      adj0 := adj;
        
      IF dsign > 0 THEN
        asub := FALSE;
        inex := STRTOG_Inexlo;
      END;
      IF adj < 2147483647.0D0 THEN
        adjL := TRUNC(adj0);
        adj0 := adj0 - FLOAT(adjL,LONGREAL);
        CASE roundMode OF
          | 0 => IF adj0 >= 0.5D0 THEN IncL(adjL); END;
          | 1 => IF asub AND adj0 > 0.0D0 THEN IncL(adjL); END;
          | 2 => IF NOT asub AND adj0 > 0.0D0 THEN IncL(adjL); END;
        ELSE
          (* nothing *)
        END;
        adj := FLOAT(adjL,LONGREAL);
      END;
    END; (* else *)
  END SetAdj;
  
PROCEDURE InitAB() =
  VAR abe,abits,j : INTEGER;
  BEGIN
    y := rve + rvbits;
    IF NOT denorm AND rvbits < fpi.nbits THEN
      j := fpi.nbits - rvbits;
      rvb := DI.shift(sess, rvb, j);
      DEC(rve, j);
      rvbits := fpi.nbits;
    END;

    ab := FU.d2b(sess, adj, abe, abits);
    ab := DI.shift(sess, ab, abe);
  END InitAB;
  
PROCEDURE CheckAsub(VAR adjL : INTEGER; VAR finished : BOOLEAN) =
  VAR
    rvbarr,rvbarr0 : DI.RefInt32;
    len,len0,k,rvbhi,rvbhi0 : INTEGER;
    rvb0 : DI.T;
    sign : [-1..1];
  BEGIN
    rvb0 := rvb;  
    DI.ToArr32(sess,rvb0,rvbarr0,len0);
      
    IF asub THEN
      rvb := FU.diff(sess, rvb, ab, sign);
      DI.ToArr32(sess,rvb,rvbarr,len);
      
      k := len0 - 1;
      rvbhi0 := FU.hi0bits(rvbarr0[k]);
      rvbhi := FU.hi0bits(rvbarr[k]);

      IF denorm THEN
        (* do nothing *)
      ELSIF len <= k OR rvbhi > rvbhi0 THEN
        (* unlikely; can only have lost 1 high bit *)
        IF rve1 = fpi.emin THEN
          DEC(rvbits);
          denorm := TRUE;
        ELSE
          rvb := DI.shift(sess, rvb, 1);
          DEC(rve);
          DEC(rve1);
          adjL := 0;
          finished := FALSE;
        END;
      END;
    ELSE
      rvb := DI.add(sess, rvb, ab);
      DI.ToArr32(sess,rvb,rvbarr,len);

      k := len - 1;
      rvbhi0 := FU.hi0bits(rvbarr0[k]);
      rvbhi := FU.hi0bits(rvbarr[k]);

      IF k >= len0 OR rvbhi < rvbhi0 THEN
        IF denorm THEN
          INC(rvbits); 
          IF rvbits = fpi.nbits THEN
            denorm := FALSE;
          END;
        ELSE
          rvb := DI.shift(sess, rvb, -1);
          INC(rve);
          INC(rve1);
          adjL := 0;
        END;
      END;
    END;
  END CheckAsub;

PROCEDURE CheckAdj(adjL : INTEGER; finished : BOOLEAN) RAISES {exitLoop} =
  VAR tol : LONGREAL;
  BEGIN
    IF finished THEN RAISE exitLoop END;
    z := rve + rvbits;

    IF y = z AND adjL > 0 THEN
      (* Can we stop now? *)
      tol := adj * 5.0D-16; (* > max rel error *)
      adj := adj0 - 0.5D0;
      IF adj < -tol THEN
        IF adj0 > tol THEN
          irv := Word.Or(irv, inex);
          RAISE exitLoop;
        END;
      ELSIF adj > tol AND adj0 < 1.0D0 - tol THEN
        irv := Word.Or(irv, inex);
        RAISE exitLoop;
      END;
    END;
  END CheckAdj;

PROCEDURE MainLoop() =
  VAR 
    adjL, cmp : INTEGER;
    finished : BOOLEAN;
    bb0 : INTEGER := 0;  (* trailing zero bits in rvb *)
  BEGIN
    (* Put digits into bd: true value = bd * 10^e *)
    TRY
      (* Max times through this loop is 3 *)
      LOOP
        InitBigs(bb0);
        InitDelta();
        adjL := 0; finished := FALSE;
        
        cmp := DI.compare(sess, delta, bs);
        TRY    
          IF roundMode > 0 AND cmp <= 0 THEN ErrorNotPos(finished); END;
          IF cmp < 0 THEN ErrorNeg(); END;
          IF cmp = 0 THEN ErrorZero(); END;
          SetAdj(adjL);
        EXCEPT
        | adjJump => (*nothing*)
        END;
     
        InitAB();
        CheckAsub(adjL,finished);
        CheckAdj(adjL,finished); 
       
        IF denorm THEN bb0 := 0; ELSE bb0 := FU.trailz(sess,rvb); END;
      END; (* loop *)
    EXCEPT
    | exitLoop => (* nothing *)
    END; (* try *)
  END MainLoop;
  
PROCEDURE FixRvb() RAISES {huge} =
  VAR j : INTEGER;
  BEGIN
    j := fpi.nbits - rvbits;
    IF NOT denorm AND j > 0 THEN
      rvb := DI.shift(sess, rvb, j);    
      DEC(rve, j);
    END;
    exp := rve;
    IF rve > fpi.emax THEN
      RAISE huge
    END;
  END FixRvb;
  
PROCEDURE Huge() =
  BEGIN
    rvb := DI.Zero;
    irv := Word.Or(Word.Or(STRTOG_Infinite, STRTOG_Overflow), STRTOG_Inexhi);
    exp := fpi.emax + 1;  
  END Huge;

PROCEDURE Finalise() =
  VAR ret,len : INTEGER;
  BEGIN
    IF denorm THEN
      IF suddenUnderflow THEN
        rvb := DI.Zero;
        irv := Word.Or(STRTOG_Underflow, STRTOG_Inexlo);
      ELSE
        IF rvb.s > 0 THEN
          ret := STRTOG_Denormal;
        ELSE
          ret := STRTOG_Zero;
        END;
      
        irv := Word.Or(Word.And(irv, Word.Not(STRTOG_Retmask)), ret);
        IF Word.And(irv, STRTOG_Inexact) > 0 THEN
          irv := Word.Or(irv, STRTOG_Underflow);
        END;
      END;
    END;

    IF sign THEN
      irv := Word.Or(irv, STRTOG_Neg);
    END;
    
    IF rvb.s >= 0 THEN
      DI.ToArr32(sess, rvb, bits, len);
    END;    
  END Finalise;
  
PROCEDURE InitRounding() =
  BEGIN
    roundMode := 0;
    CASE Word.And(ORD(fpi.rounding), 3) OF
    | ORD(RoundingModes.RoundUp) =>
        roundMode := 2 - ORD(sign);
    | ORD(RoundingModes.RoundZero) =>
        roundMode := 1;
    | ORD(RoundingModes.RoundDown) =>
        roundMode := 1 + ORD(sign);
    ELSE
    END;
  END InitRounding;
  
  BEGIN (* StrToArr32 *)
    sess := DI.NewSession();
    suddenUnderflow := fpi.suddenUnderflow;
    
    TRY
      ScanNumber (str, buf, y, z, nd, nd0, nf, e, sign);
      IF nd = 0 AND nd0 = 0 THEN RETURN STRTOG_Zero; END;
    EXCEPT
    | illegalLit => RETURN STRTOG_NoNumber;
    END;
    
    irv := STRTOG_Normal;
    DEC(e,nf); 
    
    InitRounding();
    
    (* Now we have nd0 digits, starting at s0, followed by a
     * decimal point, followed by nd-nd0 digits.  The number we're
     * after is the integer represented by those digits times
     * 10**e *)
    TRY
      InitApprox();
      InitRvb();
      CheckDenormal();
      bd0 := FU.s2b(sess, buf, nd0, nd, y);    
      
      MainLoop();
      FixRvb();
    EXCEPT
    |  huge => Huge();
    |  ret => (*nothing*)
    END;    
    Finalise();
    DI.EndSession(sess);
    RETURN irv;
  END StrToArr32; 

PROCEDURE Arr32toReal(bits : DI.RefInt32; exp, k : INTEGER) : RealRep.T =
  VAR
    real : RealRep.T;
  BEGIN
    real.sign := 0; 
    IF Word.And(k, STRTOG_Neg) = STRTOG_Neg THEN
      real.sign := 1; 
    END;
    CASE Word.And(k, STRTOG_Retmask) OF
    |  STRTOG_NoNumber, STRTOG_Zero =>
      real.exponent := 0;
      real.significand := 0;
    | STRTOG_Normal, STRTOG_NaNbits =>
      real.exponent := exp + 16_7f + 23;
      real.significand := Word.And(bits[0],16_7FFFFF);
    | STRTOG_Denormal =>
      real.exponent := Word.Extract(bits[0],23,8);
      real.significand := Word.And(bits[0],16_7FFFFF);
    | STRTOG_Infinite =>
      real.exponent := 16_7F;
      real.significand := 0;
    | STRTOG_NaN => (* quiet NaN msb of significand set to 1, however never set *)    
      real.exponent := 16_7F;
      real.significand := 16_8000;
    ELSE (* nothing *)
    END;
    RETURN real;
  END Arr32toReal;
  
PROCEDURE Arr32toLongReal(bits : DI.RefInt32; exp, k : INTEGER) : LongRealRep.T =
  VAR
    long : LongRealRep.T;
  BEGIN
    long.sign := 0; 
    IF Word.And(k, STRTOG_Neg) = STRTOG_Neg THEN
      long.sign := 1; 
    END;
    CASE Word.And(k, STRTOG_Retmask) OF
    |  STRTOG_NoNumber, STRTOG_Zero =>
      long.exponent := 0;
      long.significand0 := 0;
      long.significand1 := 0;
    | STRTOG_Normal, STRTOG_NaNbits =>
      long.exponent := exp + 16_3ff + 52;
      long.significand0 := Word.And(bits[1],Word.Not(16_100000));
      long.significand1 := bits[0];
    | STRTOG_Denormal =>
      long.exponent := Word.Extract(bits[1],20,11);
      long.significand0 := Word.And(bits[1],16_FFFFF);
      long.significand1 := bits[0];
    | STRTOG_Infinite =>
      long.exponent := 16_7FF;
      long.significand0 := 0;
      long.significand1 := 0;
    | STRTOG_NaN => (* quiet NaN msb of significand set to 1, however never set *)    
      long.exponent := 16_7FF;
      long.significand0 := 16_8000;
      long.significand1 := 0;
    ELSE (* nothing *)
    END;
    RETURN long;
  END Arr32toLongReal;
  
PROCEDURE Arr32toQuad(bits : DI.RefInt32; exp, k : INTEGER) : ExtendedRep =
  VAR
    ext : ExtendedRep;
  BEGIN
    ext.sign := 0; 
    IF Word.And(k, STRTOG_Neg) = STRTOG_Neg THEN
      ext.sign := 1; 
    END;
    CASE Word.And(k, STRTOG_Retmask) OF
    | STRTOG_NoNumber, STRTOG_Zero =>
      ext.exponent := 0;
      ext.significand0 := 0;
      ext.significand1 := 0;
      ext.significand2 := 0;
      ext.significand3 := 0;    
    | STRTOG_Normal, STRTOG_NaNbits =>
      ext.exponent := exp + 16_3fff + 112;
      ext.significand0 := Word.And(bits[3],Word.Not(16_10000));
      ext.significand1 := bits[2];
      ext.significand2 := bits[1];
      ext.significand3 := bits[0];        
    | STRTOG_Denormal =>
      ext.exponent := Word.Extract(bits[3],16,15);
      ext.significand0 := Word.And(bits[3],16_FFFF);
      ext.significand1 := bits[2];
      ext.significand2 := bits[1];
      ext.significand3 := bits[0];    
    | STRTOG_Infinite =>
      ext.exponent := 16_7FFF;
      ext.significand0 := 0;
      ext.significand1 := 0;
      ext.significand2 := 0;
      ext.significand3 := 0;
    | STRTOG_NaN => (* quiet NaN msb of significand set to 1, however never set *)    
      ext.exponent := 16_7FFF;
      ext.significand0 := 16_8000;
      ext.significand1 := 0;
      ext.significand2 := 0;
      ext.significand3 := 0;
    ELSE (* nothing *)
    END;
    RETURN ext;
  END Arr32toQuad;

PROCEDURE StrToReal(in : TEXT; rounding : RoundingModes; VAR real : RealRep.T) : INTEGER =
  VAR
    ret,exp : INTEGER;
    bits : DI.RefInt32;
    fpi : FPI;
  BEGIN
    fpi := NEW(FPI, nbits := 24, rounding := rounding, emin := 1-127-24+1,      
                    emax := 254-127-24+1, suddenUnderflow := FALSE);  
    ret := StrToArr32(in, fpi, exp, bits );
    real := Arr32toReal(bits,exp,ret);
    RETURN ret;
END StrToReal;

PROCEDURE StrToLongReal(in : TEXT; rounding : RoundingModes; VAR long : LongRealRep.T) : INTEGER =
  VAR
    ret,exp : INTEGER;
    bits : DI.RefInt32;
    fpi : FPI;
  BEGIN
    fpi := NEW(FPI, nbits := 53, rounding := rounding, emin := 1-1023-53+1,      
                    emax := 2046-1023-53+1, suddenUnderflow := FALSE);  
    ret := StrToArr32(in, fpi, exp, bits );
    long := Arr32toLongReal(bits,exp,ret);
    RETURN ret;
  END StrToLongReal;

PROCEDURE StrToExtended(in : TEXT; rounding : RoundingModes; VAR quad : ExtendedRep) : INTEGER =
  VAR
    ret,exp : INTEGER;
    bits : DI.RefInt32;
    fpi : FPI;
  BEGIN
    fpi := NEW(FPI, nbits := 113, rounding := rounding, emin := 1-16383-113+1,      
                    emax := 32766-16383-113+1, suddenUnderflow := FALSE);  
    ret := StrToArr32(in, fpi, exp, bits );
    quad := Arr32toQuad(bits,exp,ret);
    RETURN ret;
  END StrToExtended;
  
BEGIN
  FOR c := '0' TO '9' DO Digits[c] := TRUE END;
END TextToFloat.
