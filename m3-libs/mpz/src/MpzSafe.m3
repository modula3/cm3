(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE MpzSafe EXPORTS Mpz;
IMPORT MpzRep;
IMPORT Word;
IMPORT Debug;
FROM Fmt IMPORT F;


PROCEDURE Format(t : T; base := FormatBase.Decimal) : TEXT =
  BEGIN
    CASE base OF
      FormatBase.Binary => <*ASSERT FALSE*>
    |
      FormatBase.Octal =>  RETURN FormatOctal(t)
    |
      FormatBase.Decimal => RETURN FormatDecimal(t)
    |
      FormatBase.Hexadecimal => RETURN FormatHexadecimal(t)
    END     
  END Format;
  
PROCEDURE InitScan(txt : TEXT; base : CARDINAL) : T =
  VAR
    res := NEW(T);
  BEGIN
    EVAL init_set_str(res, txt, base);
    RETURN res
  END InitScan;

PROCEDURE pow(p, b, x : T) =
  VAR
    xui := get_ui(x);
  BEGIN
    <*ASSERT fits_ulong_p(x) = 1*>
    pow_ui(p, b, xui);
  END pow;

PROCEDURE ToInteger(t : T) : INTEGER =
  BEGIN
    IF cmp(t, MaxInt) > 0 OR cmp(t, MinInt) < 0 THEN
      Debug.Error(F("Mpz.ToInteger : not an INTEGER : %s [%s]",
                    FormatDecimal(t), FormatBased(t, 16)))
    END;
    RETURN get_si(t)
  END ToInteger;

PROCEDURE ToWord(t : T) : Word.T =
  BEGIN
    IF cmp(t, MaxWord) > 0 OR cmp(t, Zero) < 0 THEN
      Debug.Error(F("Mpz.ToWord : not a Word.T : %s [%s]",
                    FormatDecimal(t), FormatBased(t, 16)))
    END;
    RETURN get_ui(t)
  END ToWord;

PROCEDURE NewInt(int : INTEGER) : T =
  VAR
    res := NEW(T);
  BEGIN
    init_set_si(res, int);
    RETURN res
  END NewInt;

PROCEDURE NewWord(w : Word.T) : T =
  VAR
    res := NEW(T);
  BEGIN
    init_set_ui(res, w);
    RETURN res
  END NewWord;
  PROCEDURE LeftShift(f0 : T; f1 : T; amt : CARDINAL) =
  BEGIN
    mul_2exp(f0, f1, amt)
  END LeftShift;

PROCEDURE RightShift(f0 : T; f1 : T; amt : CARDINAL) =
  BEGIN
    fdiv_q_2exp(f0, f1, amt)
  END RightShift;

PROCEDURE Shift(f0 : T; f1 : T; amt : INTEGER) =
  BEGIN
    IF amt < 0 THEN
      RightShift(f0, f1, -amt)
    ELSE
      LeftShift(f0, f1, amt)
    END
  END Shift;

PROCEDURE ShiftMpz(f0 : T; f1 : T; amt : T) =
  BEGIN
    WITH minc = cmp(f1, MinInt),
         maxc = cmp(f1, MaxInt) DO
      IF minc = -1 THEN
        WITH c = cmp(f0, Zero) DO
          IF c = -1 THEN
            set(f0, Neg1);
            RETURN
          ELSE
            set(f0, Zero);
            RETURN
          END
        END
      ELSIF maxc = 1 THEN
        IF    cmp(f0, Neg1) = 0 THEN
          set(f0, Neg1);
          RETURN
        ELSIF cmp(f0, Zero) = 0 THEN
          set(f0, Zero);
          RETURN
        ELSE
          <*ASSERT FALSE*> (* makes no sense, OOM ... *)
        END
      END;
      Shift(f0, f1, ToInteger(amt));
      RETURN
    END
  END ShiftMpz;
  
PROCEDURE ShiftNegMpz(f0 : T; f1 : T; amt : T) =
  BEGIN
    WITH minc = cmp(f1, NegMaxInt),
         maxc = cmp(f1, MaxInt) DO
      IF minc = -1 THEN
        WITH c = cmp(f0, Zero) DO
          IF c = -1 THEN
            set(f0, Neg1);
            RETURN
          ELSE
            set(f0, Zero);
            RETURN
          END
        END
      ELSIF maxc = 1 THEN
        IF    cmp(f0, Neg1) = 0 THEN
          set(f0, Neg1);
          RETURN
        ELSIF cmp(f0, Zero) = 0 THEN
          set(f0, Zero);
          RETURN
        ELSE
          <*ASSERT FALSE*> (* makes no sense, OOM ... *)
        END
      END;
      Shift(f0, f1, -ToInteger(amt));
      RETURN
    END
  END ShiftNegMpz;
  

VAR
  MinInt  := NEW(T);
  MaxInt  := NEW(T);
  NegMaxInt := NewInt(-LAST(INTEGER));
  Zero    := NEW(T);
  One     := NEW(T);
  Two     := NEW(T);
  Neg1    := NewInt(-1);
  MaxWord := NEW(T);
BEGIN
  init_set_si(MinInt , FIRST(INTEGER));
  init_set_si(MaxInt , LAST(INTEGER));
  init_set_si(Zero   , 0);
  init_set_si(One    , 1);
  init_set_si(Two    , 2);
  init_set_si(MaxWord, 2);
  pow_ui     (MaxWord, MaxWord, 64);
  sub_ui     (MaxWord, MaxWord,  1);
END MpzSafe.
