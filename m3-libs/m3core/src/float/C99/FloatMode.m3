(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Fri Jan 27 11:53:11 PST 1995 by kalsow     *)
(*      modified on Thu May 13 09:11:04 PDT 1993 by mcjones    *)
(*      modified on Fri May  7 14:30:09 PDT 1993 by muller     *)

(* C99 version of FloatMode *)


MODULE FloatMode;

IMPORT FloatModeC;
IMPORT Word;

PROCEDURE SetRounding(md: RoundingMode) RAISES { Failure } =
  BEGIN
    WITH cm = RoundingCMap[md] DO
      IF cm = LAST(INTEGER) THEN RAISE Failure END;
      FloatModeC.fesetround(cm)
    END
  END SetRounding;

PROCEDURE GetRounding(): RoundingMode =
  BEGIN
    WITH cm = FloatModeC.fegetround() DO
      FOR i := FIRST(RoundingCMap) TO LAST(RoundingCMap) DO
        IF cm = RoundingCMap[i] THEN RETURN i END
      END;
      <*ASSERT FALSE, "FloatModeC.fegetround returned unknown rounding mode"*>
    END
  END GetRounding;

PROCEDURE GetFlags(): SET OF Flag =
  VAR res := SET OF Flag {};
  BEGIN
    WITH cset = FloatModeC.fetestexcept(AllCExcepts) DO
      FOR i := FIRST(Flag) TO LAST(Flag) DO
        WITH cf = FlagCMap[i] DO
          IF cf # LAST(INTEGER) THEN
            IF Word.And(cset,cf) = cf THEN
              res := res + SET OF Flag { i }
            END
          END
        END
      END
    END;
    RETURN res
  END GetFlags;

PROCEDURE SetFlags(s: SET OF Flag): SET OF Flag RAISES { Failure } =
  VAR
    old := GetFlags();
    raisemask, clearmask : Word.T := 0;
  BEGIN
    FOR f := FIRST(Flag) TO LAST(Flag) DO
      IF FlagCMap[f] = LAST(INTEGER) THEN RAISE Failure END;

      IF         f IN old AND NOT f IN s THEN
        clearmask := Word.Or(clearmask, FlagCMap[f])
      ELSIF  NOT f IN old AND     f IN s THEN
        raisemask := Word.Or(raisemask, FlagCMap[f])
      END
    END;

    IF clearmask # 0 THEN  FloatModeC.feclearexcept(clearmask) END;
    IF raisemask # 0 THEN  FloatModeC.feraiseexcept(raisemask) END;

    RETURN old
  END SetFlags;

PROCEDURE ClearFlag(f: Flag)=
  BEGIN
    WITH cf = FlagCMap[f] DO
      (* we ignore attempts to clear unrecognized flags *)
      IF cf # LAST(INTEGER) THEN
        FloatModeC.feclearexcept(cf)
      END
    END
  END ClearFlag;

PROCEDURE SetBehavior(<*UNUSED*> f: Flag; <*UNUSED*> b: Behavior) =
  BEGIN
    <*ASSERT FALSE, "FloatMode.SetBehavior not implemented" *>
  END SetBehavior;

PROCEDURE GetBehavior(<*UNUSED*> f: Flag): Behavior =
  BEGIN
    <*ASSERT FALSE, "FloatMode.GetBehavior not implemented" *>
  END GetBehavior;

(*------------------------------------------------- thread initialization ---*)

PROCEDURE InitThread (<*UNUSED*> VAR state: ThreadState) =
  BEGIN
  END InitThread;

VAR (*CONST*)RoundingCMap : ARRAY RoundingMode OF INTEGER;
VAR (*CONST*)FlagCMap     : ARRAY Flag         OF INTEGER;
VAR (*CONST*)AllCExcepts  : Word.T := 0;



BEGIN
  VAR cm : INTEGER;
  BEGIN
    FOR rm := FIRST(RoundingCMap) TO LAST(RoundingCMap) DO
      CASE rm OF
        RoundingMode.NearestElseEven     =>
        cm := FloatModeC.get_FE_TONEAREST()
      | RoundingMode.TowardMinusInfinity =>
        cm := FloatModeC.get_FE_DOWNWARD()
      | RoundingMode.TowardPlusInfinity  =>
        cm := FloatModeC.get_FE_UPWARD()
      | RoundingMode.TowardZero          =>
        cm := FloatModeC.get_FE_TOWARDZERO()
      ELSE
        cm := LAST(INTEGER)
      END;

      RoundingCMap[rm] := cm
      
    END
  END;

  VAR cf : INTEGER;
  BEGIN
    FOR f := FIRST(FlagCMap) TO LAST(FlagCMap) DO
      CASE f OF
        Flag.Invalid =>
        cf := FloatModeC.get_FE_INVALID()
      |
        Flag.Inexact =>
        cf := FloatModeC.get_FE_INEXACT()
      |
        Flag.Overflow =>
        cf := FloatModeC.get_FE_OVERFLOW()
      |
        Flag.Underflow =>
        cf := FloatModeC.get_FE_UNDERFLOW()
      |
        Flag.DivByZero =>
        cf := FloatModeC.get_FE_DIVBYZERO()


        (* IntOverflow and IntDivByZero not handled here *)

      ELSE
        cf := LAST(INTEGER)
      END;

      IF cf # LAST(INTEGER) THEN
        AllCExcepts := Word.Or(cf, AllCExcepts)
      END;

      FlagCMap[f] := cf
      
    END
  END
END FloatMode.
