(* Copyright (C) 1992, Xerox                                                 *)
(* All rights reserved.                                                      *)

(* Last modified on Thu Jan 26 13:47:45 PST 1995 by kalsow                   *)
(*      modified on Thu May 13 09:10:35 PDT 1993 by mcjones                  *)
(*      modified on Wed Mar  4 23:35:36 PST 1992 by muller                   *)


UNSAFE MODULE FloatMode;

(* XXX: this implementation only gets/sets flags globally, not per
   thread *)

(*
 * Unsafe because TtoS is potentially unsafe, however it is not in
 * our use of it.
 *)
FROM FPU IMPORT ieee_flags;
IMPORT FPU, Ctypes, Word, Usignal, M3toC;

(*
<sys/ieeefp.h>
    enum fp_direction_type 		/* rounding direction */
        {
        fp_nearest	= 0,
        fp_tozero	= 1,
        fp_positive	= 2,
        fp_negative	= 3
        } ;
*)

PROCEDURE SetRounding (md: RoundingMode) RAISES {Failure} =
  VAR
    dummy: Ctypes.char_star;
    x    : INTEGER;
  BEGIN
    x := ieee_flags(setStr, directionStr, rndModeToSunOs[md], dummy);
    IF x # 0 THEN RAISE Failure; END;
  END SetRounding;

PROCEDURE GetRounding (): RoundingMode =
  CONST
    sunOsToRndMode = ARRAY [0 .. 3] OF RoundingMode{
                       RoundingMode.NearestElseEven,
                       RoundingMode.TowardZero,
                       RoundingMode.TowardPlusInfinity,
                       RoundingMode.TowardMinusInfinity };
  VAR
    x    : INTEGER;
    dummy: Ctypes.char_star;
  BEGIN
    x := ieee_flags(getStr, directionStr, nullStr, dummy);
    RETURN (sunOsToRndMode[x]);
  END GetRounding;

(* XXX: this only gets/sets flags globally, not per thread *)

(*
<sys/ieeefp.h>
enum fp_exception_type		/* exceptions according to bit number */
        {
        fp_inexact	= 0,
        fp_division	= 1,
        fp_underflow	= 2,
        fp_overflow	= 3,
        fp_invalid	= 4
        } ;
*)

PROCEDURE GetFlags (): SET OF Flag =
  CONST
    sunOsToFlag = ARRAY [0 .. 4] OF
                    Flag{Flag.Inexact, Flag.DivByZero, Flag.Underflow,
                         Flag.Overflow, Flag.Invalid};
  VAR
    x     : INTEGER;
    dummy : Ctypes.char_star;
    excpts                  := SET OF Flag{};
  BEGIN
    x := ieee_flags(getStr, exceptionStr, nullStr, dummy);
    FOR i := 0 TO NUMBER(sunOsToFlag) - 1 DO
      IF Word.Extract(x, i, 1) # 0 THEN
        excpts := excpts + SET OF Flag{sunOsToFlag[i]};
      END;
    END;
    RETURN (excpts);
  END GetFlags;

PROCEDURE SetFlags (s: SET OF Flag): SET OF Flag =
  VAR
    x    : INTEGER         := 0;
    dummy: Ctypes.char_star;
    out                    := SET OF Flag{};
  BEGIN
    out := GetFlags();
    FOR i := FIRST(Flag) TO LAST(Flag) DO
      IF i IN s THEN
        x := ieee_flags(setStr, exceptionStr, flagToSunOs[i], dummy);
      ELSE
        x :=
          ieee_flags(clearStr, exceptionStr, flagToSunOs[i], dummy);
      END;
      <* ASSERT x = 0 *>
    END;
    RETURN out;
  END SetFlags;

PROCEDURE ClearFlag (f: Flag) =
  VAR
    x    : INTEGER         := 0;
    dummy: Ctypes.char_star;
  BEGIN
    x := ieee_flags(clearStr, exceptionStr, flagToSunOs[f], dummy);
    <* ASSERT x = 0 *>
  END ClearFlag;

PROCEDURE GetBehavior (f: Flag): Behavior =
  VAR x: INTEGER;
  BEGIN
    CASE f OF
    | Flag.IntOverflow => RETURN (Behavior.Ignore);
    | Flag.IntDivByZero => RETURN (Behavior.Trap);
    ELSE
      x := FPU.ieee_handler(getStr, flagToSunOs[f], NIL);
      IF x = 0 OR x = 1 THEN    (* SIGFPE_DEFAULT or SIGFPE_IGNORE *)
        RETURN (Behavior.SetFlag);
      ELSE
        RETURN (Behavior.Trap);
      END;
    END;
  END GetBehavior;

PROCEDURE SetBehavior (f: Flag; b: Behavior) RAISES {Failure} =
  VAR x: INTEGER;
  BEGIN
    CASE f OF
    | Flag.IntOverflow => RAISE Failure;
    | Flag.IntDivByZero => RAISE Failure;
    ELSE
      CASE b OF
      | Behavior.Trap =>
          x := FPU.ieee_handler(
                 setStr, flagToSunOs[f], HandleFPE);
          IF x = 1 THEN RAISE Failure; END;
      | Behavior.SetFlag =>
          (*
           * using "clear" instead of "set" would be logical, but
           * ieee_handler doesn't clear the handler structure when
           * given "clear", it only turns off trapping.  A final arg
           * to ieee_handler of NIL (=0) corresponds to SIGFPE_DEFAULT
           *)
          x := FPU.ieee_handler(setStr, flagToSunOs[f], NIL);
          IF x = 1 THEN RAISE Failure; END;
      | Behavior.Ignore => RAISE Failure;
      END;
    END;
  END SetBehavior;

(*
<sys/signal.h>
#define	    FPE_INTOVF_TRAP	0x1	/* integer overflow */
#define	    FPE_STARTSIG_TRAP	0x2	/* process using fp */
#define	    FPE_INTDIV_TRAP	0x14	/* integer divide by zero */
#define	    FPE_FLTINEX_TRAP	0xc4	/* [floating inexact result] */
#define	    FPE_FLTDIV_TRAP	0xc8	/* [floating divide by zero] */
#define	    FPE_FLTUND_TRAP	0xcc	/* [floating underflow] */
#define	    FPE_FLTOPERR_TRAP	0xd0	/* [floating operand error] */
#define	    FPE_FLTOVF_TRAP	0xd4	/* [floating overflow] */
*)

PROCEDURE HandleFPE (<* UNUSED *> sig      : INTEGER;
                                  code     : INTEGER;
                     <* UNUSED *> scp, addr: ADDRESS  ) RAISES {Trap} =
  VAR old: INTEGER;
  BEGIN
    (*
     * since the RAISE does a longjump, never leave unix signal
     * handler, and sigmask is never restored.  So restore it here.
     *)
    old := Usignal.sigsetmask(0);
    EVAL (Usignal.sigsetmask(Word.And(old, Word.Not(128))));
    CASE code OF                <* NOWARN *>
    | 16_c4 => RAISE Trap(Flag.Inexact);
    | 16_c8 => RAISE Trap(Flag.DivByZero);
    | 16_cc => RAISE Trap(Flag.Underflow);
    | 16_d0 => RAISE Trap(Flag.Invalid);
    | 16_d4 => RAISE Trap(Flag.Overflow);

    | 16_01 => RAISE Trap(Flag.IntOverflow); (* should never get here *)
    | 16_14 => RAISE Trap(Flag.IntDivByZero);
    END;
  END HandleFPE;

PROCEDURE InitThread (<*UNUSED*> VAR state: ThreadState) =
    BEGIN
        END InitThread;

PROCEDURE BuildConversionArrays () =
  CONST
    rndModes = ARRAY RoundingMode OF
                 TEXT{
                 "nearest", "negative", "positive", "tozero", "xxx", ..};
    flags = ARRAY Flag OF
              TEXT{"invalid", "inexact", "overflow", "underflow",
                   "division", "", ""};

  BEGIN
    FOR i := FIRST(rndModes) TO LAST(rndModes) DO
      rndModeToSunOs[i] := M3toC.TtoS(rndModes[i]);
    END;
    FOR i := FIRST(flags) TO LAST(flags) DO
      flagToSunOs[i] := M3toC.TtoS(flags[i]);
    END;
  END BuildConversionArrays;

VAR
  setStr, directionStr, getStr: Ctypes.char_star;
  exceptionStr, nullStr, clearStr: Ctypes.char_star;
  rndModeToSunOs: ARRAY RoundingMode OF Ctypes.char_star;
  flagToSunOs: ARRAY Flag OF Ctypes.char_star;
BEGIN
  setStr := M3toC.TtoS("set");
  directionStr := M3toC.TtoS("direction");
  getStr := M3toC.TtoS("get");
  exceptionStr := M3toC.TtoS("exception");
  nullStr := M3toC.TtoS("");
  clearStr := M3toC.TtoS("clear");

  BuildConversionArrays();

  (* 16_14 = INTDIV_TRAP from above *)
  EVAL (FPU.sigfpe(16_14, HandleFPE));

END FloatMode.
