(* Copyright (C) 1992, Xerox                                                 *)
(* All rights reserved.                                                      *)

(* Last modified on Thu Jan 26 13:49:13 PST 1995 by kalsow                   *)
(*      modified on Thu May 13 09:10:12 PDT 1993 by mcjones                  *)
(*      modified on Thu Apr 29 15:47:28 PDT 1993 by muller                   *)


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
        fp_negative	= 1,
        fp_positive	= 2,
        fp_tozero	= 3
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
                       RoundingMode.TowardMinusInfinity,
                       RoundingMode.TowardPlusInfinity,
                       RoundingMode.TowardZero };
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
        fp_invalid	= 0,
        fp_denormalized	= 1,
        fp_division	= 2,
        fp_overflow	= 3,
        fp_underflow	= 4,
        fp_inexact      = 5
        } ;
*)

PROCEDURE GetFlags (): SET OF Flag =
  CONST
    sunOsToFlag = ARRAY [0 .. 5] OF Flag{
                    Flag.Invalid,
                    Flag.Underflow,  (* Denormalized *)
                    Flag.DivByZero,
                    Flag.Overflow,
                    Flag.Underflow,
                    Flag.Inexact };
  VAR
    x     : INTEGER;
    dummy : Ctypes.char_star;
    excpts := SET OF Flag{};
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
    EVAL( ieee_flags( clearAllStr, exceptionStr, dummy, dummy));
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
#define     FPE_INTDIV_TRAP	0x14	/* integer divide by zero */
#define     FPE_INTOVF_TRAP	0x15	/* integer overflow */
#define     FPE_FLTOPERR_TRAP	0x01	/* [floating operand error] */
#define     FPE_FLTDEN_TRAP	0x02	/* [floating denormalized operand] */
#define     FPE_FLTDIV_TRAP	0x03	/* [floating divide by zero] */
#define     FPE_FLTOVF_TRAP	0x04	/* [floating overflow] */
#define     FPE_FLTUND_TRAP	0x05	/* [floating underflow] */
#define     FPE_FLTINEX_TRAP	0x06	/* [floating inexact result] */
#define     FPE_UUOP_TRAP	0x07	/* [floating undefined opcode] */
#define     FPE_DATACH_TRAP	0x08	/* [floating data chain exception] */
#define     FPE_FLTSTK_TRAP	0x10	/* [floating stack fault] */
#define     FPE_FPA_ENABLE	0x11	/* [FPA not enabled] */
#define     FPE_FPA_ERROR	0x12	/* [FPA arithmetic exception] */
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
    | 16_06 => RAISE Trap(Flag.Inexact);
    | 16_03 => RAISE Trap(Flag.DivByZero);
    | 16_05 => RAISE Trap(Flag.Underflow);
    | 16_01 => RAISE Trap(Flag.Invalid);
    | 16_04 => RAISE Trap(Flag.Overflow);
    | 16_02 => RAISE Trap(Flag.Underflow); (* Trap(Flag.Denormalized); *)
    | 16_07 => RAISE Trap(Flag.Invalid);
    | 16_14 => RAISE Trap(Flag.IntDivByZero);
    | 16_15 => RAISE Trap(Flag.IntOverflow);
    END;
  END HandleFPE;

PROCEDURE InitThread (<*UNUSED*> VAR state: ThreadState) =
    BEGIN
        END InitThread;

PROCEDURE BuildConversionArrays () =
  CONST
    rndModes = ARRAY RoundingMode OF
                 TEXT{
                 "nearest", "negative", "positive", "tozero",  "xxx", ..};
    flags = ARRAY Flag OF
              TEXT{"invalid", "inexact", "overflow", 
                   "underflow", "division", "xxx", ..};

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
  clearAllStr: Ctypes.char_star;
BEGIN
  setStr := M3toC.TtoS("set");
  directionStr := M3toC.TtoS("direction");
  getStr := M3toC.TtoS("get");
  exceptionStr := M3toC.TtoS("exception");
  nullStr := M3toC.TtoS("");
  clearStr := M3toC.TtoS("clear");
  clearAllStr := M3toC.TtoS ("clearall");

  BuildConversionArrays();

  (* 16_14 = INTDIV_TRAP from above *)
  EVAL (FPU.sigfpe(16_14, HandleFPE));

END FloatMode.
