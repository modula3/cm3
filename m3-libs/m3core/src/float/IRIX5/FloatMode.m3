(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Thu Jan 26 13:48:48 PST 1995 by kalsow     *)
(*      modified on Fri Oct  7 10:03:46 PDT 1994 by ericv      *)
(*      modified on Thu May 13 09:11:30 PDT 1993 by mcjones    *)
(*      modified on Mon Feb 24 11:07:36 PST 1992 by muller     *)


UNSAFE MODULE FloatMode (* FOR IRIX5 *);

(* NOTE: the following code makes some assumptions about threads:

   1) thread switching (via _setjmp/_longjmp) properly preserves the IEEE
      "sticky" and "trap enable" bits for each thread.

   2) when the signal for a float-point exception is actually delivered,
      we're running in the thread that caused the exception and
      that thread asked for the particular fault to be trapped.
      (i.e. signals are delivered quickly and thread switching doesn't
       doesn't cause floating-point exceptions)
*)

IMPORT FPU, Usignal, ThreadF, Word, RTMisc;

TYPE RM  = RoundingMode;
TYPE MRM = FPU.RoundingMode;

PROCEDURE SetRounding(md: RoundingMode) RAISES {Failure} =
  BEGIN
    CASE md OF
    | RM.TowardMinusInfinity => EVAL FPU.SetRounding (ORD (MRM.ToMinusInfinity));
    | RM.TowardPlusInfinity  => EVAL FPU.SetRounding (ORD (MRM.ToPlusInfinity));
    | RM.TowardZero          => EVAL FPU.SetRounding (ORD (MRM.ToZero));
    | RM.NearestElseEven     => EVAL FPU.SetRounding (ORD (MRM.ToNearest));
    ELSE RAISE Failure;
    END;
  END SetRounding;

PROCEDURE GetRounding(): RoundingMode =
  CONST Map = ARRAY MRM OF RM{ RM.NearestElseEven, RM.TowardZero,
                               RM.TowardPlusInfinity, RM.TowardMinusInfinity };
  VAR status := LOOPHOLE (FPU.GetStatus (),  FPU.ControlStatus);
  BEGIN
    RETURN Map [status.rounding_mode];
  END GetRounding;

PROCEDURE GetFlags(): SET OF Flag =
  VAR status := LOOPHOLE (FPU.GetStatus (),  FPU.ControlStatus);
  VAR state  := ThreadF.MyFPState ();
  BEGIN
    RETURN ExtractFlags (status, state^);
  END GetFlags;

PROCEDURE ExtractFlags (READONLY status: FPU.ControlStatus;
                        READONLY state: ThreadState): SET OF Flag =
  VAR flags  := NoFlags;
  BEGIN
    IF (state.behavior[Flag.Inexact] = Behavior.Ignore) THEN
      IF (state.sticky[Flag.Inexact]) THEN
        flags := flags + SET OF Flag{Flag.Inexact};
      END;
    ELSIF (status.se_inexact) THEN
      flags := flags + SET OF Flag{Flag.Inexact};
    END;

    IF (state.behavior[Flag.Underflow] = Behavior.Ignore) THEN
      IF (state.sticky[Flag.Underflow]) THEN
        flags := flags + SET OF Flag{Flag.Underflow};
      END;
    ELSIF (status.se_underflow) THEN
      flags := flags + SET OF Flag{Flag.Underflow};
    END;

    IF (state.behavior[Flag.Overflow] = Behavior.Ignore) THEN
      IF (state.sticky[Flag.Overflow]) THEN
        flags := flags + SET OF Flag{Flag.Overflow};
      END;
    ELSIF (status.se_overflow) THEN
      flags := flags + SET OF Flag{Flag.Overflow};
    END;

    IF (state.behavior[Flag.DivByZero] = Behavior.Ignore) THEN
      IF (state.sticky[Flag.DivByZero]) THEN
        flags := flags + SET OF Flag{Flag.DivByZero};
      END;
    ELSIF (status.se_divide0) THEN
      flags := flags + SET OF Flag{Flag.DivByZero};
    END;

    IF (state.behavior[Flag.Inexact] = Behavior.Ignore) THEN
      IF (state.sticky[Flag.Inexact]) THEN
        flags := flags + SET OF Flag{Flag.Invalid};
      END;
    ELSIF (status.se_invalid) THEN
      flags := flags + SET OF Flag{Flag.Invalid};
    END;

    IF (state.behavior[Flag.Inexact] = Behavior.Ignore) THEN
      IF (state.sticky[Flag.Inexact]) THEN
        flags := flags + SET OF Flag{Flag.Invalid};
      END;
    ELSIF (status.se_invalid) THEN
      flags := flags + SET OF Flag{Flag.Invalid};
    END;

    IF (state.sticky[Flag.IntOverflow]) THEN
      flags := flags + SET OF Flag{Flag.IntOverflow};
    END;

    IF (state.sticky[Flag.IntDivByZero]) THEN
      flags := flags + SET OF Flag{Flag.IntDivByZero};
    END;

    RETURN flags;
  END ExtractFlags;

PROCEDURE SetFlags(s: SET OF Flag): SET OF Flag =
  VAR status := LOOPHOLE (FPU.GetStatus (),  FPU.ControlStatus);
  VAR state  := ThreadF.MyFPState ();
  VAR flags  := ExtractFlags (status, state^);
  VAR new: FPU.ControlStatus;
  BEGIN
    (* set the FPU control register *)
    new := status;
    new.se_inexact   := (Flag.Inexact   IN s);
    new.se_underflow := (Flag.Underflow IN s);
    new.se_overflow  := (Flag.Overflow  IN s);
    new.se_divide0   := (Flag.DivByZero IN s);
    new.se_invalid   := (Flag.Invalid   IN s);
    EVAL FPU.SetStatus (LOOPHOLE (new, INTEGER));

    (* set the saved thread state *)
    FOR f := FIRST (Flag) TO LAST (Flag) DO
      state.sticky [f] := (f IN s);
    END;

    RETURN flags;
  END SetFlags;

PROCEDURE ClearFlag(f: Flag) =
  VAR status := LOOPHOLE (FPU.GetStatus (),  FPU.ControlStatus);
  VAR state  := ThreadF.MyFPState ();
  BEGIN
    CASE f OF
    | Flag.Inexact      => status.se_inexact   := FALSE;
    | Flag.Underflow    => status.se_underflow := FALSE;
    | Flag.Overflow     => status.se_overflow  := FALSE;
    | Flag.DivByZero    => status.se_divide0   := FALSE;
    | Flag.Invalid      => status.se_invalid   := FALSE;
    | Flag.IntOverflow  => (* nop *)
    | Flag.IntDivByZero => (* nop *)
    ELSE
    END;
    EVAL FPU.SetStatus (LOOPHOLE (status, INTEGER));
    state.sticky [f] := FALSE;
  END ClearFlag;

TYPE
  BHMap = ARRAY Behavior OF BOOLEAN;
CONST
  AllowedBehavior = ARRAY Flag OF BHMap {
    (*  --- flag ---           Trap    SetFlag  Ignore  *)
    (* Invalid      *) BHMap { TRUE,   TRUE,    TRUE  },
    (* Inexact      *) BHMap { TRUE,   TRUE,    TRUE  },
    (* Overflow     *) BHMap { TRUE,   TRUE,    TRUE  },
    (* Underflow    *) BHMap { TRUE,   TRUE,    TRUE  },
    (* DivByZero    *) BHMap { TRUE,   TRUE,    TRUE  },
    (* IntOverflow  *) BHMap { FALSE,  FALSE,   TRUE  },
    (* IntDivByZero *) BHMap { TRUE,   FALSE,   FALSE }
  };

PROCEDURE SetBehavior(f: Flag; b: Behavior) RAISES {Failure} =
  TYPE BH = Behavior;
  VAR status := LOOPHOLE (FPU.GetStatus (),  FPU.ControlStatus);
  VAR state  := ThreadF.MyFPState ();
  VAR old    := state.behavior [f];
  BEGIN
    IF (old = b) THEN RETURN END;
    IF NOT AllowedBehavior [f, b] THEN RAISE Failure END;
    state.behavior [f] := b;
    CASE f OF
    | Flag.Inexact =>
        IF (old = BH.Ignore) THEN
          status.se_inexact := state.sticky[Flag.Inexact];
        END;
        CASE b OF
        | BH.Ignore  => state.sticky[Flag.Inexact] := status.se_inexact;
        | BH.SetFlag => status.en_inexact := FALSE;
        | BH.Trap    => status.en_inexact := TRUE;
        END;
    | Flag.Underflow =>
        IF (old = BH.Ignore) THEN
          status.se_underflow := state.sticky[Flag.Underflow];
        END;
        CASE b OF
        | BH.Ignore  => state.sticky[Flag.Underflow] := status.se_underflow;
        | BH.SetFlag => status.en_underflow := FALSE;
        | BH.Trap    => status.en_underflow := TRUE;
        END;
    | Flag.Overflow =>
        IF (old = BH.Ignore) THEN
          status.se_overflow := state.sticky[Flag.Overflow];
        END;
        CASE b OF
        | BH.Ignore  => state.sticky[Flag.Overflow] := status.se_overflow;
        | BH.SetFlag => status.en_overflow := FALSE;
        | BH.Trap    => status.en_overflow := TRUE;
        END;
    | Flag.DivByZero =>
        IF (old = BH.Ignore) THEN
          status.se_divide0 := state.sticky[Flag.DivByZero];
        END;
        CASE b OF
        | BH.Ignore  => state.sticky[Flag.DivByZero] := status.se_divide0
        | BH.SetFlag => status.en_divide0 := FALSE;
        | BH.Trap    => status.en_divide0 := TRUE;
        END;
    | Flag.Invalid =>
        IF (old = BH.Ignore) THEN
          status.se_invalid := state.sticky[Flag.Invalid];
        END;
        CASE b OF
        | BH.Ignore  => state.sticky[Flag.Invalid] := status.se_invalid;
        | BH.SetFlag => status.en_invalid := FALSE;
        | BH.Trap    => status.en_invalid := TRUE;
        END;
    | Flag.IntOverflow  => (* only Ignore is allowed => ok *)
    | Flag.IntDivByZero => (* only Trap is allowed => ok *)
    ELSE RAISE Failure;
    END;
    EVAL FPU.SetStatus (LOOPHOLE (status, INTEGER));
  END SetBehavior;

PROCEDURE GetBehavior(f: Flag): Behavior =
  BEGIN
    RETURN ThreadF.MyFPState().behavior [f];
  END GetBehavior;

(*------------------------------------------------- thread initialization ---*)

CONST
  DefaultControl = FPU.ControlStatus {
     0, FALSE, FALSE, 0,                (* flush, condition flags *)
     FALSE,                             (* unimplemented exception *)
     FALSE, FALSE, FALSE, FALSE, FALSE, (* exception bits *)
     FALSE, FALSE, FALSE, FALSE, FALSE, (* trap enable bits *)
     FALSE, FALSE, FALSE, FALSE, FALSE, (* sticky bits *)
     FPU.RoundingMode.ToNearest
  };

CONST
  DefaultState = ThreadState {
     ARRAY Flag OF Behavior { Behavior.SetFlag, .. },
     ARRAY Flag OF BOOLEAN { FALSE, .. }
  };

PROCEDURE InitThread (VAR state: ThreadState) =
  BEGIN
    (* set the actual FPU control register *)
    EVAL FPU.SetStatus (LOOPHOLE (DefaultControl, INTEGER));

    (* initialize the saved thread state *)
    state := DefaultState;
    state.behavior [Flag.IntOverflow]  := Behavior.Ignore;
    state.behavior [Flag.IntDivByZero] := Behavior.Trap;
  END InitThread;

(*----------------------------------------- floating-point fault handling ---*)

VAR 
  new_handler,
  old_FPE_handler,
  old_TRAP_handler : Usignal.struct_sigvec;
  
PROCEDURE InstallTraps () =
  VAR i: INTEGER;
  BEGIN
    new_handler.sv_handler := LOOPHOLE (FPFaultHandler, Usignal.SignalHandler);
    new_handler.sv_mask    := Usignal.empty_sv_mask;
    new_handler.sv_flags   := 0;
    i := Usignal.sigvec (Usignal.SIGFPE, new_handler, old_FPE_handler);
    <* ASSERT  i = 0 *>
    i := Usignal.sigvec (Usignal.SIGTRAP, new_handler, old_TRAP_handler);
    <* ASSERT  i = 0 *>
  END InstallTraps;

PROCEDURE FPFaultHandler (sig: INTEGER;  code: INTEGER; 
                          scp: UNTRACED REF Usignal.struct_sigcontext)
     RAISES {Trap} =
  VAR flag: Flag; old_handler : Usignal.struct_sigvec; i: INTEGER;
  BEGIN
    IF sig = Usignal.SIGFPE AND code = 0 THEN
      (* floating point trap *)
      VAR
        status := LOOPHOLE (scp.sc_fpc_csr, FPU.ControlStatus);
      BEGIN
        (* inexact should be tested first, because other flags have
           precedence *)
        IF status.ex_inexact THEN   flag := Flag.Inexact; END;
        IF status.ex_underflow THEN flag := Flag.Underflow; END;
        IF status.ex_overflow THEN  flag := Flag.Overflow; END;
        IF status.ex_divide0 THEN   flag := Flag.DivByZero; END;
        IF status.ex_invalid THEN   flag := Flag.Invalid; END;

        status.ex_inexact := FALSE;
        status.ex_underflow := FALSE;
        status.ex_overflow := FALSE;
        status.ex_divide0 := FALSE;
        status.ex_invalid := FALSE;

        EVAL FPU.SetStatus (LOOPHOLE (status, INTEGER));

        (* enable the exception *)
        i := Usignal.sigsetmask (0);
        i := Word.And (i, Word.Not (Usignal.sigmask (Usignal.SIGFPE)));
        EVAL Usignal.sigsetmask (i); 

        RAISE Trap (flag);
      END;

    ELSIF sig = Usignal.SIGTRAP AND code = Usignal.BRK_DIVZERO THEN
      i := Usignal.sigsetmask (0);
      i := Word.And (i, Word.Not (Usignal.sigmask (Usignal.SIGTRAP)));
      EVAL Usignal.sigsetmask (i); 
      RAISE Trap (Flag.IntDivByZero); 

    ELSIF sig = Usignal.SIGFPE THEN
      old_handler := old_FPE_handler;
    ELSIF sig = Usignal.SIGTRAP THEN
      old_handler := old_TRAP_handler;
    ELSE
      Die ("unrecognized arithmetic trap!?");
    END;


    (* if we got here, the fault is unhandled => resignal to the old handler *)
    VAR p := old_handler.sv_handler; BEGIN
      IF (p = Usignal.SIG_IGN) THEN
        (* ignore *)
      ELSIF (p = Usignal.SIG_DFL) THEN
        (* default => crash *)
        Die ("unhandled arithmetic trap");
      ELSE (* call the old handler *)
        p (sig, code, scp);
      END;
    END;
  END FPFaultHandler;

PROCEDURE Die (msg: TEXT) =
  BEGIN
    RTMisc.FatalError (NIL, 0, msg);
    <*ASSERT FALSE*>
  END Die;

BEGIN
  InstallTraps ();
END FloatMode.
