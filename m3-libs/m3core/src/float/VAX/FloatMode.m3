(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Fri Jan 27 13:14:24 PST 1995 by kalsow     *)
(*      modified on Thu May 13 09:08:42 PDT 1993 by mcjones    *)
(*      modified on Wed Nov 27 17:06:11 PST 1991 by muller     *)


MODULE FloatMode (* FOR VAX *);

IMPORT ThreadF;

PROCEDURE SetRounding(md: RoundingMode) RAISES {Failure} =
  BEGIN
    IF (md # RoundDefault) THEN RAISE Failure END;
  END SetRounding;

PROCEDURE GetRounding(): RoundingMode =
  BEGIN
    RETURN RoundDefault;
  END GetRounding;

PROCEDURE GetFlags(): SET OF Flag =
  VAR flags: SET OF Flag;
  PROCEDURE Get(READONLY state: ThreadState) =
    BEGIN
      flags := state.sticky;
    END Get;
  BEGIN
    ThreadF.GetMyFPState(Get);
    RETURN flags;
  END GetFlags;

PROCEDURE SetFlags(s: SET OF Flag): SET OF Flag =
  VAR old: SET OF Flag;
  PROCEDURE Set(VAR state: ThreadState) =
    BEGIN
      old := state.sticky;
      state.sticky := s;
    END Set;
  BEGIN
    RETURN old;
  END SetFlags;

PROCEDURE ClearFlag(f: Flag) =
  PROCEDURE Set(VAR state: ThreadState) =
    BEGIN
      state.sticky := state.sticky - SET OF Flag {f};
    END Set;
  BEGIN
    ThreadF.SetMyFPState(Set);
  END ClearFlag;

TYPE
  BHMap = ARRAY Behavior OF BOOLEAN;
CONST
  AllowedBehavior = ARRAY Flag OF BHMap {
    (*  --- flag ---           Trap    SetFlag  Ignore  *)
    (* Invalid      *) BHMap { FALSE,  FALSE,   TRUE  },
    (* Inexact      *) BHMap { FALSE,  FALSE,   TRUE  },
    (* Overflow     *) BHMap { FALSE,  FALSE,   TRUE  },
    (* Underflow    *) BHMap { FALSE,  FALSE,   TRUE  },
    (* DivByZero    *) BHMap { FALSE,  FALSE,   TRUE  },
    (* IntOverflow  *) BHMap { FALSE,  FALSE,   TRUE  },
    (* IntDivByZero *) BHMap { FALSE,  FALSE,   TRUE  }
  };

PROCEDURE SetBehavior(f: Flag; b: Behavior) RAISES {Failure} =
  PROCEDURE Set(VAR state: ThreadState) =
    BEGIN
      IF (state.behavior [f] = b) THEN RETURN END;
      IF NOT AllowedBehavior [f, b] THEN RAISE Failure END;
      state.behavior [f] := b;
    END Set;
  BEGIN
    ThreadF.SetMyFPState(Set);
  END SetBehavior;

PROCEDURE GetBehavior(f: Flag): Behavior =
  VAR behavior: Behavior;
  PROCEDURE Get(READONLY state: ThreadState) =
    BEGIN
      behavior := state.behavior [f];
    END Get;
  BEGIN
    RETURN behavior;
  END GetBehavior;

(*------------------------------------------------- thread initialization ---*)

CONST
  DefaultState = ThreadState {
     ARRAY Flag OF Behavior { Behavior.Ignore, .. },
     NoFlags
  };

PROCEDURE InitThread (VAR state: ThreadState) =
  BEGIN
    state := DefaultState;
  END InitThread;

BEGIN
END FloatMode.
