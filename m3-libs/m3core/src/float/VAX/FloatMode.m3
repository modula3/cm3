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
  VAR state := ThreadF.MyFPState ();
  BEGIN
    RETURN state.sticky;
  END GetFlags;

PROCEDURE SetFlags(s: SET OF Flag): SET OF Flag =
  VAR state := ThreadF.MyFPState ();
  VAR old := state.sticky;
  BEGIN
    state.sticky := s;
    RETURN old;
  END SetFlags;

PROCEDURE ClearFlag(f: Flag) =
  VAR state := ThreadF.MyFPState ();
  BEGIN
    state.sticky := state.sticky - SET OF Flag {f};
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
  VAR state := ThreadF.MyFPState ();
  BEGIN
    IF (state.behavior [f] = b) THEN RETURN END;
    IF NOT AllowedBehavior [f, b] THEN RAISE Failure END;
    state.behavior [f] := b;
  END SetBehavior;

PROCEDURE GetBehavior(f: Flag): Behavior =
  BEGIN
    RETURN ThreadF.MyFPState().behavior [f];
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
