(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Jan 29 15:29:36 PST 1993 by mhb    *)
(*      modified on Fri Jul 31 18:05:47 PDT 1992 by meehan *)
(*      modified on Tue Jun 16 13:08:55 PDT 1992 by muller *)

MODULE FeedbackVBT;

IMPORT Filter, VBT;

TYPE
  LastCall = {Normal, Excited};

REVEAL
  T = Public BRANDED OBJECT
        last  := LastCall.Normal;
        state := FALSE;
      OVERRIDES
        init    := Init;
        normal  := Noop;
        excited := Noop;
        redisplay := Redisplay;
      END;

PROCEDURE Init (v: T; ch: VBT.T): T =
  BEGIN
    RETURN Filter.T.init(v, ch);
  END Init;

PROCEDURE Noop (<* UNUSED *> v: T) =
  BEGIN
  END Noop;

PROCEDURE Redisplay (v: T) =
  BEGIN
    SetState(v, GetState(v))
  END Redisplay;

PROCEDURE SetState (v: T; state: BOOLEAN) =
  BEGIN
    IF v.state # state THEN
      v.state := state;
      IF v.last = LastCall.Normal THEN
        v.normal()
      ELSE
        v.excited()
      END
    END
  END SetState;

PROCEDURE GetState (v: T): BOOLEAN =
  BEGIN
    RETURN v.state
  END GetState;

PROCEDURE Normal (v: T) =
  BEGIN
    v.last := LastCall.Normal;
    v.normal()
  END Normal;

PROCEDURE Excited (v: T) =
  BEGIN
    v.last := LastCall.Excited;
    v.excited()
  END Excited;

BEGIN
END FeedbackVBT.
