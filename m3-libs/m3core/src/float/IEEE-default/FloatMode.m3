(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Fri Jan 27 11:53:11 PST 1995 by kalsow     *)
(*      modified on Thu May 13 09:11:04 PDT 1993 by mcjones    *)
(*      modified on Fri May  7 14:30:09 PDT 1993 by muller     *)


MODULE FloatMode;

IMPORT RTMisc;

PROCEDURE SetRounding(<*UNUSED*> md: RoundingMode) =
  BEGIN
    Die ("FloatMode.SetRounding not implemented");
  END SetRounding;

PROCEDURE GetRounding(): RoundingMode =
  BEGIN
    RETURN RoundingMode.NearestElseEven;
    (***  Die ("FloatMode.GetRounding not implemented"); ***)
    (*** <*ASSERT FALSE*> ***)
  END GetRounding;

PROCEDURE GetFlags(): SET OF Flag =
  BEGIN
    Die ("FloatMode.GetFlags not implemented");
    <*ASSERT FALSE*>
  END GetFlags;

PROCEDURE SetFlags(<*UNUSED*> s: SET OF Flag): SET OF Flag =
  BEGIN
    Die ("FloatMode.SetFlags not implemented");
    <*ASSERT FALSE*>
  END SetFlags;

PROCEDURE ClearFlag(<*UNUSED*> f: Flag) =
  BEGIN
    Die ("FloatMode.ClearFlag not implemented");
  END ClearFlag;

PROCEDURE SetBehavior(<*UNUSED*> f: Flag; <*UNUSED*> b: Behavior) =
  BEGIN
    Die ("FloatMode.SetBehavior not implemented");
  END SetBehavior;

PROCEDURE GetBehavior(<*UNUSED*> f: Flag): Behavior =
  BEGIN
    Die ("FloatMode.GetBehavior not implemented");
    <*ASSERT FALSE*>
  END GetBehavior;

(*------------------------------------------------- thread initialization ---*)

PROCEDURE InitThread (<*UNUSED*> VAR state: ThreadState) =
  BEGIN
  END InitThread;

(*----------------------------------------- floating-point fault handling ---*)

PROCEDURE Die (msg: TEXT) =
  BEGIN
    RTMisc.FatalError (NIL, 0, msg);
    <*ASSERT FALSE*>
  END Die;

BEGIN
END FloatMode.
