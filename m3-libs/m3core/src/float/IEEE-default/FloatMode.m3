(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Fri Jan 27 11:53:11 PST 1995 by kalsow     *)
(*      modified on Thu May 13 09:11:04 PDT 1993 by mcjones    *)
(*      modified on Fri May  7 14:30:09 PDT 1993 by muller     *)


MODULE FloatMode;

PROCEDURE SetRounding(<*UNUSED*> md: RoundingMode) =
  BEGIN
    <*ASSERT FALSE, "FloatMode.SetRounding not implemented" *>
  END SetRounding;

PROCEDURE GetRounding(): RoundingMode =
  BEGIN
    RETURN RoundingMode.NearestElseEven;
    (***  <*ASSERT FALSE, "FloatMode.GetRounding not implemented" *> ***)
  END GetRounding;

PROCEDURE GetFlags(): SET OF Flag =
  BEGIN
    <*ASSERT FALSE, "FloatMode.GetFlags not implemented" *>
  END GetFlags;

PROCEDURE SetFlags(<*UNUSED*> s: SET OF Flag): SET OF Flag =
  BEGIN
    <*ASSERT FALSE, "FloatMode.SetFlags not implemented" *>
  END SetFlags;

PROCEDURE ClearFlag(<*UNUSED*> f: Flag) =
  BEGIN
    <*ASSERT FALSE, "FloatMode.ClearFlag not implemented" *>
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

BEGIN
END FloatMode.
