(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman, Mark Manasse and Greg Nelson           *)
(* Last modified on Tue Jan 31 09:42:33 PST 1995 by kalsow   *)
(*      modified on Tue Mar 10 19:08:41 1992 by steveg   *)
(*      modified on Mon Feb 24 13:53:14 PST 1992 by muller   *)
(*      modified on Sun Nov 10 17:57:39 PST 1991 by gnelson  *)
(*      modified on Wed Sep 11 15:31:06 PDT 1991 by msm      *)
<*PRAGMA LL*>

MODULE Filter;

IMPORT VBT, FilterClass, VBTClass;

PROCEDURE Child(v: T): VBT.T RAISES {} =
  BEGIN
    RETURN v.ch
  END Child;

PROCEDURE Replace(v: T; new: VBT.T): VBT.T =
  VAR res := v.ch;
  BEGIN
    IF new # NIL AND new.parent # NIL THEN Crash() END;
    IF new = NIL AND res = NIL THEN RETURN NIL END;
    IF new # NIL THEN
      IF new.st # v.st THEN VBTClass.Rescreen(new, v.st) END
    END;
    v.replace(res, new);
    VBT.Mark(v);
    VBT.NewShape(v);
    RETURN res
  END Replace;

EXCEPTION FatalError;

PROCEDURE Crash () =
  <* FATAL FatalError *>
  BEGIN
    RAISE FatalError;
  END Crash;

BEGIN END Filter.
