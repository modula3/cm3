(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Jan 31 09:45:35 PST 1995 by kalsow     *)
(*      modified on Thu Jan 28 10:51:10 PST 1993 by msm        *)
(*      modified on Mon Feb 24 13:54:31 PST 1992 by muller     *)
(*      modified on Sat Nov  2 15:42:15 PST 1991 by gnelson    *)
(*      modified on Fri Sep  7 16:19:48 PDT 1990 by steveg     *)
<*PRAGMA LL*>

MODULE Split;

IMPORT VBT, VBTClass, Point, Rect;

PROCEDURE Delete(v: T; ch: VBT.T) RAISES {NotAChild} =
  BEGIN
    Replace(v, ch, NIL)
  END Delete;

PROCEDURE Replace(v: T; ch, new: VBT.T) RAISES {NotAChild} =
  BEGIN
    IF ch = NIL OR ch.parent # v THEN RAISE NotAChild END;
    IF new # NIL AND new.parent # NIL THEN Crash() END;
    IF new # NIL AND new.st # v.st THEN VBTClass.Rescreen(new, v.st) END;
    v.replace(ch, new);
    VBT.Mark(v)
  END Replace;

PROCEDURE Insert(v: T; pred, new: VBT.T) RAISES {NotAChild} =
  BEGIN
    IF pred # NIL AND pred.parent # v THEN RAISE NotAChild END;
    IF new = NIL OR new.parent # NIL THEN Crash() END;
    IF new.st # v.st THEN VBTClass.Rescreen(new, v.st) END;
    v.insert(pred, new);
    VBT.NewShape(v);
    VBT.Mark(v)
  END Insert;

PROCEDURE Move(v: T; pred, ch: VBT.T) RAISES {NotAChild} =
  BEGIN
    IF ch = NIL OR ch.parent # v THEN RAISE NotAChild END;
    IF pred # NIL AND pred.parent # v THEN RAISE NotAChild END;
    IF pred = ch OR v.succ(pred) = ch THEN RETURN END;
    v.move(pred, ch);
    VBT.Mark(v)
  END Move;

PROCEDURE AddChildArray(v: T; READONLY new: ARRAY OF VBT.T) =
  <*FATAL NotAChild*>
  VAR pred := Pred(v, NIL); BEGIN
    FOR i := 0 TO LAST(new) DO
      IF new[i] # NIL THEN
        Insert(v, pred, new[i]);
        pred := new[i]
      END
    END
  END AddChildArray;

PROCEDURE AddChild(v: T;
  new0, new1, new2, new3, new4, new5, new6, new7, new8, new9: VBT.T := NIL) =
  BEGIN 
    AddChildArray(v, ARRAY OF VBT.T{new0, new1, new2, new3, new4, 
      new5, new6, new7, new8, new9}) 
  END AddChild;

PROCEDURE Succ(v: T; ch: VBT.T): VBT.T RAISES {NotAChild} =
  BEGIN
    IF ch # NIL AND ch.parent # v THEN RAISE NotAChild END;
    RETURN v.succ(ch)
  END Succ;

PROCEDURE Pred(v: T; ch: VBT.T): VBT.T RAISES {NotAChild} =
  BEGIN
    IF ch # NIL AND ch.parent # v THEN RAISE NotAChild END;
    RETURN v.pred(ch)
  END Pred;

PROCEDURE Nth(v: T; n: CARDINAL): VBT.T RAISES {} =
  BEGIN
    RETURN v.nth(n)
  END Nth;

PROCEDURE NumChildren(v: T): CARDINAL RAISES {} =
  BEGIN 
    RETURN v.index(NIL) 
  END NumChildren;

PROCEDURE Index(v: T; ch: VBT.T): CARDINAL RAISES {NotAChild} =
  BEGIN
    IF ch # NIL AND ch.parent # v THEN RAISE NotAChild END;
    RETURN v.index(ch) 
  END Index;

PROCEDURE Locate(v: T; READONLY pt: Point.T): VBT.T RAISES {} =
  VAR r: Rect.T;
  BEGIN
    RETURN v.locate(pt, r)
  END Locate;

EXCEPTION FatalError;

PROCEDURE Crash() =
  <*FATAL FatalError*>
  BEGIN
    RAISE FatalError
  END Crash;

BEGIN END Split.
