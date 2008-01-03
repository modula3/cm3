(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Wed Mar  8 18:34:37 PST 1995 by msm                      *)
(*      modified on Tue Jan 31 09:41:23 PST 1995 by kalsow                   *)
(*      modified on Fri Feb 28 19:23:28 1992 by guarino                  *)
(*      modified on Mon Feb 24 13:54:01 PST 1992 by muller                   *)

MODULE ProperSplit;

IMPORT VBT, VBTClass, Split, VBTRep;

REVEAL T = Public BRANDED OBJECT
  OVERRIDES
    succ := Succ; 
    pred := Pred; 
    nth := Nth; 
    index := Index;
    beChild := BeChild;
    replace := ReplaceDefault;
    insert := InsertDefault;
    move := MoveDefault;
  END;

PROCEDURE InsertDefault (v: T; pred, ch: VBT.T) =
  <*FATAL Split.NotAChild*>
  VAR predCh := PreInsert(v, pred, ch);
  BEGIN
    LOCK ch DO LOCK v DO Insert(v, predCh, ch) END END
  END InsertDefault;

PROCEDURE MoveDefault (v: T; pred, ch: VBT.T) =
  VAR predCh: Child;
  BEGIN
    IF pred # NIL THEN predCh := pred.upRef ELSE predCh := NIL END;
    LOCK v DO Move(v, predCh, ch.upRef) END
  END MoveDefault;
  
PROCEDURE ReplaceDefault (v: T; ch, new: VBT.T) RAISES {} =
  BEGIN
    IF new # NIL THEN InsertDefault(v, ch, new) END;
    Delete(v, ch.upRef)
  END ReplaceDefault;
  
PROCEDURE PreInsert(v: T; pred, ch: VBT.T): Child 
  RAISES {Split.NotAChild} =
  VAR predCh: Child;
  BEGIN
    IF ch.parent # NIL THEN Crash() END;
    IF pred # NIL THEN 
      IF pred.parent # v THEN RAISE Split.NotAChild END;
      predCh := pred.upRef
    ELSE
      predCh := NIL
    END;
    IF v.st # ch.st THEN VBTClass.Rescreen(ch, v.st) END;
    RETURN predCh
  END PreInsert;

PROCEDURE BeChild(v: VBT.Split; ch: VBT.T) RAISES {} =
  BEGIN
    IF ch.upRef = NIL THEN ch.upRef := NEW(Child) END;
    NARROW(ch.upRef, Child).ch := ch;
    VBT.Split.beChild(v, ch)
  END BeChild;

PROCEDURE Succ(v: T; ch: VBT.T): VBT.T RAISES {} =
  BEGIN
    IF ch = NIL THEN
      IF v.lastChild = NIL THEN 
        RETURN NIL 
      ELSE 
        RETURN v.lastChild.succ.ch 
      END
    ELSE
      WITH ur = NARROW(ch.upRef, Child) DO
        IF ur = v.lastChild THEN 
          RETURN NIL
        ELSE
          RETURN ur.succ.ch
        END
      END
    END
  END Succ;

PROCEDURE Pred(v: T; ch: VBT.T): VBT.T RAISES {} =
  BEGIN
    IF ch = NIL THEN
      IF v.lastChild = NIL THEN 
        RETURN NIL 
      ELSE 
        RETURN v.lastChild.ch 
      END
    ELSE
      WITH ur = NARROW(ch.upRef, Child) DO
        IF ur.pred = NIL THEN RETURN NIL ELSE RETURN ur.pred.ch END
      END
    END
  END Pred;

PROCEDURE Nth(v: T; n: CARDINAL): VBT.T RAISES {} =
  VAR ur, lc := v.lastChild;   
  BEGIN
    IF ur = NIL THEN RETURN NIL END;
    ur := ur.succ;
    WHILE (ur # lc) AND (n # 0) DO
      DEC(n); ur := ur.succ
    END;
    IF n = 0 THEN RETURN ur.ch ELSE RETURN NIL END
  END Nth;

PROCEDURE Index(v: T; ch: VBT.T): CARDINAL RAISES {} =
  VAR res := 0; ur := v.lastChild;
  BEGIN
    IF ch = NIL THEN
      WHILE ur # NIL DO INC(res); ur := ur.pred END
    ELSE
      ur := ur.succ;
      WHILE ur.ch # ch DO INC(res); ur := ur.succ END
    END;
    RETURN res
  END Index;

PROCEDURE Insert(v: T; pred: Child; newch: VBT.T) RAISES {} =
  BEGIN
    v.beChild(newch);
    InsertInternal(v, pred, newch.upRef)
  END Insert;

PROCEDURE InsertInternal(v: T; pred, ur: Child) RAISES {} =
  VAR insertLast := (pred = v.lastChild);
  BEGIN
    ur.pred := pred;
    IF pred = NIL THEN pred := v.lastChild END;
    IF pred = NIL THEN
      ur.succ := ur
    ELSE
      ur.succ := pred.succ;
      pred.succ := ur
    END;
    IF insertLast THEN v.lastChild := ur ELSE ur.succ.pred := ur END
  END InsertInternal;

PROCEDURE Move(v: T; pred, ch: Child) RAISES {} =
  BEGIN
    IF pred = ch THEN Crash() END;
    IF ch.pred # pred THEN 
      VBTRep.Mark(v);
      DeleteInternal(v, ch);
      InsertInternal(v, pred, ch)
    END
  END Move;

PROCEDURE Delete(v: T; ch: Child) RAISES {} =
  BEGIN
    VBT.Mark(v);
    LOCK v DO
      DeleteInternal(v, ch);
      ch.pred := NIL;
      ch.succ := NIL
    END;
    VBTClass.Detach(ch.ch)
  END Delete;

PROCEDURE DeleteInternal(v: T; ch: Child) RAISES {} =
  BEGIN
    IF ch.pred = NIL THEN 
      v.lastChild.succ := ch.succ 
    ELSE 
      ch.pred.succ := ch.succ 
    END;
    IF v.lastChild = ch THEN
      v.lastChild := ch.pred 
    ELSE 
      ch.succ.pred := ch.pred
    END;
  END DeleteInternal;

EXCEPTION FatalError;

PROCEDURE Crash () =
  <* FATAL FatalError *>
  BEGIN
    RAISE FatalError;
  END Crash;

BEGIN END ProperSplit.
