(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Jul 21 16:22:42 PDT 1994 by mhb      *)
(*      modified on Fri May 14 17:02:08 PDT 1993 by meehan   *)
(*      modified on Tue Jun 16 13:08:34 PDT 1992 by muller   *)


MODULE MultiClass;

IMPORT RefList, MultiSplit, VBT;
IMPORT Split AS TrestleSplit;

EXCEPTION FatalError; <* FATAL FatalError *>

REVEAL
  Split = T BRANDED OBJECT
      OVERRIDES
(*
        replace := NIL;
        insert  := NIL;
*)
        move    := MoveDefault;
        succ    := SuccDefault;
        pred    := PredDefault;
        nth     := NthDefault;
        index   := IndexDefault;
      END;

TYPE
  Prop = REF RECORD t: T END;
  ChProp = REF RECORD parents: RefList.T (* of VBT.T *) END;

PROCEDURE Be (vbt: VBT.T; t: T) =
  BEGIN
    t.vbt := vbt;
    VBT.PutProp (vbt, NEW (Prop, t := t))
  END Be;

PROCEDURE Resolve (vbt: VBT.T): T =
  VAR prop: Prop := VBT.GetProp (vbt, TYPECODE (Prop));
  BEGIN
    IF prop = NIL THEN RETURN NIL ELSE RETURN prop.t END
  END Resolve;

PROCEDURE BeChild (vbt: VBT.T; ch: VBT.T) =
  VAR chProp: ChProp := VBT.GetProp (ch, TYPECODE (ChProp));
  BEGIN
    IF chProp = NIL THEN
      chProp := NEW (ChProp);
      VBT.PutProp (ch, chProp);
    END;
    chProp.parents := RefList.Cons (vbt, chProp.parents);
  END BeChild;

PROCEDURE UnChild (vbt: VBT.T; ch: VBT.T) =
  VAR chProp: ChProp := VBT.GetProp (ch, TYPECODE (ChProp));
  PROCEDURE remove (VAR list: RefList.T) =
    BEGIN
      IF list = NIL THEN         (* skip *)
      ELSIF list.head = vbt THEN
        list := list.tail
      ELSE
        remove (list.tail)
      END
    END remove;
  BEGIN
    remove (chProp.parents)
  END UnChild;

PROCEDURE IsChild (vbt: VBT.T; ch: VBT.T): BOOLEAN =
  BEGIN
    RETURN RefList.Member(Parents(ch), vbt)
  END IsChild;

PROCEDURE Parents (ch: VBT.T): RefList.T =
  VAR chProp: ChProp := VBT.GetProp (ch, TYPECODE (ChProp));
  BEGIN
      IF chProp = NIL THEN RETURN NIL ELSE RETURN chProp.parents END
  END Parents;

PROCEDURE MoveDefault (t: Split; pred, ch: VBT.T) =
  <* FATAL MultiSplit.NotAChild *>
  BEGIN
    MultiSplit.Delete (t.vbt, ch);
    MultiSplit.Insert (t.vbt, pred, ch);
  END MoveDefault;

PROCEDURE SuccDefault (t: Split; ch: VBT.T): VBT.T =
  <* FATAL TrestleSplit.NotAChild *>
  VAR chP, oldP, p: VBT.T;
  BEGIN
    chP := ch;
    IF chP = NIL THEN p := t.vbt ELSE p := VBT.Parent(chP) END;
    REPEAT
      chP := TrestleSplit.Succ(p, chP);
      WHILE (chP = NIL) AND (p # t.vbt) DO
        oldP := p;
        p := VBT.Parent(p);
        chP := TrestleSplit.Succ(p, oldP);
      END;
      WHILE (chP # NIL) AND (NOT IsChild(t.vbt, chP))
              AND HasChild(chP) DO
        p := chP;
        chP := TrestleSplit.Succ(p, NIL);
      END;
    UNTIL (chP = NIL) OR IsChild(t.vbt, chP);
    RETURN chP
  END SuccDefault;

PROCEDURE HasChild (v: VBT.T): BOOLEAN =
  <* FATAL TrestleSplit.NotAChild *>
  BEGIN
    RETURN ISTYPE (v, VBT.Split) AND TrestleSplit.Succ(v, NIL) # NIL
(*
     WITH t = Resolve(v) DO
      RETURN (t # NIL) AND (t.succ(NIL) # NIL)
    END
*)
  END HasChild;

PROCEDURE PredDefault (t: Split; ch: VBT.T): VBT.T =
  VAR next, res: VBT.T;
  BEGIN
    next := t.succ(NIL);
    WHILE next # NIL AND next # ch DO
      res := next;
      next := t.succ(res)
    END;
    IF next = ch THEN RETURN res ELSE RAISE FatalError END
  END PredDefault;

PROCEDURE NthDefault (t: Split; n: CARDINAL): VBT.T =
  VAR ch: VBT.T;
  BEGIN
    ch := t.succ(NIL);
    WHILE ch # NIL AND n # 0 DO DEC(n); ch := t.succ(ch) END;
    RETURN ch
  END NthDefault;

PROCEDURE IndexDefault (t: Split; ch: VBT.T): CARDINAL =
  VAR res := 0; chP := t.succ (NIL);
  BEGIN
    WHILE chP # ch AND chP # NIL DO INC (res); chP := t.succ (chP) END;
    IF chP = ch THEN RETURN res ELSE RAISE FatalError END
  END IndexDefault;

REVEAL Filter =
    Split BRANDED OBJECT OVERRIDES insert := FilterInsert; END;

PROCEDURE FilterInsert (t: Filter; pred, new: VBT.T) =
  BEGIN
    t.replace(t.succ(pred), new);
  END FilterInsert;

BEGIN
END MultiClass.
