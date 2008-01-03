(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Sat Sep 30 11:56:12 PDT 1995 by mhb                      *)
(*      modified on Tue Jun 16 20:39:55 PDT 1992 by muller                   *)
(*      modified on Mon Jun  8  1:58:39 PDT 1992 by meehan                   *)
(*      modified on Fri Mar 27 02:32:06 1992 by steveg                       *)

MODULE MultiSplit;

IMPORT MultiClass, Point, Split, VBT;

EXCEPTION FatalError; <* FATAL FatalError *>

PROCEDURE Delete (v: VBT.T; ch: VBT.T) RAISES {NotAChild} =
  BEGIN
    Replace(v, ch, NIL)
  END Delete;

PROCEDURE Replace (v: T; ch, new: VBT.T) RAISES {NotAChild} =
  BEGIN
    WITH m = MultiClass.Resolve(v) DO
      IF m = NIL THEN
        TRY
          Split.Replace(v, ch, new)
        EXCEPT
        | Split.NotAChild => RAISE NotAChild
        END
      ELSE
        IF (ch # NIL) AND (NOT MultiClass.IsChild(v, ch)) THEN
          RAISE NotAChild
        END;
        IF new # NIL THEN MultiClass.BeChild(v, new); END;
        m.replace(ch, new);
        IF ch # NIL THEN MultiClass.UnChild(v, ch) END;
      END
    END
  END Replace;

PROCEDURE Succ (v: VBT.T; ch: VBT.T): VBT.T RAISES {NotAChild} =
  BEGIN
    WITH m = MultiClass.Resolve(v) DO
      IF m = NIL THEN
        TRY
          RETURN Split.Succ(v, ch)
        EXCEPT
        | Split.NotAChild => RAISE NotAChild
        END
      ELSE
        IF (ch # NIL) AND (NOT MultiClass.IsChild(v, ch)) THEN
          RAISE NotAChild
        END;
        RETURN m.succ(ch);
      END
    END
  END Succ;

PROCEDURE Pred (v: VBT.T; ch: VBT.T): VBT.T RAISES {NotAChild} =
  BEGIN
    WITH m = MultiClass.Resolve(v) DO
      IF m = NIL THEN
        TRY
          RETURN Split.Pred(v, ch)
        EXCEPT
        | Split.NotAChild => RAISE NotAChild
        END
      ELSE
        IF (ch # NIL) AND (NOT MultiClass.IsChild(v, ch)) THEN
          RAISE NotAChild
        END;
        RETURN m.pred(ch);
      END
    END
  END Pred;

PROCEDURE Nth (v: VBT.T; n: CARDINAL): VBT.T =
  BEGIN
    WITH m = MultiClass.Resolve(v) DO
      IF m = NIL THEN RETURN Split.Nth(v, n) ELSE RETURN m.nth(n) END
    END
  END Nth;

PROCEDURE NumChildren (v: VBT.T): CARDINAL RAISES {NotAChild} =
  BEGIN
    RETURN Index (v, NIL)
  END NumChildren;

PROCEDURE Index (v: VBT.T; ch: VBT.T): CARDINAL RAISES {NotAChild} =
  BEGIN
    WITH m = MultiClass.Resolve(v) DO
      IF m = NIL THEN
        TRY
          RETURN Split.Index(v, ch)
        EXCEPT
        | Split.NotAChild => RAISE NotAChild
        END
      ELSE
        IF (ch # NIL) AND (NOT MultiClass.IsChild(v, ch)) THEN
          RAISE NotAChild
        END;
        RETURN m.index(ch);
      END
    END
  END Index;

PROCEDURE Locate (v: VBT.T; READONLY pt: Point.T): VBT.T =
  VAR ch := Split.Locate(v, pt);
  BEGIN
    WHILE ch # NIL DO
      IF MultiClass.IsChild(v, ch) THEN RETURN ch END;
      ch := Split.Locate(ch, pt);
    END;
    RETURN NIL;
  END Locate;

PROCEDURE Move (v: T; pred, ch: VBT.T) RAISES {NotAChild} =
  BEGIN
    WITH m = MultiClass.Resolve(v) DO
      IF m = NIL THEN
        TRY
          Split.Move(v, pred, ch)
        EXCEPT
        | Split.NotAChild => RAISE NotAChild
        END
      ELSE
        IF ch = NIL THEN RAISE FatalError END;
        IF NOT MultiClass.IsChild(v, ch) THEN RAISE NotAChild END;
        IF (pred # NIL) AND (NOT MultiClass.IsChild(v, pred)) THEN
          RAISE NotAChild
        END;
        IF pred # ch AND m.succ(pred) # ch THEN m.move(pred, ch) END
      END
    END
  END Move;

PROCEDURE Insert (v: T; pred, new: VBT.T) RAISES {NotAChild} =
  BEGIN
    IF new = NIL THEN RAISE FatalError END;
    WITH m = MultiClass.Resolve(v) DO
      IF m = NIL THEN
        TRY
          Split.Insert(v, pred, new)
        EXCEPT
        | Split.NotAChild => RAISE NotAChild
        END
      ELSIF pred # NIL AND NOT MultiClass.IsChild(v, pred) THEN
        RAISE NotAChild
      ELSE
        MultiClass.BeChild(v, new);
        m.insert(pred, new);
      END
    END
  END Insert;

PROCEDURE AddChildArray (v: T; READONLY new: ARRAY OF VBT.T) =
  <* FATAL NotAChild *> (* Can't happen, since we start at the top. *)
  VAR pred: VBT.T;
  BEGIN
    pred := Pred (v, NIL);
    FOR i := 0 TO LAST (new) DO
      IF new [i] # NIL THEN Insert (v, pred, new [i]); pred := new [i] END
    END
  END AddChildArray;

PROCEDURE AddChild (v: T;
                    new0, new1, new2, new3, new4, new5, new6, new7, new8,
                      new9: VBT.T := NIL) =
  VAR a: ARRAY [0 .. 9] OF VBT.T;
  BEGIN
    a[0] := new0;
    a[1] := new1;
    a[2] := new2;
    a[3] := new3;
    a[4] := new4;
    a[5] := new5;
    a[6] := new6;
    a[7] := new7;
    a[8] := new8;
    a[9] := new9;
    AddChildArray(v, a)
  END AddChild;

BEGIN
END MultiSplit.
















