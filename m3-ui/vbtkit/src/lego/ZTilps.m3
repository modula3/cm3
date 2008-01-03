(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Mar  5 20:23:05 PST 1993 by meehan *)
(*      modified on Thu Jan 28 14:14:53 PST 1993 by mhb    *)
(*      modified on Tue Jun 16 13:07:54 PDT 1992 by muller *)

MODULE ZTilps;

IMPORT MultiClass, Rect, Split, VBT, ZChildVBT, ZSplit;

REVEAL
  T = Public BRANDED OBJECT
           OVERRIDES
             init := Init;
           END;

TYPE
  MC = MultiClass.Split BRANDED OBJECT
       OVERRIDES
         insert  := Insert;
         replace := Replace;
         succ    := Succ;
         pred    := Pred;
       END;

PROCEDURE Init (v         : T;
                saveBits        := FALSE;
                parlim          := -1): T =
  BEGIN
    EVAL ZSplit.T.init (v, NIL, saveBits, parlim);
    MultiClass.Be (v, NEW(MC));
    RETURN v
  END Init;

PROCEDURE Insert (m: MC; pred, ch: VBT.T) =
  <* FATAL Split.NotAChild *>
  BEGIN
    IF pred = NIL THEN
      ZSplit.InsertAt (
        m.vbt, ch, Rect.NorthWest (VBT.Domain (m.vbt)),
        ZSplit.Altitude.Bot);
      ZSplit.SetReshapeControl (ch, ZSplit.Background)
    ELSE
      ZSplit.InsertAfter (
        m.vbt, Split.Pred (m.vbt, pred), ch, Rect.Empty,
        ZChildVBT.InitiallyMapped (ch));
      ZChildVBT.Inserted (ch)
    END
  END Insert;

PROCEDURE Replace (m: MC; ch, new: VBT.T) = 
  <* FATAL Split.NotAChild *>
  BEGIN
    Split.Replace (m.vbt, ch, new)
  END Replace;

PROCEDURE Succ (m: MC; ch: VBT.T): VBT.T =
  <* FATAL Split.NotAChild *>
  BEGIN
    RETURN Split.Pred (m.vbt, ch)
  END Succ;

PROCEDURE Pred (m: MC; ch: VBT.T): VBT.T =
  <* FATAL Split.NotAChild *>
  BEGIN
    RETURN Split.Succ (m.vbt, ch)
  END Pred;

BEGIN
END ZTilps.



