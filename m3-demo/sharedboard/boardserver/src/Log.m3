(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)

MODULE Log;

IMPORT SmallDB, OSError, Pickle,
       AtomicItemTbl, NotifyRec, Item, BoardX;

TYPE UpdateRec = REF RECORD
    code: NotifyRec.Code;
    its: Item.TArray;
    ids: Item.IDArray;
  END;

TYPE UpdateClosure = SmallDB.UpdateClosure OBJECT
    state: AtomicItemTbl.State;
  OVERRIDES
    apply := UpdateApply;
  END;

PROCEDURE UpdateApply (uc: UpdateClosure; value: REFANY) =
  VAR ur: UpdateRec := value;
      tbl := uc.state.tbl;
      its := ur.its;
      ids := ur.ids;
      old: Item.T;
  BEGIN
    CASE ur.code OF
    | NotifyRec.Code.Create, NotifyRec.Code.Modify => 
      FOR i := FIRST (its^) TO LAST (its^) DO
        EVAL tbl.put (its[i].id, its[i]);
        uc.state.id := MAX (uc.state.id, its[i].id);
      END;
    | NotifyRec.Code.Delete => 
      FOR i := FIRST (ids^) TO LAST (ids^) DO
        EVAL tbl.delete (ids[i], old);
      END;
    ELSE (*SKIP*)
    END;
  END UpdateApply;

CONST LogLimit = 100000;
      (* When the log grows bigger than this, a snapshot is taken 
         to truncate the log. 
         Note: the longer this limit, the longer it might take to recover.
      *)

PROCEDURE Update (stable: SmallDB.T; bd: BoardX.T; nr: NotifyRec.T) 
    RAISES {OSError.E} =
  BEGIN
    stable.update (NEW (UpdateRec, code := nr.code, 
                        its := nr.its, ids := nr.ids), 
                   forceToDisk := FALSE);
    IF (stable.logBytes () > LogLimit) THEN
      BoardX.Save (bd);
    END;
  END Update;

PROCEDURE Recover (stable: SmallDB.T; state: AtomicItemTbl.State)
    RAISES {OSError.E, SmallDB.CorruptedDB, Pickle.Error} =
  VAR uc := NEW (UpdateClosure, state := state);
  BEGIN
      stable.recoverUpdates (uc);
  END Recover;

BEGIN
END Log.
