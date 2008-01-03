(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)

MODULE Log;

IMPORT SmallDB, OSError, Pickle, ItemTbl,
       AtomicItemTbl, NotifyRec, Item, BoardX, Rd, Wr;

TYPE UpdateRec = REF RECORD
    code: NotifyRec.Code;
    its: Item.TArray;
    ids: Item.IDArray;
  END;

REVEAL UpdateClosure = SmallDB.Closure BRANDED OBJECT
  OVERRIDES
    readUpdate := ReadUpdate;
    new := New;
    recover := Recover;
    snapshot := Snapshot;
    logUpdate := Snapshot;
  END;

PROCEDURE Recover(uc : UpdateClosure; rd : Rd.T) : REFANY =
  BEGIN RETURN Pickle.Read(rd) END Recover;

PROCEDURE Snapshot(uc : UpdateClosure; wr : Wr.T; r : REFANY) =
  BEGIN Pickle.Write(wr,r) END Snapshot;

PROCEDURE New(uc : UpdateClosure) : REFANY =
  CONST ExpectedItems = 1000;
  BEGIN RETURN 
      NEW (AtomicItemTbl.State,
                    tbl := NEW (ItemTbl.Default).init (ExpectedItems),
                    id := 0);
  END New;

PROCEDURE ReadUpdate (uc: UpdateClosure; rd : Rd.T; stateA: REFANY) : REFANY =
  VAR ur: UpdateRec := Pickle.Read(rd);
      state := NARROW(stateA,AtomicItemTbl.State);
      tbl := state.tbl;
      its := ur.its;
      ids := ur.ids;
      old: Item.T;
  BEGIN
    CASE ur.code OF
    | NotifyRec.Code.Create, NotifyRec.Code.Modify => 
      FOR i := FIRST (its^) TO LAST (its^) DO
        EVAL tbl.put (its[i].id, its[i]);
        state.id := MAX (state.id, its[i].id);
      END;
    | NotifyRec.Code.Delete => 
      FOR i := FIRST (ids^) TO LAST (ids^) DO
        EVAL tbl.delete (ids[i], old);
      END;
    ELSE (*SKIP*)
    END;

    RETURN state
  END ReadUpdate;

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

BEGIN
END Log.
