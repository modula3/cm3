(* $Id$ *)

MODULE MagMergeDB;
IMPORT MagLayerSet, MagLayerSetDef, MagLayer, MagLayer_LayerSetTbl;
IMPORT MagLayerList;
IMPORT Debug;
IMPORT MagRouteLayer AS RouteLayer;

CONST DoDebug = FALSE;

REVEAL
  T = Public BRANDED Brand OBJECT 
    mergeableSet : MagLayerSet.T;
    tbl : MagLayer_LayerSetTbl.T;
  OVERRIDES
    init           :=  Init;
    addEquivalence :=  AddEquivalence;
    isMergeable    :=  IsMergeable;
    merge          :=  Merge;
    debugDump      :=  DebugDump;
  END;

PROCEDURE DebugDump(self : T) =
  VAR
    si := self.mergeableSet.iterate();
    ti := self.tbl.iterate();
    l : MagLayer.T;
    ls : MagLayerSet.T;
  BEGIN
    Debug.Out("Mergeable set:");
    WHILE si.next(l) DO Debug.Out(NARROW(l,RouteLayer.T).name) END;
    
    WHILE ti.next(l,ls) DO
      VAR
        lsi := ls.iterate();
      BEGIN
        Debug.Out("Merged layer " & NARROW(l,RouteLayer.T).name & ":");
        WHILE lsi.next(l) DO Debug.Out("   " & NARROW(l,RouteLayer.T).name) END
      END
    END
    
  END DebugDump;

PROCEDURE Init(self : T) : T =
  BEGIN
    self.mergeableSet := NEW(MagLayerSetDef.T).init();
    self.tbl := NEW(MagLayer_LayerSetTbl.Default).init();
    RETURN self
  END Init;

PROCEDURE AddEquivalence(self : T; compositeLayer : MagLayer.T;
                         componentLayers : MagLayerList.T) =
  VAR
    s : MagLayerSet.T := NEW(MagLayerSetDef.T).init();
    c := componentLayers;
  BEGIN
    EVAL self.mergeableSet.insert(compositeLayer);
    WHILE c # NIL DO
      EVAL s.insert(c.head);
      EVAL self.mergeableSet.insert(c.head);
      c := c.tail
    END;
    EVAL self.tbl.put(compositeLayer, s)
  END AddEquivalence;

PROCEDURE IsMergeable(self : T; layer : MagLayer.T) : BOOLEAN =
  BEGIN RETURN self.mergeableSet.member(layer) END IsMergeable;

PROCEDURE Merge(self : T; u : MagLayerSet.T) : MagLayerSet.T =
  VAR 
    done := FALSE;
    unmerged := u.copy();
  BEGIN
    IF DoDebug THEN
      Debug.Out("MagMergeDB.Merge: merging set: " & FmtSet(u))
    END;

    (* first expand the set.. *)
    WHILE NOT done DO
      done := TRUE;

      VAR
        iter := unmerged.iterate();
        s : MagLayer.T;
        ss : MagLayerSet.T;
      BEGIN
        WHILE iter.next(s) DO
          IF self.tbl.get(s,ss) THEN
            EVAL unmerged.delete(s);
            unmerged := unmerged.union(ss);
            done := FALSE;
            EXIT
          END
        END
      END
    END;

    IF DoDebug THEN
      Debug.Out("MagMergeDB.Merge: maximal set: " & FmtSet(unmerged))
    END;
    
    (* unmerged is now maximal, merge it up *)
    done := FALSE;
    WHILE NOT done DO
      done := TRUE;
      VAR
        iter := self.tbl.iterate();
        s : MagLayer.T;
        ss : MagLayerSet.T;
        soFar := 1;
        best : MagLayer.T;
        bestS : MagLayerSet.T;
      BEGIN
        WHILE iter.next(s,ss) DO
          IF ss.subset(unmerged) AND 
             unmerged.intersection(ss).size() > soFar THEN
            done := FALSE;
            soFar := unmerged.intersection(ss).size();
            
            (* remember set and replacement layer *)
            best :=  s;
            bestS := ss;
          END
        END;
        IF NOT done THEN EVAL unmerged.diffD(bestS).insert(best) END
      END
    END;

    IF DoDebug THEN
      Debug.Out("MagMergeDB.Merge: merged result: " & FmtSet(unmerged))
    END;

    RETURN unmerged
  END Merge;

PROCEDURE FmtSet(s : MagLayerSet.T) : TEXT = 
  VAR
    si := s.iterate();
    l : MagLayer.T;
    res := "";
  BEGIN
    WHILE si.next(l) DO res := res & NARROW(l,RouteLayer.T).name & " " END;
    RETURN res
  END FmtSet;

BEGIN END MagMergeDB.
