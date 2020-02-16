(* $Id$ *)

MODULE RectBins;
IMPORT MagCell, MagLayerRect AS LayerRect;
IMPORT MagRect, MagPoint;
IMPORT BinsTbl;
IMPORT Debug, TextUtils, Fmt;
IMPORT RectBinsClass;
IMPORT IntPair;
IMPORT MagLayerRectSet, MagLayerRectSetDef;
IMPORT IntPairSet, IntPairSetDef, Bins;
IMPORT RectSet;

<*NOWARN*>VAR DebugIt := Debug.DebugThis("RectBins");

(* XXX this code is not thread-safe if a single object T can be
   shared between multiple threads since the init access to initSet
   can be interleaved with outside accesses thru put and get *)
REVEAL
  T = RectBinsClass.T BRANDED Brand OBJECT  
    binsTbl : BinsTbl.T;
  OVERRIDES
    advise := Advise;
    put  := Put;
    get  := Get;
    init := Init;
    flush := Flush;
    initAtKeys := InitAtKeys;
  END;
  
(* helper proc for flattening the layout *)
PROCEDURE InitBinsMapper(rect : LayerRect.T; args : REFANY) =
  BEGIN NARROW(args,T).addLayerRect(rect) END InitBinsMapper;

PROCEDURE Flush(self : T; READONLY p : IntPair.T) =
  VAR
    v : MagLayerRectSet.T;
  BEGIN
    EVAL self.binsTbl.delete(p,v)
  END Flush;

PROCEDURE Init(self : T; root : MagCell.T; step : CARDINAL) : T = 
  BEGIN 
    EVAL self.initClass();
    self.root := root;
    self.step := step;
    self.binsTbl := NEW(BinsTbl.Default, keyEqual := KeyEqual).init();
    
    RETURN self 
  END Init;

PROCEDURE InitAtKey(self : T; READONLY key : IntPair.T) =
  VAR
    ll := MagPoint.T { key.k1 * self.step, key.k2 * self.step };
    ur := MagPoint.T { (key.k1 + 1) * self.step, 
                       (key.k2 + 1) * self.step };
    bbox := NEW(REF MagRect.T, ll := ll, ur := ur );
  BEGIN
    IF DebugIt THEN
      Debug.Out("RectBins.InitAtKey: initializing " & 
        Fmt.Int(key.k1) & "," & Fmt.Int(key.k2) & ": bbox= " &
        MagRect.Format(bbox^))
    END;
        
    (* this is an upcall---need to be careful here *)
    (* must put each LAYER here... *)
    EVAL self.binsTbl.put(key,NIL);
    self.root.flatClipMap2(InitBinsMapper, self, bbox);
  END InitAtKey;

PROCEDURE InitAtKeys(self : T; keys : IntPairSet.T) =
  VAR
    key : IntPair.T;
    iter := keys.iterate();
    bbox : MagRect.T;
  BEGIN
    IF keys.size() = 0 THEN RETURN END;
    
    (* seed bbox *)
    EVAL iter.next(key);
    bbox := MagRect.T { ll:=MagPoint.T{key.k1*self.step, key.k2 * self.step },
                        ur:=MagPoint.T { (key.k1 + 1) * self.step, 
                       (key.k2 + 1) * self.step } };
                        
    EVAL self.binsTbl.put(key, NIL);
    WHILE iter.next(key) DO
      VAR
        xbbox := MagRect.T { ll:=MagPoint.T{key.k1*self.step, key.k2 * self.step },
                             ur:=MagPoint.T { (key.k1 + 1) * self.step, 
                                              (key.k2 + 1) * self.step } };
      BEGIN
        bbox := MagRect.Union(bbox,xbbox)
      END;
      EVAL self.binsTbl.put(key, NIL)
    END;
    VAR
      b2 := NEW(REF MagRect.T);
    BEGIN
      b2^ := bbox;
      self.root.flatClipMap2(InitBinsMapper, self, b2)
    END
  END InitAtKeys;

(* this is a bit of a hack... *)
PROCEDURE KeyEqual(<*UNUSED*>self : BinsTbl.Default;
                   READONLY a, b : IntPair.T) : BOOLEAN =
  BEGIN RETURN a = b END KeyEqual;

(* implementation of binsTbl *)

<*NOWARN*>PROCEDURE PrintBinStats(self : T) = 
  VAR
    iter := self.binsTbl.iterate();
    idx : IntPair.T;
    totRects := 0;
    totBins := 0;
    set : MagLayerRectSet.T;
  BEGIN
    WHILE iter.next(idx, set) DO
      INC(totRects, set.size());
      INC(totBins)
    END;
    Debug.Out(TextUtils.Pluralize("bin", totBins) &
      ": average content = " & Fmt.Real(FLOAT(totRects) / FLOAT(totBins)) &
      " rects.")
  END PrintBinStats;

PROCEDURE Advise(t : T; willTouch : RectSet.T) =
  VAR
    set := NEW(IntPairSetDef.T).init();
    rIter := willTouch.iterate();
    r : LayerRect.T;
  BEGIN
    WHILE rIter.next(r) DO Bins.ComputeAndAddToSet(t.step, r.rect, set) END;

    VAR
      set2 := set.copy();
      iter := set2.iterate();
      ip : IntPair.T;
      rects : MagLayerRectSet.T;
    BEGIN
      WHILE iter.next(ip) DO
        IF t.binsTbl.get(ip,rects) THEN
          EVAL set.delete(ip)
        END
      END
    END;
    t.initAtKeys(set)
  END Advise;

PROCEDURE Get(self : T; 
              READONLY key : IntPair.T; VAR val : MagLayerRectSet.T) =
  VAR
    i := 0;
  BEGIN 
    IF DebugIt THEN Debug.Out("RectBins.Get: peeking in bin " & 
      Fmt.Int(key.k1) & "," & Fmt.Int(key.k2)) END;
    (* this should only fail at most once :) *)
    WHILE NOT self.binsTbl.get(key,val) DO
      IF DebugIt THEN 
        Debug.Out("RectBins.Get: INITIALIZING bin " & 
        Fmt.Int(key.k1) & "," & Fmt.Int(key.k2)) 
      END;
      InitAtKey(self,key);
      INC(i)
    END; 
    <* ASSERT i <= 1 *>
  END Get;

PROCEDURE Put(self : T; 
              READONLY key : IntPair.T; READONLY val : LayerRect.T) =
  VAR
    old : MagLayerRectSet.T;
  BEGIN 
    (* careful here---we only Put into things that exist or are being
       initialized.  User puts that fall outside the initialized bins
       are simply discarded... *)
    IF self.binsTbl.get(key,old) THEN
      IF DebugIt THEN Debug.Out("RectBins.Put: really putting.") END;
      IF old = NIL THEN 
        old := NEW(MagLayerRectSetDef.T).init();
        EVAL self.binsTbl.put(key,old)
      END;
      EVAL old.insert(val);
    END
  END Put;

(************************************************************************)  

BEGIN END RectBins.

