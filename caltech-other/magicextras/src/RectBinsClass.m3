(* $Id$ *)
MODULE RectBinsClass;

IMPORT Bins;
IMPORT MagLayerRect AS LayerRect, MagRect;
IMPORT RectSet;
IMPORT RectBins;
FROM RectBins IMPORT NoConnectedRects;
IMPORT MagLayer AS Layer;
IMPORT MagLayerRectSeq AS LayerRectSeq;
IMPORT TextMagLayerTbl AS TextLayerTbl;
IMPORT MagLabel AS Label;
IMPORT IntPair;
IMPORT Debug;
IMPORT Fmt;
IMPORT MagSession;
IMPORT MagLayerRectSet;
IMPORT IntPairSetDef;

VAR DebugIt := Debug.DebugThis("RectBins");

(* A RectBinsClass.T has two iterators.  Both iterators are locked to
   allow safe concurrent (and recursive) access.  binIter is used for
   downcalls and binIter2 for upcalls (which may occur in the same
   call stack). *)

(* The two binIters are really just kept here to avoid allocating memory
   each time we want to iterate thru bins. *)

REVEAL
  T = Private BRANDED "RectBinsClass Private" OBJECT 
    binIter, binIter2 : Bins.Iterator := NIL; (* protected by mutex *)
  OVERRIDES
    initClass := Init;
    addLayerRect :=        AddLayerRect;
    getConnectedClosure := GetConnectedClosure;
    getATouchingRect :=    GetATouchingRect;
    getInterferingRects := GetInterferingRects;
    mapInterferingRects := MapInterferingRects;
    flushAroundRect := FlushAroundRect;
    addRectSet := AddRectSet;
    flushStaleSession := FlushStaleSession;
    getOverlappingRectsAllLayers := GetOverlappingRectsAllLayers;
    loadEverything := LoadEverything;
  END;

TYPE S = T;

PROCEDURE AddRectSet(t : T; s : RectSet.T) =
  VAR
    iter := s.iterate();
    r : LayerRect.T;
  BEGIN
    WHILE iter.next(r) DO t.addLayerRect(r) END
  END AddRectSet;

PROCEDURE FlushStaleSession(t : T; sess : MagSession.T) =
  VAR
    iter := sess.iterate();
    r : LayerRect.T;
  BEGIN
    WHILE iter.next(r) DO t.flushAroundRect(r) END
  END FlushStaleSession;

PROCEDURE Init(self : T) : T =
  BEGIN 
    self.binIter  := NEW(Bins.Iterator); 
    self.binIter2 := NEW(Bins.Iterator); 
    RETURN self 
  END Init;

(* this procedure needs to be re-entrant because it can get called up *)
PROCEDURE AddLayerRect(self : S; READONLY rect : LayerRect.T) =
  VAR
    ip : IntPair.T;
  BEGIN
    LOCK self.binIter2 DO
      Bins.ComputeIterator(self.step, rect.rect, self.binIter2);
      WHILE self.binIter2.next(ip) DO 
        IF DebugIt THEN 
          Debug.Out("Adding rect " & LayerRect.Format(rect) & " to bin " &
            Fmt.Int(ip.k1) & "," & Fmt.Int(ip.k2))
        END;
        self.put(ip, rect) 
      END
    END
  END AddLayerRect;

PROCEDURE GetOverlappingRectsAllLayers(t : T;
                                       READONLY rect : MagRect.T) : RectSet.T =
  VAR
    res := NEW(RectSet.T).init();
    ip : IntPair.T;
    set : MagLayerRectSet.T;
  BEGIN
    LOCK t.binIter DO
      Bins.ComputeIterator(t.step, rect, t.binIter);
      WHILE t.binIter.next(ip) DO
        t.get(ip, set);
        IF set # NIL THEN
          res := res.unionD(set)
        END
      END
    END;
    RETURN res
  END GetOverlappingRectsAllLayers;


PROCEDURE FlushAroundRect(self : S; READONLY rect : LayerRect.T) =
  VAR
    ip : IntPair.T;
  BEGIN
    LOCK self.binIter2 DO
      Bins.ComputeIterator(self.step, rect.rect, self.binIter2);
      WHILE self.binIter2.next(ip) DO self.flush(ip) END
    END
  END FlushAroundRect;

TYPE ScanProc = PROCEDURE (READONLY rect : LayerRect.T; arg : REFANY);

PROCEDURE MapDirectlyConnectedRects(self : T; 
                                     READONLY a : LayerRect.T;
                                     scanner : ScanProc;
                                     arg : REFANY) =
  BEGIN
    LOCK self.binIter DO 

      (* for all grid squares... *)
      VAR 
        set : MagLayerRectSet.T;
        ip : IntPair.T;
      BEGIN
        Bins.ComputeIterator(self.step, a.rect, self.binIter);
        IF DebugIt THEN
          Debug.Out("RectBinsClass.MapDirectlyConnectedRects: --- CHECKING " & 
            LayerRect.Format(a)  ) 
        END;
        WHILE self.binIter.next(ip) DO
          IF DebugIt THEN
            Debug.Out("RectBinsClass.MapDirectlyConnectedRects: peeking in bin " & 
              Fmt.Int(ip.k1) & "," & Fmt.Int(ip.k2))
          END;    
          self.get(ip, set);
          (* for all rects.. *)
          IF set # NIL THEN
            VAR 
              iter := set.iterate();
              h : LayerRect.T;
            BEGIN
              WHILE iter.next(h) DO
                IF DebugIt THEN
                  Debug.Out("RectBinsClass.MapDirectlyConnectedRects: checking connections for " & 
                    LayerRect.Format(a) & ": scanning " & 
                    LayerRect.Format(h))
                END;
                
                IF LayerRect.ElectricallyConnected(h, a) THEN
                  IF DebugIt THEN
                    Debug.Out("RectBinsClass.MapDirectlyConnectedRects: keeping "& 
                      LayerRect.Format(h))
                  END;
                  scanner(h, arg)
                ELSE
                  IF DebugIt THEN
                    Debug.Out("RectBinsClass.MapDirectlyConnectedRects: tossing "& 
                      LayerRect.Format(h))
                  END;
                END
              END
            END
          END (* IF set # NIL *)

        END
      END
    END
  END MapDirectlyConnectedRects;

TYPE MapperArg = REF RECORD seq : LayerRectSeq.T; set : RectSet.T END;

PROCEDURE RectMapper(READONLY rect : LayerRect.T; argAny : REFANY) =
  VAR 
    arg : MapperArg := argAny;
  BEGIN
    IF NOT arg.set.member(rect) THEN arg.seq.addhi(rect) END
  END RectMapper;

PROCEDURE GetConnectedClosure(self : S; 
                              READONLY seed : LayerRect.T) : RectSet.T =
  VAR
    (* set of rects we have so far *)
    set := NEW(RectSet.T).init();

    (* sequence that we are going down *)
    seq := NEW(LayerRectSeq.T).init();
  BEGIN
    (* initialize sequence *)
    seq.addlo(seed);

    (* iterate until we have computed the closure... *)
    WHILE seq.size() > 0 DO
      VAR
        nextSeed := seq.remlo();
      BEGIN
        IF NOT set.member(nextSeed) THEN
          MapDirectlyConnectedRects(self, nextSeed, RectMapper, 
                                    NEW(MapperArg, seq := seq, set := set));
          
          (* add seed if the mapping thing didn't... ??? *)
          EVAL set.insert(nextSeed)
        END
      END
    END;
    RETURN set
  END GetConnectedClosure;
  
PROCEDURE GetInterferingRects(self : S; 
                              READONLY a : LayerRect.T) : RectSet.T =
  VAR
    res := NEW(RectSet.T).init();
  BEGIN
    (* for all grid squares... *)
    VAR 
      set : MagLayerRectSet.T;
      ip : IntPair.T;
    BEGIN
      LOCK self.binIter DO
        Bins.ComputeIterator(self.step, a.rect, self.binIter);
        WHILE self.binIter.next(ip) DO
       
          self.get(ip, set);
          (* for all rects.. *)

          IF set # NIL THEN
            VAR 
              iter := set.iterate();
              h : LayerRect.T;
            BEGIN
              WHILE iter.next(h) DO
                IF LayerRect.ElectricallyConnected(h, a) THEN
                  EVAL res.insert(h)
                END
              END
            END
          END
        END
      END
    END;
    RETURN res
  END GetInterferingRects;
  
PROCEDURE MapInterferingRects(self : S; 
                              READONLY a : LayerRect.T;
                              mapper : RectBins.Mapper;
                              args : REFANY) =
  BEGIN
    (* for all grid squares... *)
    VAR 
      set : MagLayerRectSet.T;
      ip : IntPair.T;
    BEGIN
      LOCK self.binIter DO
        Bins.ComputeIterator(self.step, a.rect, self.binIter);
        WHILE self.binIter.next(ip) DO
       
          self.get(ip, set);
          (* for all rects.. *)

          IF set # NIL THEN
            VAR 
              iter := set.iterate();
              h : LayerRect.T;
            BEGIN
              WHILE iter.next(h) DO
                IF LayerRect.ElectricallyConnected(h, a) THEN
                  mapper(a,h,args)
                END
              END
            END
          END
        END
      END
    END
  END MapInterferingRects;

PROCEDURE GetADirectlyConnectedRect(self : T; 
                                     READONLY a : LayerRect.T;
                                     VAR b : LayerRect.T) : BOOLEAN =
  VAR
    checkBins := Bins.Compute(self.step, a.rect);
    cp := checkBins; 
    set : MagLayerRectSet.T;
  BEGIN
    WHILE cp # NIL DO
      self.get(cp.head, set);
      (* for all rects.. *)
      
      IF set # NIL THEN
        VAR 
          iter := set.iterate();
          h : LayerRect.T;
        BEGIN
          WHILE iter.next(h) DO
            IF LayerRect.ElectricallyConnected(h, a) THEN
              b := h;
              RETURN TRUE
            END
          END
        END
      END;
      cp := cp.tail
    END; (* WHILE *)
    RETURN FALSE
  END GetADirectlyConnectedRect;

PROCEDURE GetATouchingRect(self : S;
                           layerTbl : TextLayerTbl.T;
                           READONLY label : Label.T) : LayerRect.T RAISES { NoConnectedRects } =
  VAR
    layer : Layer.T;
    res : LayerRect.T;
  BEGIN
    IF NOT layerTbl.get(label.layer, layer) THEN RAISE NoConnectedRects END;
    IF NOT GetADirectlyConnectedRect(self, 
                                     LayerRect.T { label.rect, layer },res) THEN
      RAISE NoConnectedRects
    END;
    RETURN res
  END GetATouchingRect;

PROCEDURE LoadEverything(s : S) =
  VAR
    set := NEW(IntPairSetDef.T).init();
    bbox := s.root.getBBox();
  BEGIN
    (* grow bbox a bit for good measure *)

    DEC(bbox.ll.x,2*s.step);
    DEC(bbox.ll.y,2*s.step);

    INC(bbox.ur.x,2*s.step);
    INC(bbox.ur.y,2*s.step);

    Bins.ComputeAndAddToSet(s.step, bbox, set);
    s.initAtKeys(set)
  END LoadEverything;

BEGIN END RectBinsClass.
