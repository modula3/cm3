(* $Id$ *)

INTERFACE RectBins;
IMPORT MagLayerRect, MagRect, MagCell, RectSet;
IMPORT MagLabel;
IMPORT TextMagLayerTbl;
IMPORT MagSession;

EXCEPTION NoConnectedRects;

CONST DefaultStep = 30;

TYPE
  Mapper = PROCEDURE(READONLY baseRect, iRect : MagLayerRect.T; args : REFANY);

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init(root : MagCell.T; step : CARDINAL := DefaultStep) : T;
    addLayerRect(READONLY rect : MagLayerRect.T);
    
    addRectSet(set : RectSet.T);

    getATouchingRect(layerDB : TextMagLayerTbl.T; 
                     READONLY label : MagLabel.T) : MagLayerRect.T RAISES { NoConnectedRects } ;

    getConnectedClosure(READONLY seed : MagLayerRect.T) : RectSet.T;

    getOverlappingRectsAllLayers(READONLY rect : MagRect.T) : RectSet.T;

    getInterferingRects(READONLY rect : MagLayerRect.T) : RectSet.T;

    mapInterferingRects(READONLY rect : MagLayerRect.T;
                        mapper : Mapper;
                        args : REFANY);

    (* flush cache around a given rect to pick up changes that
       can't be described through addLayerRect (i.e., deletions) *)
    flushAroundRect(READONLY rect : MagLayerRect.T);

    (* take a just rolled-back session and flush the rectangles that
       were in that session---this doesn't really seem right, because
       discipline would demand that we not use the session after it has
       been rolled back *)
    flushStaleSession(session : MagSession.T);

    advise(willTouch : RectSet.T);
    (* advise that we will touch these rects---may improve performance *)

    loadEverything();
    (* bin everything in memory *)
  END;

CONST Brand = "RectBins";

END RectBins.
