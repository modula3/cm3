(* $Id$ *)

INTERFACE SimpleGrid;
IMPORT MagRect;
IMPORT MagPoint, MagPointList;
IMPORT MagRectList;

TYPE
  
  PointType = { Obstacle, Target, Space };
  TypeSet = SET OF PointType;

  Dir = { N, E, W, S };

  T = OBJECT METHODS
    (* all operations will later be clipped to clip *)
    init(clip : MagRect.T; initType := PointType.Space) : T;

    (* draw a rectangle of the specified type *)
    drawRect(type : PointType; rect : MagRect.T);
    
    (* draw a bunch of points *)
    drawPointList(type : PointType; pl : MagPointList.T);
    
    (* bloat existing layout *)
    bloat(type : PointType; dir : Dir; by : CARDINAL);

    (* shrink existing layout *)
    shrink(type : PointType; dir : Dir; by : CARDINAL);
    
    (* route from src to the nearest layout of type tgt. *)
    (* stay within the bounds (this improves performance) *)
    route(READONLY src, llBound, urBound : MagPoint.T ; tgt : PointType; 
          VAR path : MagPointList.T) : BOOLEAN;

    (* is a rect completely covered *)
    isCovered(type : PointType; rect : MagRect.T) : BOOLEAN;

    (* is a rect at all covered -- does not include edges *)
    touches(type : PointType; rect : MagRect.T) : BOOLEAN;

    (* produce a horizontally maximal tiling of the stuff in the cell *)
    (* the tiles that are returned are turned into Space in the       *)
    (* underlying object *)
    rectify(type : PointType) : MagRectList.T;

    (* change all points of type from -> type to *)
    zap(from, to : PointType);
    
    (* copy points from another T *)
    copyPointsFrom(grid : T; types : TypeSet);

    (* return a copy of the object *)
    copy() : T;

    (* a nice debugging printout... *)
    format() : TEXT;
  END;

  (* implementations *)
  (* Do not instantiate a SimpleGrid.T.  Instead, instantiate either a *)
  (* SimpleGrid.TblImpl or a SimpleGrid.ArrImpl, depending on which you *)
  (* want... *) 

  (* Two implementations are provided:                               *)
  (* TblImpl optimizes for memory use (for sparse data structures)   *)
  (* ArrImpl optimizes for speed and for memory use for dense d.s.'s *)
  TblImpl <: T;
  ArrImpl <: T;
    
CONST Brand = "SimpleGrid";

PROCEDURE FormatDir(dir : Dir; longForm := FALSE) : TEXT;

PROCEDURE Recycle(stale : T);

END SimpleGrid.
