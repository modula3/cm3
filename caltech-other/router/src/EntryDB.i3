(* $Id$ *)

INTERFACE EntryDB;
IMPORT GridPoint, MagPointList, GridPointSet;
FROM EndPointStatus IMPORT Dir;
IMPORT RectSet;
IMPORT RectBins;
IMPORT RouteEntries;

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init(bins : RectBins.T) : T;
    get(READONLY gp : GridPoint.T;  tag : REFANY;
        tgtSet, otherRectsInSameNet : RectSet.T; 
        dir : Dir;
        VAR way : MagPointList.T; 
        VAR defaultOK : BOOLEAN) : BOOLEAN;
    
    entriesOK(READONLY gp : GridPoint.T;  tag : REFANY;
              tgtSet, otherRectsInSameNet : RectSet.T) : ARRAY Dir OF BOOLEAN;

    entries(READONLY gp : GridPoint.T;  tag : REFANY;
              tgtSet, otherRectsInSameNet : RectSet.T) : RouteEntries.T;

    (* can we enter this point from EVERY direction? *)
    allEntriesOK(READONLY gp : GridPoint.T;  tag : REFANY;
                 tgtSet, otherRectsInSameNet : RectSet.T) : BOOLEAN;
    
    (* can we enter this point from ANY direction? *)
    someEntryOK(READONLY gp : GridPoint.T;  tag : REFANY;
                tgtSet, otherRectsInSameNet : RectSet.T) : BOOLEAN;
    
    (* indicate that entries are stale for this GridPoint because *)
    (* layout has been updated *)
    (* if radius is zero, flush only gp;
       if it is one, flush all neighbors (N, NE, E, SE, S, SW, W, NW) too
       
       it doesn't do up and down, but do we need that??? 
    *)
    flushEntries(READONLY gp : GridPoint.T; radius : [0..1] := 0);

    (* slow algorithm to determine if the default layout is OK in a cell *)
    defaultOK(READONLY gp : GridPoint.T;  tag : REFANY;
              tgtSet, otherRectsInSameNet : RectSet.T) : BOOLEAN;

    (* slow algorithm to determine if the default layout is OK in a cell *)
    defaultOKrecomputeAlways(READONLY gp : GridPoint.T;
              tgtSet, otherRectsInSameNet : RectSet.T) : BOOLEAN;

    advise(s : GridPointSet.T);
    (* for performance, advise we will inquire about these points soon *)
    
  END;

PROCEDURE DefaultLayoutConnects(tgtSet : RectSet.T; READONLY gp : GridPoint.T) : BOOLEAN;

CONST Brand = "EntryDB";

END EntryDB.
