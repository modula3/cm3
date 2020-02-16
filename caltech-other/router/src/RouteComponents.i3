(* $Id$ *)

INTERFACE RouteComponents;
IMPORT MagLabelList AS LabelList;
IMPORT MagCell;
IMPORT TextMagLayerTbl AS TextLayerTbl;
IMPORT RectBins, EntryDB;
IMPORT GridPointRouteIDTbl;
IMPORT Wr;
IMPORT GridPointEntriesTbl, GridPointSetSeq;
IMPORT Components;
IMPORT GridPointSet;

TYPE 
  Default <: DefaultPublic;
  

TYPE
  T = Components.T;

  DefaultPublic = T OBJECT METHODS
    init(initLayout : MagCell.T; 
         wiringCell : MagCell.T;
         targets : LabelList.T;
         layerDB : TextLayerTbl.T;
         entryDB : EntryDB.T;
         binnedLayout : RectBins.T;

         specialEndPtsTbl (* OUT *) : GridPointRouteIDTbl.T := NIL;

         delWr : Wr.T;
         (* delWr is a writer whither you'd like written the magic
            coordinates of rip-ups as they happen.

            Useful for keeping track of congestion... 
            making delWr NIL turns off this feature
         *)

         gridPointEntries : GridPointEntriesTbl.T := NIL
         (* if non-NIL, put in all gridpoint entries *)
         ) : T;
(* init will fill in any special endpoints---i.e.,
   endpoints that are the ONLY legal endpoints of their given
   components---into specialEndPtsTbl; the router can then use
   these to prevent routes from being boxed in (as an optimization) *)

  END;


CONST Brand = "RouteComponents";

PROCEDURE DefaultEndpoints(targets : LabelList.T;
                           layerDB : TextLayerTbl.T;
                           entryDB : EntryDB.T;
                           binnedLayout : RectBins.T;
                           nonObstacles : GridPointSet.T := NIL;
                           warmUpBins : BOOLEAN := FALSE) : GridPointSetSeq.T;
  (* will fill in nonObstacles with points that shouldn't be treated as
     obstacles when routing this net 

     warmUp denotes whether or not DefaultEndpoints should warm up the bins
     before extracting.  Recommended to put this to TRUE *unless* bins are 
     already warmed up by other code *)
  

END RouteComponents.


