(* $Id$ *)

INTERFACE MagicStuff;
IMPORT MagPointList, GridPoint, RectSet;
FROM EndPointStatus IMPORT Dir;
IMPORT MagCell;
IMPORT MagLabel;

PROCEDURE ConnectTo(entry : MagPointList.T; 
                    READONLY p : GridPoint.T;
                    dir : Dir) : RectSet.T;

PROCEDURE ConnRects(READONLY a, b : GridPoint.T) : RectSet.T;
PROCEDURE EndPadRects(READONLY a : GridPoint.T) : RectSet.T;
(* draw "landing pad" at point *)

PROCEDURE WireWidth() : CARDINAL;
PROCEDURE ViaSize() : CARDINAL;  (* this is the Magic "contact" size *)
PROCEDURE ViaOverlap() : CARDINAL;  (* this is how much the layout must 
                                       overlap the edge*)
PROCEDURE ViaNEOverlap() : CARDINAL; (* how much extra on NE sides *)

PROCEDURE SetViaNEOverlap(to : CARDINAL);

PROCEDURE MetalSpacing() : CARDINAL;
PROCEDURE GridStep() : CARDINAL;

(* draw a target and return the MagLabel used to label it *)
PROCEDURE DrawATarget(c : MagCell.Labelled;
                      at : GridPoint.T;
                      labelled : TEXT ) : MagLabel.T ;
                      

END MagicStuff.
