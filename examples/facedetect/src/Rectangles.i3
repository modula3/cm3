INTERFACE Rectangles;

IMPORT RefSeq,Image;

TYPE
  Rect = RECORD
    x,y,width,height : INTEGER;
  END;
  RefRect = REF Rect;
  
PROCEDURE GroupRectangles(VAR rectList : RefSeq.T; groupThreshold : INTEGER; eps : REAL);

(* draw white bounding boxes around detected faces *)
PROCEDURE DrawRectangle(VAR image : Image.Image; r : Rect);
  
END Rectangles.
