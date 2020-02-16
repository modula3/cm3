MODULE MagCellUtils;
IMPORT MagCell;
IMPORT MagCellExtendable;
IMPORT MagLayer;
IMPORT MagRectUtils;
IMPORT MagLabel;
IMPORT MagRouteLayer;
IMPORT OSError;
IMPORT Rect;
IMPORT Region;
IMPORT Thread;
IMPORT Wr;

<* FATAL OSError.E, Thread.Alerted, Wr.Failure *>


PROCEDURE AddRect(self: T; layer: MagLayer.T; r: Rect.T) =
  BEGIN
    self.addRect(MagRectUtils.FromRect(r), layer);
  END AddRect;

PROCEDURE AddLabel(self: T; layer: MagLayer.T; r: Rect.T; t: TEXT) =
  BEGIN
    self.addLabel(MagLabel.T{MagRectUtils.FromRect(r),
                             NARROW(layer, MagRouteLayer.T).name,
                             0, t});
  END AddLabel; 

PROCEDURE AddRegion(self: T; layer: MagLayer.T; r: Region.T) =
  VAR
    rects := Region.ToRects(r);
  BEGIN
    FOR i := 0 TO LAST(rects^) DO
      AddRect(self, layer, rects[i]);
    END;
  END AddRegion;

PROCEDURE NewLayer(name: TEXT): MagLayer.T =
  BEGIN
    RETURN NEW(MagRouteLayer.T).init(name);
  END NewLayer;

PROCEDURE Open(filename: TEXT): T =
  BEGIN
    RETURN NEW(MagCell.Labelled).init(filename);
  END Open;

PROCEDURE Close(self: T) =
  BEGIN
    NARROW(self, MagCell.Labelled).write();
  END Close;

BEGIN
END MagCellUtils.
