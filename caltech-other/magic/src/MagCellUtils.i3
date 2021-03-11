INTERFACE MagCellUtils;
(* $Id$
   magic cell creation for the layman:
   everything you need to create a magic cell, in one interface *)

IMPORT Rect;
IMPORT Region;
IMPORT MagLayer;
IMPORT MagCell;
TYPE
  T = MagCell.T;

PROCEDURE NewLayer(name: TEXT): MagLayer.T;

PROCEDURE AddRect(self: T; layer: MagLayer.T; r: Rect.T);
PROCEDURE AddLabel(self: T; layer: MagLayer.T; r: Rect.T; t: TEXT);
PROCEDURE AddRegion(self: T; layer: MagLayer.T; r: Region.T);

PROCEDURE Open(filename: TEXT): T;
PROCEDURE Close(self: T);

END MagCellUtils.
