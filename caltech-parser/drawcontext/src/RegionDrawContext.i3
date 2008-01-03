(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: RegionDrawContext.i3,v 1.2 2001-09-19 15:30:31 wagner Exp $ *)

INTERFACE RegionDrawContext;
(* produces region bounding drawn lines and text *)
IMPORT BoundDrawContext;
IMPORT Region;
IMPORT TextBounder;
TYPE
  T <: Public;
  Public = BoundDrawContext.T OBJECT
  METHODS
    init(textBounder: TextBounder.T): T;
    (* resets accumulated region,
       and sets textBounder for bounding LinoText.T's. *)

    toRegion(): Region.T;
  END;
END RegionDrawContext.
