(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: Squat.mg,v 1.1.1.1 2001-09-19 11:40:10 wagner Exp $ *)

GENERIC MODULE Squat(Elem);
IMPORT Axis;
IMPORT VBT;
REVEAL
  T = Elem.T BRANDED OBJECT
  OVERRIDES
    shape := Shape;
  END; 
PROCEDURE Shape(self: T; ax: Axis.T; n: CARDINAL): VBT.SizeRange =
  CONST
    size = 16;
  BEGIN
    IF ax = Axis.T.Ver THEN
      RETURN VBT.SizeRange{size-1,size,size+1};
    ELSE
      RETURN Elem.T.shape(self, ax, n);
    END;
  END Shape;
BEGIN
END Squat.
