(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Fri Feb  3 22:06:12 PST 1995 by najork                   *)
(*       Created on Fri Feb  3 21:46:55 PST 1995 by najork                   *)


MODULE GraphicsBase EXPORTS GraphicsBase, GraphicsBasePrivate;

IMPORT BSphere, GO, Matrix4, Point3, TransformPropPrivate;

REVEAL
  T = Private BRANDED OBJECT
  (* bounding volume stuff *)
    b_vol_min        : Point3.T;
    b_vol_max        : Point3.T;
  OVERRIDES
    resetBoundingVolume := ResetBoundingVolume;
    growBoundingVolume  := GrowBoundingVolume;
    getBoundingVolume   := GetBoundingVolume;
  END;

(*****************************************************************************)
(* Bounding Volume Management                                                *)
(*****************************************************************************)


PROCEDURE ResetBoundingVolume (self : T) =
  BEGIN
    self.b_vol_min := Point3.Max;
    self.b_vol_max := Point3.Min;
  END ResetBoundingVolume;


PROCEDURE GrowBoundingVolume (self : T; center : Point3.T; radius : REAL) =
  BEGIN
    WITH A       = GO.Transform.getState(self),
         center1 = Matrix4.TransformPoint3 (A, center),
         radius1 = radius * Matrix4.UnitSphereMaxSquishFactor (A),
         min     = self.b_vol_min, 
         max     = self.b_vol_max DO

      min := Point3.T {MIN (min.x, center1.x - radius1),
                       MIN (min.y, center1.y - radius1),
                       MIN (min.z, center1.z - radius1)};
      max := Point3.T {MAX (max.x, center1.x + radius1),
                       MAX (max.y, center1.y + radius1),
                       MAX (max.z, center1.z + radius1)};
    END;
  END GrowBoundingVolume;


PROCEDURE GetBoundingVolume (self : T) : BSphere.T =
  BEGIN
    WITH min = self.b_vol_min, max = self.b_vol_max DO
      IF min = Point3.Max AND max = Point3.Min THEN
        (* There are no objects with nonempty bounding boxes in the root, 
           so we set the bounding sphere to a default value. *)
        RETURN BSphere.T {Point3.Origin, 0.0};
      ELSE
        (* We put a (conservative) bounding sphere around the bounding box. *)

        RETURN BSphere.T {Point3.MidPoint (min, max), 
                          Point3.Distance (min, max) / 2.0};
      END;
    END;
  END GetBoundingVolume;



BEGIN
END GraphicsBase.

