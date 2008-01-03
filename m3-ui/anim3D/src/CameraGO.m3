(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Fri Sep 30 10:51:10 PDT 1994 by najork                   *)
(*       Created on Mon Feb 14 16:01:13 PST 1994 by najork                   *)


MODULE CameraGO EXPORTS CameraGO, CameraGOPrivate;

IMPORT GO, GOPrivate, Point3, PointProp, PointPropPrivate, RealProp, 
       RealPropPrivate;


REVEAL
  T = Private BRANDED OBJECT
  OVERRIDES
    init := Init;
    needsTransparency := NeedsTransparency;
  END;


PROCEDURE Init (self : T) : T =
  BEGIN
    EVAL GO.T.init (self);
    self.flag := FALSE;
    RETURN self;
  END Init;


PROCEDURE NeedsTransparency (<* UNUSED *> self : T; 
                             <* UNUSED *> t    : REAL) : BOOLEAN =
  BEGIN
    RETURN FALSE;
  END NeedsTransparency;


(*****************************************************************************)
(* Convenience Procedures                                                    *)
(*****************************************************************************)


PROCEDURE SetFrom (o : GO.T; p: Point3.T) =
  BEGIN
    o.setProp(From.bind(PointProp.NewConst(p)));
  END SetFrom;


PROCEDURE SetTo (o : GO.T; p: Point3.T) =
  BEGIN
    o.setProp(To.bind(PointProp.NewConst(p)));
  END SetTo;


PROCEDURE SetUp (o : GO.T; p: Point3.T) =
  BEGIN
    o.setProp(Up.bind(PointProp.NewConst(p)));
  END SetUp;

PROCEDURE SetAspect (o : GO.T; r: REAL) = 
  BEGIN
    o.setProp(Aspect.bind(RealProp.NewConst(r)));
  END SetAspect;


(*****************************************************************************)
(* Module body                                                               *)
(*****************************************************************************)


BEGIN
  From   := NEW (PointProp.Name).init (Point3.T {0.0, 0.0, 100.0});
  To     := NEW (PointProp.Name).init (Point3.Origin);
  Up     := NEW (PointProp.Name).init (Point3.T {0.0, 1.0, 0.0});
  Aspect := NEW (RealProp.Name).init (1.0);
END CameraGO.
