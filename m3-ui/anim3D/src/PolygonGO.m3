(* Copyright (C) 1993, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Created by Marc Najork                                                    *)
(* Last modified on Mon Jan 30 23:03:35 PST 1995 by najork                   *)


MODULE PolygonGO EXPORTS PolygonGO, PolygonGOProxy;

IMPORT AnimServer, BSphere, GO, GOPrivate, GraphicsBase, GraphicsBasePrivate, 
       Point3, PointProp, PointPropPrivate, Prop, PropPrivate, SurfaceGO;

REVEAL T = SurfaceGO.T BRANDED "PolygonGO.T" OBJECT END;

(*****************************************************************************)
(* Static                                                                    *)
(*****************************************************************************)

TYPE
  Static = T BRANDED OBJECT
    pts    : REF ARRAY OF Point3.T;
    shape  : GO.Shape;
    bs     : BSphere.T;
  METHODS
    init (READONLY pts : ARRAY OF Point3.T; s : GO.Shape) : Static 
      := InitStatic;
  OVERRIDES
    draw := DrawStatic;
  END;


PROCEDURE InitStatic (self         : Static; 
                      READONLY pts : ARRAY OF Point3.T; 
                      s            : GO.Shape) : Static =
  VAR
    min, max : Point3.T;
  BEGIN
    EVAL GO.T.init (self);
    self.pts := NEW (REF ARRAY OF Point3.T, NUMBER (pts));
    self.pts^ := pts;
    self.shape := s;

    (* Compute a bounding sphere. Precision is not that relevant, as long as 
       our guess is conservative (i.e. the sphere indeed contains the entire 
       polygon). *)

    (* First, compute a bounding box containing all points of the polygon. *)
    min := Point3.Max;
    max := Point3.Min;
    FOR i := FIRST (pts) TO LAST (pts) DO
      min.x := MIN (min.x, pts[i].x);
      min.y := MIN (min.y, pts[i].y);
      min.z := MIN (min.z, pts[i].z);
      max.x := MAX (max.x, pts[i].x);
      max.y := MAX (max.y, pts[i].y);
      max.z := MAX (max.z, pts[i].z);
    END;

    IF min # Point3.Max AND max # Point3.Min THEN
      (* Fit a bounding sphere around the bounding box. *)
      self.bs.center := Point3.MidPoint (min, max);
      self.bs.radius := Point3.Distance (min, max) / 2.0;
    END;

    IF MkProxyT # NIL AND self.proxy = NIL THEN
      MkProxyT (self);
    END;

    RETURN self;
  END InitStatic;


PROCEDURE DrawStatic (self : Static; state : GraphicsBase.T) =
  BEGIN
    state.push (self);

    state.drawPolygon (self.pts^, self.shape);
    IF NUMBER (self.pts^) > 0 THEN
      state.growBoundingVolume (self.bs.center, self.bs.radius);
    END;

    state.pop (self);
  END DrawStatic;


PROCEDURE NewStatic (READONLY pts : ARRAY OF Point3.T; 
                     s := GO.Shape.Unknown) : T =
  BEGIN
    RETURN NEW (Static).init (pts, s);
  END NewStatic;


(*****************************************************************************)
(* Dynamic                                                                   *)
(*****************************************************************************)


TYPE
  Dynamic = T BRANDED OBJECT
    pvs   : REF ARRAY OF PointProp.Val;
    pts   : REF ARRAY OF Point3.T;
    shape : GO.Shape;
  METHODS
    init (READONLY pvs : ARRAY OF PointProp.Val; s : GO.Shape) : Dynamic 
      := InitDynamic;
  OVERRIDES
    adjust := AdjustDynamic;
    draw   := DrawDynamic;
  END;


PROCEDURE InitDynamic (self         : Dynamic; 
                       READONLY pvs : ARRAY OF PointProp.Val; 
                       s            : GO.Shape) : Dynamic =
  BEGIN
    EVAL GO.T.init (self);
    self.pts := NEW (REF ARRAY OF Point3.T, NUMBER (pvs));
    self.pvs := NEW (REF ARRAY OF PointProp.Val, NUMBER (pvs));
    self.pvs^ := pvs;
    self.shape := s;

    IF MkProxyT # NIL AND self.proxy = NIL THEN
      MkProxyT (self);
    END;

    RETURN self;
  END InitDynamic;


PROCEDURE AdjustDynamic (self : Dynamic; time : LONGREAL) =
  BEGIN
    (*** adjust the properties in the property list ***)
    T.adjust (self, time);

    (*** adjust the point proverty values describing the corners ***)
    FOR i := FIRST (self.pvs^) TO LAST (self.pvs^) DO
      TRY
        IF self.pvs[i].adjust (time) THEN
          self.damaged := TRUE;
        END;
        self.pts[i] := self.pvs[i].val;
      EXCEPT
      | Prop.BadMethod (msg) => AnimServer.ReportError (msg & "\n");
      END;
    END;
  END AdjustDynamic;



PROCEDURE DrawDynamic (self : Dynamic; state : GraphicsBase.T) =
  VAR
    min, max : Point3.T;
  BEGIN
    state.push (self);

    (* Compute a bounding sphere. Precision is not that relevant, as long as 
       our guess is conservative (i.e. the sphere indeed contains the entire 
       polygon). *)

    (* First, compute a bounding box containing all points of the polygon. *)
    IF NUMBER (self.pts^) > 0 THEN
      min := Point3.Max;
      max := Point3.Min;
      FOR i := FIRST (self.pts^) TO LAST (self.pts^) DO
        min.x := MIN (min.x, self.pts[i].x);
        min.y := MIN (min.y, self.pts[i].y);
        min.z := MIN (min.z, self.pts[i].z);
        max.x := MAX (max.x, self.pts[i].x);
        max.y := MAX (max.y, self.pts[i].y);
        max.z := MAX (max.z, self.pts[i].z);
      END;

      (* Fit a bounding sphere around the bounding box. *)
      WITH center = Point3.MidPoint (min, max),
           radius = Point3.Distance (min, max) / 2.0 DO
        state.growBoundingVolume (center, radius);
      END;
    END;

    state.drawPolygon (self.pts^, self.shape);

    state.pop (self);
  END DrawDynamic;


PROCEDURE New (READONLY pts : ARRAY OF PointProp.Val; 
               s := GO.Shape.Unknown) : T =
  BEGIN
    RETURN NEW (Dynamic).init (pts, s);
  END New;


BEGIN
END PolygonGO.
