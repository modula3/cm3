(* Copyright (C) 1993, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Created by Marc Najork                                                    *)
(* Last modified on Tue Feb 21 10:16:23 PST 1995 by najork                   *)


MODULE QuadMeshGO EXPORTS QuadMeshGO, QuadMeshGOProxy;

IMPORT BSphere, Color, GO, GOPrivate, GraphicsBase, GraphicsBasePrivate, 
       Point3;

REVEAL 
  T = Public BRANDED OBJECT
    pts   : REF ARRAY OF ARRAY OF Point3.T; 
    shape : GO.Shape;
    cols  : REF ARRAY OF ARRAY OF Color.T; 
    bs    : BSphere.T;
  OVERRIDES
    init              := Init;
    draw              := Draw;
    addFacetColors    := AddFacetColors;
    setColorOfFacet   := SetColorOfFacet;
  END;


PROCEDURE Init (self : T; 
                READONLY pts : ARRAY OF ARRAY OF Point3.T; 
                s : GO.Shape) : T =
  VAR
    min, max : Point3.T;
  BEGIN
    EVAL GO.T.init (self);
    self.pts := NEW (REF ARRAY OF ARRAY OF Point3.T, 
                     NUMBER(pts), NUMBER(pts[0])); 
    self.pts^ := pts;
    self.shape := s; 
    self.cols := NIL;

    (* Compute a bounding sphere. Precision is not that relevant, as long as 
       our guess is conservative (i.e. the sphere indeed contains the entire 
       quad-mesh). *)

    (* First, compute a bounding box containing all points of the quadmesh. *)
    min := Point3.Max;
    max := Point3.Min;
    FOR i := FIRST (pts) TO LAST (pts) DO
      FOR j := FIRST (pts[i]) TO LAST (pts[i]) DO
        min.x := MIN (min.x, pts[i][j].x);
        min.y := MIN (min.y, pts[i][j].y);
        min.z := MIN (min.z, pts[i][j].z);
        max.x := MAX (max.x, pts[i][j].x);
        max.y := MAX (max.y, pts[i][j].y);
        max.z := MAX (max.z, pts[i][j].z);
      END;
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
  END Init;


PROCEDURE Draw (self : T; state : GraphicsBase.T) =
  BEGIN
    IF self.damaged THEN
      state.openDisplayList (self);
      state.push (self);

      IF NUMBER (self.pts^) > 0 AND NUMBER (self.pts[0]) > 0 THEN
        state.growBoundingVolume (self.bs.center, self.bs.radius);
      END;

      IF self.cols = NIL THEN 
        state.drawQuadMesh (self.pts^, self.shape);
      ELSE
        state.drawColoredQuadMesh (self.pts^, self.cols^, self.shape);
      END;

      state.pop (self);
      state.closeDisplayList ();
    ELSE
      (* Even if we can used the cached quad mesh, we have to grow the
         global bounding volume. In order to do so, we have to push the 
         state (since "growBoundingVolume" uses the current transformation
         to map the bounding sphere into world coordinates). *)
      IF NUMBER (self.pts^) > 0 AND NUMBER (self.pts[0]) > 0 THEN
        state.push (self);
        state.growBoundingVolume (self.bs.center, self.bs.radius);
        state.pop (self);
      END;
    END;

    (*** Use the cache values ***)
    state.callDisplayList (self);
(****
    state.push (self);
    IF NUMBER (self.pts^) > 0 AND NUMBER (self.pts[0]) > 0 THEN
      state.growBoundingVolume (self.bs.center, self.bs.radius);
    END;

    IF self.cols = NIL THEN 
      state.drawQuadMesh (self.pts^, self.shape);
    ELSE
      state.drawColoredQuadMesh (self.pts^, self.cols^, self.shape);
    END;

    state.pop (self);
****)
  END Draw;


PROCEDURE AddFacetColors (self : T; READONLY cols : ARRAY OF ARRAY OF Color.T) 
    RAISES {BadSize} =
  BEGIN
    self.damaged := TRUE;
    IF NUMBER(cols) # NUMBER(self.pts^) - 1 OR
       NUMBER(cols[0]) # NUMBER(self.pts[0]) - 1 THEN
      RAISE BadSize;
    END;
    self.cols := NEW(REF ARRAY OF ARRAY OF Color.T,
                     NUMBER(cols), NUMBER(cols[0]));
    self.cols^ := cols;
  END AddFacetColors;


PROCEDURE SetColorOfFacet (self : T; i, j : INTEGER; c : Color.T) 
    RAISES {ColorsUndefined} = 
  BEGIN
    self.damaged := TRUE;
    IF self.cols = NIL THEN
      RAISE ColorsUndefined;
    END;
    self.cols[i][j] := c;
  END SetColorOfFacet;


BEGIN
END QuadMeshGO.
