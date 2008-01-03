(* Copyright (C) 1993, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Created by Marc Najork                                                    *)
(* Last modified on Mon Jan 30 22:32:36 PST 1995 by najork                   *)


MODULE BoxGO EXPORTS BoxGO, BoxGOProxy;

IMPORT GO, GOPrivate, GraphicsBase, GraphicsBasePrivate, Point3, PointProp, 
       PointPropPrivate, Prop;

REVEAL 
  T = Public BRANDED OBJECT
    a,b : Point3.T;
    v   : ARRAY [1 .. 6] OF ARRAY [1 .. 4] OF Point3.T;
  OVERRIDES
    init              := Init;
    draw              := Draw;
    damageIfDependent := DamageIfDependent; 
  END;


PROCEDURE Init (self : T) : T =
  BEGIN
    EVAL GO.T.init (self);
    self.a := Point3.Min;  (* This shall ensure that "stale" is true upon the
    self.b := Point3.Max;     first call to "Draw", so that self.v is properly
                              initialized. *)

    IF MkProxyT # NIL AND self.proxy = NIL THEN
      MkProxyT (self);
    END;

    RETURN self;
  END Init;


PROCEDURE DamageIfDependent (self : T; pn : Prop.Name) =
  BEGIN
    IF pn = Corner1 OR pn = Corner2 THEN
      self.damaged := TRUE;
    END;
  END DamageIfDependent;


PROCEDURE Draw (self : T; state : GraphicsBase.T) =
  BEGIN
    state.push (self);
    WITH a = Corner1.getState (state),
         b = Corner2.getState (state) DO

      IF a # self.a OR b # self.b THEN
        self.a := a;
        self.b := b;

        self.v[1][1] := Point3.T{a.x, a.y, a.z};
        self.v[1][2] := Point3.T{a.x, a.y, b.z};
        self.v[1][3] := Point3.T{a.x, b.y, b.z};
        self.v[1][4] := Point3.T{a.x, b.y, a.z};

        self.v[2][1] := Point3.T{b.x, a.y, a.z};
        self.v[2][2] := Point3.T{b.x, a.y, b.z};
        self.v[2][3] := Point3.T{b.x, b.y, b.z};
        self.v[2][4] := Point3.T{b.x, b.y, a.z};

        self.v[3][1] := Point3.T{a.x, a.y, a.z};
        self.v[3][2] := Point3.T{b.x, a.y, a.z};
        self.v[3][3] := Point3.T{b.x, a.y, b.z};
        self.v[3][4] := Point3.T{a.x, a.y, b.z};

        self.v[4][1] := Point3.T{a.x, b.y, a.z};
        self.v[4][2] := Point3.T{b.x, b.y, a.z};
        self.v[4][3] := Point3.T{b.x, b.y, b.z};
        self.v[4][4] := Point3.T{a.x, b.y, b.z};

        self.v[5][1] := Point3.T{a.x, a.y, a.z};
        self.v[5][2] := Point3.T{a.x, b.y, a.z};
        self.v[5][3] := Point3.T{b.x, b.y, a.z};
        self.v[5][4] := Point3.T{b.x, a.y, a.z};

        self.v[6][1] := Point3.T{a.x, a.y, b.z};
        self.v[6][2] := Point3.T{a.x, b.y, b.z};
        self.v[6][3] := Point3.T{b.x, b.y, b.z};
        self.v[6][4] := Point3.T{b.x, a.y, b.z};
      END;
    END;

    state.drawPolygon (self.v[1], GO.Shape.Convex);
    state.drawPolygon (self.v[2], GO.Shape.Convex);
    state.drawPolygon (self.v[3], GO.Shape.Convex);
    state.drawPolygon (self.v[4], GO.Shape.Convex);
    state.drawPolygon (self.v[5], GO.Shape.Convex);
    state.drawPolygon (self.v[6], GO.Shape.Convex);

    state.growBoundingVolume (Point3.MidPoint (self.a, self.b), 
                              Point3.Distance (self.a, self.b) / 2.0);

    state.pop (self);
  END Draw;


PROCEDURE New (a, b : Point3.T) : T =
  VAR 
    box := NEW (T).init ();
  BEGIN
    SetCorner1 (box, a);
    SetCorner2 (box, b);
    RETURN box;
  END New;


(*****************************************************************************)
(* Convenience Procedures                                                    *)
(*****************************************************************************)


PROCEDURE SetCorner1 (o : GO.T; v : Point3.T) =
  BEGIN
    o.setProp (Corner1.bind (PointProp.NewConst (v)));
  END SetCorner1;


PROCEDURE SetCorner2 (o : GO.T; v : Point3.T) =
  BEGIN
    o.setProp (Corner2.bind (PointProp.NewConst (v)));
  END SetCorner2;


BEGIN
  Corner1 := NEW (PointProp.Name).init (Point3.T {0.0, 0.0, 0.0});
  Corner2 := NEW (PointProp.Name).init (Point3.T {1.0, 1.0, 1.0});
END BoxGO.
