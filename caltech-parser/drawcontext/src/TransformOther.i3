(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id$ *)

INTERFACE TransformOther;
IMPORT RealPoint;
IMPORT Transform;
IMPORT Point;
IMPORT Rect;
IMPORT Region;
IMPORT Line;
IMPORT LinoText;

TYPE
  T = Transform.T;

CONST
  Identity = T{a11 := 1.0, a12 := 0.0, a21 := 0.0,
               a22 := 1.0, a31 := 0.0, a32 := 0.0};
  RotLeft = T{a11 := 0.0, a12 := -1.0, a21 := 1.0,
              a22 := 0.0, a31 := 0.0, a32 := 0.0};
  RotRight = T{a11 := 0.0, a12 := 1.0, a21 := -1.0,
               a22 := 0.0, a31 := 0.0, a32 := 0.0};
  FlipH = T{a11 := -1.0, a12 := 0.0, a21 := 0.0,
            a22 := 1.0, a31 := 0.0, a32 := 0.0};
  FlipV = T{a11 := 1.0, a12 := 0.0, a21 := 0.0,
            a22 := -1.0, a31 := 0.0, a32 := 0.0};

  Brand = "Transform";

PROCEDURE ApplyToRect(t: T; r: Rect.T): Rect.T;
PROCEDURE ApplyToRegion(t: T; r: Region.T): Region.T;
PROCEDURE ApplyToLine(t: T; l: Line.T): Line.T;

PROCEDURE LeftTurns(t: T): [0..3];

PROCEDURE ApplyToAttach(t: T; a: LinoText.Attach): LinoText.Attach;
PROCEDURE ApplyToInt(t: T; i: INTEGER): INTEGER;
PROCEDURE ApplyToText(t: T; tx: LinoText.T): LinoText.T;

PROCEDURE Inverse(t: T): T;
PROCEDURE IsoScaleAbout(scale: REAL; p: Point.T): T;

PROCEDURE RoundApplyToReal(t: T; r: REAL): INTEGER;
PROCEDURE ApplyToRealPoint(t: T; p: RealPoint.T): RealPoint.T;
PROCEDURE AnIsoScaleAboutReal(hScale, vScale: REAL; p: RealPoint.T): T;

PROCEDURE Equal(a, b: T): BOOLEAN;

END TransformOther. 
