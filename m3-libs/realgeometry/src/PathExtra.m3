(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* by Steve Glassman and Stephen Harrison *)
(* *)
(* Contributed by Michel Dagenais (dagenais@vlsi.polymtl.ca), 1994. *)

MODULE PathExtra;

IMPORT Path, RealPath, Point, RealPoint, Rect, 
       RealTransform, Math;

(* TO DO:
   - Extending arrays by one each time is too lazy.
   - Fix up index stuff.
*)

TYPE
  PathBoundsClosure = Path.MapObject OBJECT
    bounds := Rect.Empty;
  OVERRIDES
    move := PathBoundsMove;
    line := PathBoundsLine;
    close := PathBoundsClose;
    curve := PathBoundsCurve;
  END;

PROCEDURE PathBoundsMove(self: PathBoundsClosure; READONLY pt: Point.T) =
  BEGIN
    self.bounds := Rect.Extend(self.bounds, pt);
  END PathBoundsMove;
  
PROCEDURE PathBoundsLine(self: PathBoundsClosure; READONLY pt1, pt2: Point.T) =
  BEGIN
    self.bounds := Rect.Extend(self.bounds, pt1);
    self.bounds := Rect.Extend(self.bounds, pt2);
  END PathBoundsLine;
  
PROCEDURE PathBoundsClose(self: PathBoundsClosure; READONLY pt1, pt2: Point.T) =
  BEGIN
    PathBoundsLine(self, pt1, pt2);
  END PathBoundsClose;
  
PROCEDURE PathBoundsCurve(self: PathBoundsClosure; READONLY pt1, pt2, pt3, pt4: Point.T) =
  BEGIN
    self.bounds := Rect.Extend(self.bounds, pt1);
    self.bounds := Rect.Extend(self.bounds, pt2);
    self.bounds := Rect.Extend(self.bounds, pt3);
    self.bounds := Rect.Extend(self.bounds, pt4);
  END PathBoundsCurve;

PROCEDURE PathBounds(READONLY path: Path.T): Rect.T =
  VAR
    closure := NEW(PathBoundsClosure);
  BEGIN
    TRY
      Path.Map(path, closure);
    EXCEPT
    | Path.Malformed =>
      <* ASSERT FALSE *>
    END;

    RETURN closure.bounds;
  END PathBounds;

TYPE
  ConvertToPathClosure = RealPath.MapObject OBJECT
    path: Path.T := NIL;
    matrix := RealTransform.Identity;
  METHODS
    transformPoint(READONLY pt: RealPoint.T): Point.T := TransformPoint;
  OVERRIDES
    move   := ConvertToPathMove;
    line   := ConvertToPathLine;
    close  := ConvertToPathClose;
    curve  := ConvertToPathCurve;
    arc    := ConvertToPathArc;
  END;

PROCEDURE TransformPoint(self: ConvertToPathClosure; 
                         READONLY pt: RealPoint.T): Point.T =
  VAR
    p: Point.T;
    rp: RealPoint.T;
  BEGIN
    rp := RealTransform.Transform(self.matrix, pt);
    p.h := ROUND(rp[0]);
    p.v := ROUND(rp[1]);
    RETURN p;
  END TransformPoint;

PROCEDURE ConvertToPathMove(self: ConvertToPathClosure; 
                            READONLY r: RealPoint.T) =
  BEGIN
    Path.MoveTo(self.path, self.transformPoint(r));
  END ConvertToPathMove;

PROCEDURE ConvertToPathLine (self: ConvertToPathClosure;
                             READONLY r: RealPoint.T) =
  BEGIN
    Path.LineTo(self.path, self.transformPoint(r));
  END ConvertToPathLine;

PROCEDURE ConvertToPathClose (self: ConvertToPathClosure) =
  BEGIN
    Path.Close(self.path);
  END ConvertToPathClose;

PROCEDURE ConvertToPathCurve (self: ConvertToPathClosure;
                              READONLY pt2, pt3, pt4: RealPoint.T) =
  BEGIN
    Path.CurveTo(
        self.path,
        self.transformPoint(pt2),
        self.transformPoint(pt3),
        self.transformPoint(pt4));
  END ConvertToPathCurve;

PROCEDURE ConvertToPathArc(self: ConvertToPathClosure;
                           READONLY center: RealPoint.T; 
                           READONLY radius, ang1, ang2: REAL) =
  VAR
    p1, p2, p3, p4: RealPoint.T;
    theta1 := ang1;
    theta2 := ang2;
    r: REAL;
  BEGIN
    (* We have a full circle *)
    IF (theta2 - theta1) >= 360.0 THEN
      theta1 := 0.0;
      theta2 := 360.0;

    ELSE
      (* if ang2 is smaller than ang1, add 360 until bigger *)
      IF theta2 < theta1 THEN
        r := FLOAT(CEILING((theta1 - theta2)/360.0),REAL);
        theta2 := theta2 + 360.0 * r;
      END;
      (* normalize for ang1 between 0 and 360 *)
      r := FLOAT(FLOOR(theta1/360.0),REAL);
      theta1 := theta1 - 360.0 * r;
      theta2 := theta2 - 360.0 * r;

    END;

    (* Do as if the center was 0,0 and simply make the correction
       just before issuing the points.
       Get the first end point and line to it*)
    p1[0] := radius *FLOAT(Math.cos(DegToRad * FLOAT(theta1,LONGREAL)),REAL);
    p1[1] := radius *FLOAT(Math.sin(DegToRad * FLOAT(theta1,LONGREAL)),REAL);
    Path.LineTo(self.path,self.transformPoint(RealPoint.Add(p1,center)));

    (* perhaps the arc is longer than 180 degrees. In this case P2 is
       is perpendicular to P1 and 4/3 the length. Same with p4 and p3. *)
    IF theta1 + 180.0 <= theta2 THEN
      p2[0] := -4.0/3.0 * p1[1];
      p3[0] := -p2[0];
      p2[0] := p2[0] + p1[0];
      p4[0] := -p1[0];
      p3[0] := p3[0] + p4[0];
      p2[1] := 4.0/3.0 * p1[0];
      p3[1] := -p2[1];
      p2[1] := p2[1] + p1[1];
      p4[1] := -p1[1];
      p3[1] := p3[1] + p4[1];
      Path.CurveTo(
          self.path,
          self.transformPoint(RealPoint.Add(p2,center)),
          self.transformPoint(RealPoint.Add(p3,center)),
          self.transformPoint(RealPoint.Add(p4,center)));

      theta1 := theta1 + 180.0;
      p1 := p4;
    END;

    (* the remaining arc is smaller that 180 degrees *)
    p4[0] := radius *FLOAT(Math.cos(DegToRad * FLOAT(theta2,LONGREAL)),REAL);
    p4[1] := radius *FLOAT(Math.sin(DegToRad * FLOAT(theta2,LONGREAL)),REAL);

    (* p2 and p3 are perpendicular to p1 and p4. Length? *)
    VAR
      length: REAL;
      theta := theta2 - theta1;
    BEGIN
      IF theta >= 180.0 THEN length := 4.0/3.0;
      ELSIF theta < 90.0 THEN
        length := 2.0/3.0 * 
          FLOAT((1.0D0 - Math.sqrt(1.0D0 - Math.sin(DegToRad *
                 FLOAT(theta,LONGREAL)))),REAL);
      ELSE
        length := 2.0/3.0 *
          FLOAT((1.0D0 + Math.sqrt(1.0D0 - Math.sin(DegToRad * 
                 FLOAT(theta,LONGREAL)))),REAL);
      END;
      p2[0] := -length * p1[1];
      p2[1] := length * p1[0];
      p3[0] := -length * p4[1];
      p3[1] := length * p4[0];
    END;

    Path.CurveTo(
        self.path,
        self.transformPoint(RealPoint.Add(p2,center)),
        self.transformPoint(RealPoint.Add(p3,center)),
        self.transformPoint(RealPoint.Add(p4,center)));
  END ConvertToPathArc;

PROCEDURE RealPathToPath(READONLY realPath: RealPath.T; 
                         READONLY matrix := RealTransform.Identity): Path.T =
  VAR
    path := NEW(Path.T);
  BEGIN
    Path.Reset(path);

    realPath.map(NEW(ConvertToPathClosure, path := path, matrix := matrix));

    RETURN path;
  END RealPathToPath;

CONST DegToRad = FLOAT(2.0 * Math.Pi / 360.0, LONGREAL);
BEGIN
END PathExtra.

