(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* by Steve Glassman and Stephen Harrison *)
(* *)
(* Last modified on Fri Aug 19 16:26:41 PDT 1994 by steveg   *)
(*      modified on Sun Jul 19 11:48:17 PDT 1992 by harrison *)

MODULE R2PathExtra;

IMPORT Axis, MG, Path, R2Path, R2, Point, Pts, VBT, Rect, Matrix2D, Math;

(* TO DO:
   - Extending arrays by one each time is too lazy.
   - Fix up index stuff.
*)

REVEAL
  Segments =
    PublicSegments BRANDED OBJECT
    st: VBT.ScreenType := NIL;
  END;

PROCEDURE Cos(READONLY radians: REAL): REAL =
  BEGIN
    RETURN FLOAT(Math.cos(FLOAT(radians, LONGREAL)));
  END Cos;

PROCEDURE Acos(READONLY radians: REAL): REAL =
  BEGIN
    RETURN FLOAT(Math.acos(FLOAT(radians, LONGREAL)));
  END Acos;

PROCEDURE Sin(READONLY radians: REAL): REAL =
  BEGIN
    RETURN FLOAT(Math.sin(FLOAT(radians, LONGREAL)));
  END Sin;

<* UNUSED *>
PROCEDURE Asin(READONLY radians: REAL): REAL =
  BEGIN
    RETURN FLOAT(Math.asin(FLOAT(radians, LONGREAL)));
  END Asin;

PROCEDURE FindArcPoint(READONLY center: R2.T; READONLY radius, angle: REAL): R2.T =
  BEGIN
    RETURN R2.T{center[0] + radius * Cos(angle),
                center[1] + radius * Sin(angle)};
  END FindArcPoint;

TYPE
  TranslatePathClosure = Path.MapObject OBJECT
    newPath: Path.T := NIL;
    delta := Point.Origin;
  OVERRIDES
    move := TranslateMove;
    line := TranslateLine;
    close := TranslateClose;
    curve := TranslateCurve;
  END;

PROCEDURE TranslateMove(self: TranslatePathClosure; READONLY pt: Point.T) =
  BEGIN
    Path.MoveTo(self.newPath, Point.Add(pt, self.delta));
  END TranslateMove;
  
PROCEDURE TranslateLine(self: TranslatePathClosure; <* UNUSED *> READONLY pt1: Point.T; READONLY pt2: Point.T) =
  BEGIN
    Path.LineTo(self.newPath, Point.Add(pt2, self.delta));
  END TranslateLine;
  
PROCEDURE TranslateClose(self: TranslatePathClosure; <* UNUSED *> READONLY pt1, pt2: Point.T) =
  BEGIN
    Path.Close(self.newPath);
  END TranslateClose;
  
PROCEDURE TranslateCurve(self: TranslatePathClosure; <* UNUSED *> READONLY pt1: Point.T; READONLY pt2, pt3, pt4: Point.T) =
  BEGIN
    Path.CurveTo(self.newPath, Point.Add(pt2, self.delta), Point.Add(pt3, self.delta), Point.Add(pt4, self.delta));
  END TranslateCurve;

PROCEDURE TranslatePath(READONLY path: Path.T; READONLY delta: Point.T): Path.T =
  VAR
    closure := NEW(TranslatePathClosure, newPath := NEW(Path.T), delta := delta);
  BEGIN
    TRY
      Path.Map(path, closure);
    EXCEPT
    | Path.Malformed =>
      <* ASSERT FALSE *>
    END;

    RETURN closure.newPath;
  END TranslatePath;

(* Does not work yet
PROCEDURE CreateIndex (segments: Segments): REF ARRAY OF INTEGER =
  VAR
    length := (segments.totalSteps + INDEX_RESOLUTION - 1)
                DIV INDEX_RESOLUTION;
    index := NEW(REF ARRAY OF INTEGER, length);
    elem, subpath := 0; (* indices into current element and subpath *)
    x := 0; (* index into index array *)
  BEGIN
    FOR i := 0 TO segments.totalSteps BY INDEX_RESOLUTION DO
      WHILE seg < LAST(segments.elems^) AND step < i DO
        INC(step, segments.elems[seg].steps);
        INC(seg);
      END;
      index[i] := seg;
    END;

    RETURN index;
  END CreateIndex;
*)

TYPE
  CountSegmentsClosure = Path.MapObject OBJECT
    n := 0; (* Number of segments *)
  OVERRIDES
    move := CountMove;
    line := CountLine;
    close := CountClose;
    curve := CountCurve;
  END;

PROCEDURE CountMove(<* UNUSED *> self: CountSegmentsClosure; <* UNUSED *> READONLY pt: Point.T) =
  BEGIN
  END CountMove;
  
PROCEDURE CountLine(self: CountSegmentsClosure; <* UNUSED *> READONLY pt1, pt2: Point.T) =
  BEGIN
    INC(self.n);
  END CountLine;
  
PROCEDURE CountClose(self: CountSegmentsClosure; READONLY pt1, pt2: Point.T) =
  BEGIN
    CountLine(self, pt1, pt2);
  END CountClose;
  
PROCEDURE CountCurve(<* UNUSED *> self: CountSegmentsClosure; <* UNUSED *> READONLY pt1, pt2, pt3, pt4: Point.T) =
  BEGIN
  END CountCurve;

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

TYPE
  CreateSegmentsClosure = Path.MapObject OBJECT
    segments: Segments := NIL;
    current_subpath, current_elem := 0;
    next := 0;
  OVERRIDES
    move := CreateMove;
    line := CreateLine;
    close := CreateClose;
    curve := CreateCurve;
  END;

PROCEDURE CreateMove(self: CreateSegmentsClosure; READONLY pt: Point.T) =
  BEGIN
    IF self.segments.subPaths = NIL THEN
      (* Create a new subpath array *)
      self.segments.subPaths := NEW(REF ARRAY OF SubPath, 1);
    ELSE
      INC(self.current_subpath);
      self.current_elem := 0;
      IF self.current_subpath > LAST(self.segments.subPaths^) THEN
        (* Extend the current subpath *)
        WITH new = NEW(REF ARRAY OF SubPath, NUMBER(self.segments.subPaths^) + 1) DO
          SUBARRAY(new^, 0, NUMBER(self.segments.subPaths^)) := self.segments.subPaths^;
          self.segments.subPaths := new;
        END;
      END;
    END;

    self.segments.subPaths[self.current_subpath] := SubPath{pt, NIL, FALSE};
  END CreateMove;
  
PROCEDURE CreateLine(self: CreateSegmentsClosure; READONLY pt1, pt2: Point.T) =
  VAR
    steps := MAX(ABS(pt1.h - pt2.h), ABS(pt1.v - pt2.v));
  BEGIN
    WITH subPath = self.segments.subPaths[self.current_subpath] DO
      IF subPath.elems = NIL THEN
        (* First element in subpath *)
        subPath.elems := NEW(REF ARRAY OF Element, 1);
        self.current_elem := 0;
      ELSE
        INC(self.current_elem);
        IF self.current_elem > LAST(subPath.elems^) THEN
          (* Extend the element array *)
          WITH new = NEW(REF ARRAY OF Element, NUMBER(subPath.elems^) + 1) DO
            SUBARRAY(new^, 0, NUMBER(subPath.elems^)) := subPath.elems^;
            subPath.elems := new;
          END;
	END;
      END;

      subPath.elems[self.current_elem] := Element{pt2, steps};
      IF self.current_elem > 0 THEN
        (* Don't count vertex steps twice *)
        DEC(subPath.elems[self.current_elem].steps);
      END;
    END;
  END CreateLine;
  
PROCEDURE CreateClose(self: CreateSegmentsClosure; READONLY pt1, pt2: Point.T) =
  BEGIN
    CreateLine(self, pt1, pt2);
    INC(self.current_subpath);
  END CreateClose;
  
PROCEDURE CreateCurve(<* UNUSED *> self: CreateSegmentsClosure; <* UNUSED *> READONLY pt1, pt2, pt3, pt4: Point.T) =
  BEGIN
    (* Ignore---not present, we hope. *)
    <* ASSERT FALSE *>
  END CreateCurve;

PROCEDURE MakeSegments(v: MG.V; READONLY path: Path.T): Segments =
  VAR
    segments := NEW(Segments);
    createSegments := NEW(CreateSegmentsClosure, segments := segments);
  BEGIN
    (* First count the number segments *)
    TRY
      Path.Map(path, createSegments);
    EXCEPT
    | Path.Malformed =>
    END;
        
    segments.st := VBT.ScreenTypeOf(v);
(* !!!
    segments.index := CreateIndex(segments);
*)

    RETURN segments;
  END MakeSegments;

(* The strategy here is to first convert /R2Path/ to a Path.T, then
   flatten it (the Path.T) and create a Segments object from the resulting
   line segments. *)

TYPE
  ConvertToPathClosure = R2Path.MapObject OBJECT
    v: MG.V := NIL;
    path: Path.T := NIL;
    matrix := Matrix2D.Identity;
  METHODS
    toScreenPoint(READONLY pt: R2.T): Point.T := ToScreenPoint;
  OVERRIDES
    move   := ConvertToPathMove;
    line   := ConvertToPathLine;
    arc    := ConvertToPathArc;
    close  := ConvertToPathClose;
    curve  := ConvertToPathCurve;
  END;

PROCEDURE ToScreenPoint(self: ConvertToPathClosure; READONLY pt: R2.T): Point.T =
  BEGIN
    RETURN MG.MGCToScreenPoint(self.v, Matrix2D.Transform(self.matrix, pt));
  END ToScreenPoint;

PROCEDURE ConvertToPathMove(self: ConvertToPathClosure; READONLY r: R2.T) =
  BEGIN
    Path.MoveTo(self.path, self.toScreenPoint(r));
  END ConvertToPathMove;

PROCEDURE ConvertToPathLine (self: ConvertToPathClosure;
                             READONLY r: R2.T) =
  BEGIN
    Path.LineTo(self.path, self.toScreenPoint(r));
  END ConvertToPathLine;

PROCEDURE ConvertToPathArc (         self  : ConvertToPathClosure;
                            READONLY center: R2.T;
                            READONLY radius, ang1, ang2: REAL) =
  BEGIN
    (* Draw the first line segment from the current point to the start of
       the arc *)
    Path.LineTo(
      self.path, self.toScreenPoint(FindArcPoint(center, radius, ang1)));

    (* Now find the number of segments in the arc.  Start by finding out
       how large half a pixel is in our coordinate system. *)
    WITH inv  = Matrix2D.Inverse(self.matrix),
         a_pt = Matrix2D.Transform(inv, R2.T{0.0, 0.0}),
         b_pt = Matrix2D.Transform(inv, R2.T{0.0, 1.0}),
         d    = R2.Sub(a_pt, b_pt),
         half_a_pixel = 1.0
                          / (2.0 * R2.Length(
                               R2.T{Pts.ToPixels(self.v, d[0], Axis.T.Hor),
                                    Pts.ToPixels(self.v, d[1], Axis.T.Ver)})),
         alpha    = 2.0 * Acos(1.0 - half_a_pixel / radius),
         sweep    = ang2 - ang1,
         segments = MIN(100, MAX(1, ROUND(ABS(sweep) / alpha))) DO
      FOR i := 1 TO segments DO
        WITH angle     = ang1 + sweep * FLOAT(i) / FLOAT(segments),
             arc_point = FindArcPoint(center, radius, angle)        DO
          Path.LineTo(self.path, self.toScreenPoint(arc_point));
        END;                    (* with *)
      END;                      (* for *)
    END;                        (* with *)
  END ConvertToPathArc;

PROCEDURE ConvertToPathClose (self: ConvertToPathClosure) =
  BEGIN
    Path.Close(self.path);
  END ConvertToPathClose;

PROCEDURE ConvertToPathCurve (self: ConvertToPathClosure;
                              READONLY pt2, pt3, pt4: R2.T) =
  BEGIN
    Path.CurveTo(
        self.path,
        self.toScreenPoint(pt2),
        self.toScreenPoint(pt3),
        self.toScreenPoint(pt4));
  END ConvertToPathCurve;

PROCEDURE R2PathToPath(v: MG.V; READONLY R2Path: R2Path.T; READONLY matrix := Matrix2D.Identity): Path.T =
  VAR
    path := NEW(Path.T);
  BEGIN
    Path.Reset(path);

    R2Path.map(NEW(ConvertToPathClosure, v := v, path := path, matrix := matrix));

    RETURN path;
  END R2PathToPath;

PROCEDURE PathToSegments(v: MG.V; READONLY path: Path.T): Segments =
  BEGIN
    TRY
      RETURN MakeSegments(v, Path.Flatten(path));
    EXCEPT
    | Path.Malformed =>
      <* ASSERT FALSE *>
    END;
  END PathToSegments;

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

BEGIN
END R2PathExtra.

