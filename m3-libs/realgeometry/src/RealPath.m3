(* Copyright (C) 1992, Digital Equipment Corporation                 *)
(* All rights reserved.                                              *)
(* See the file COPYRIGHT for a full description.                    *)
(*                                                                   *)

(* Contributed by Michel Dagenais (dagenais@vlsi.polymtl.ca), 1994. *)

(* After Path.m3---see that file for authors. *)

<*PRAGMA LL*>

MODULE RealPath;

IMPORT Math, RealTransform, RealPoint, RealRect;

CONST
  PATH_INITIAL_ELEMENTS = 1;
  PATH_GROWTH_FACTOR = 1.61;		 (* The golden gizmo *)

TYPE
  ElemList = REF ARRAY OF Element;

REVEAL
  T = PublicT BRANDED OBJECT
    elemList: ElemList := NIL;		 (* Empty path *)
    nextElement := 0;			 (* Where to place the next element *)
  OVERRIDES
    init := Init;
    moveTo := MoveTo;
    lineTo := LineTo;
    arcTo := ArcTo;
    curveTo := CurveTo;
    close := Close;
    isEmpty := IsEmpty;
    translate := Translate;
    map := Map;
    bbox := BBox;
    nbElement := NbElement;
    insertElement := InsertElement;
    replaceElement := ReplaceElement;
    removeElement := RemoveElement;
    getElement := GetElement;
    currentPoint := CurrentPoint;
  END;

PROCEDURE Init(self: T) =
  BEGIN
    self.elemList := NIL;
    self.nextElement := 0;
  END Init;

PROCEDURE MoveTo(self: T; READONLY pt: RealPoint.T) =
  BEGIN
    Append(self, NEW(MoveElem, pt := pt));
  END MoveTo;

PROCEDURE LineTo(self: T; READONLY pt: RealPoint.T) =
  BEGIN
    Append(self, NEW(LineElem, pt := pt));
  END LineTo;

PROCEDURE ArcTo(self: T;
                READONLY center: RealPoint.T;
                READONLY radius, ang1, ang2: REAL) =
  BEGIN
    Append(self, NEW(ArcElem, center := center, radius := radius, ang1 := ang1, ang2 := ang2));
  END ArcTo;

PROCEDURE CurveTo(self: T; READONLY p, q, r: RealPoint.T) =
  BEGIN
    Append(self, NEW(CurveElem, p := p, q := q, r := r));
  END CurveTo;

PROCEDURE Close(self: T) =
  BEGIN
    Append(self, NEW(CloseElem));
  END Close;

PROCEDURE Append(VAR path: T; READONLY elem: Element) =
  BEGIN
    IF path.elemList = NIL THEN
      path.elemList := NEW(ElemList, PATH_INITIAL_ELEMENTS);
      path.nextElement := 0;
    END; (* if *)

    IF path.nextElement > LAST(path.elemList^) THEN
      VAR
        (* Increase size by at least 1 each time *)
        l := MAX(NUMBER(path.elemList^) + 1,
                 ROUND(FLOAT(NUMBER(path.elemList^)) * PATH_GROWTH_FACTOR));
        newPath := NEW(ElemList, l);
      BEGIN
        SUBARRAY(newPath^, 0, NUMBER(path.elemList^)) := path.elemList^;
        path.elemList := newPath;
      END (* block *)
    END; (* if *)

    path.elemList[path.nextElement] := elem;
    INC(path.nextElement);
  END Append;

PROCEDURE IsEmpty(self: T): BOOLEAN =
  BEGIN
    RETURN self.elemList = NIL OR self.nextElement = 0;
  END IsEmpty;

PROCEDURE CurrentPoint (self: T): RealPoint.T
  RAISES {NoCurrentPoint} =
  VAR
    pt: RealPoint.T;
  BEGIN
    IF self.nextElement = 0 THEN RAISE NoCurrentPoint; END;

    TYPECASE self.elemList[self.nextElement - 1] OF
    | MoveElem(e) =>
        RETURN e.pt;
    | LineElem(e) =>
        RETURN e.pt;
    | CloseElem =>
        RAISE NoCurrentPoint;
    | CurveElem(e) =>
        RETURN e.r;
    | ArcElem(e) =>
        pt[0] := e.center[0] + e.radius * 
                 FLOAT(Math.cos(DegToRad * FLOAT(e.ang2,LONGREAL)),REAL);
        pt[1] := e.center[1] + e.radius * 
                 FLOAT(Math.sin(DegToRad * FLOAT(e.ang2,LONGREAL)),REAL);
        RETURN pt;
    ELSE
      <* ASSERT FALSE *>
    END; (* typecase *)
  END CurrentPoint;

PROCEDURE Translate(self: T; READONLY delta: RealPoint.T): T =
  VAR
    newPath := Copy(self);
  BEGIN
    FOR i := 0 TO newPath.nextElement - 1 DO
      TYPECASE newPath.elemList[i] OF
      | MoveElem(e) =>
          e.pt := RealPoint.Add(e.pt, delta);
      | LineElem(e) =>
          e.pt := RealPoint.Add(e.pt, delta);
      | CloseElem(e) =>
          e.pt := RealPoint.Add(e.pt, delta);
      | CurveElem(e) =>
          e.p := RealPoint.Add(e.p, delta);
          e.q := RealPoint.Add(e.q, delta);
          e.r := RealPoint.Add(e.r, delta);
      | ArcElem(e) =>
          e.center := RealPoint.Add(e.center, delta);
      ELSE
        <* ASSERT FALSE *>
      END; (* typecase *)
    END; (* for *)

    RETURN newPath;
  END Translate;

PROCEDURE Copy(self: T): T =
  VAR
    newPath := NEW(T);
  BEGIN
    Init(self);

    FOR i := 0 TO self.nextElement - 1 DO
      TYPECASE self.elemList[i] OF
      | MoveElem(e) =>
          Append(newPath, NEW(MoveElem, pt := e.pt));
      | LineElem(e) =>
          Append(newPath, NEW(LineElem, pt := e.pt));
      | ArcElem(e) =>
          Append(newPath, NEW(ArcElem, center := e.center, radius := e.radius, ang1 := e.ang1, ang2 := e.ang2));
      | CloseElem(e) =>
          Append(newPath, NEW(CloseElem, pt := e.pt));
      | CurveElem(e) =>
          Append(newPath, NEW(CurveElem, p := e.p, q := e.q, r := e.r));
      ELSE
        <* ASSERT FALSE *>
      END; (* typecase *)
    END; (* for *)

    RETURN newPath;
  END Copy;

PROCEDURE Map(self: T; map: MapObject) =
  BEGIN
    FOR i := 0 TO self.nextElement - 1 DO
      TYPECASE self.elemList[i] OF
      | MoveElem(e) =>
          map.move(e.pt);
      | LineElem(e) =>
          map.line(e.pt);
      | CloseElem =>
          map.close();
      | ArcElem(e) =>
          map.arc(e.center, e.radius, e.ang1, e.ang2)
      | CurveElem(e) =>
          map.curve(e.p, e.q, e.r);
      ELSE
        <* ASSERT FALSE *>
      END; (* typecase *)
    END; (* for *)
  END Map;

PROCEDURE BBox(self: T; READONLY matrix := RealTransform.Identity): RealRect.T =
  VAR
    bounds := RealRect.Empty;
    min, max : RealPoint.T;

  PROCEDURE extendBBox(READONLY pt: RealPoint.T) =
    BEGIN
      bounds := RealRect.Extend(bounds, RealTransform.Transform(matrix, pt));
    END extendBBox;

  BEGIN
    FOR i := 0 TO self.nextElement - 1 DO
      TYPECASE self.elemList[i] OF
      | MoveElem(e) =>
          extendBBox(e.pt);
      | LineElem(e) =>
          extendBBox(e.pt);
      | CloseElem =>
      | ArcElem(e) =>
          FindArcBBox(e,min,max);
	  extendBBox(min);
	  extendBBox(max);
      | CurveElem(e) =>
          extendBBox(e.p);
          extendBBox(e.q);
          extendBBox(e.r);
      ELSE
        <* ASSERT FALSE *>
      END; (* typecase *)
    END; (* for *)

    RETURN bounds;
  END BBox;

PROCEDURE FindArcBBox(e: ArcElem; VAR min, max: RealPoint.T) =
  VAR
    p1, p2: RealPoint.T;
    ang1 := e.ang1;
    ang2 := e.ang2;
    r: REAL;
  BEGIN
    (* We have a full circle *)
    IF (ang2 - ang1) >= 360.0 THEN
      min[0] := e.center[0] - e.radius;
      min[1] := e.center[1] - e.radius;
      max[0] := e.center[0] + e.radius;
      max[1] := e.center[1] + e.radius;
      RETURN;
    END;

    (* if ang2 is smaller than ang1, add 360 until bigger *)
    IF ang2 < ang1 THEN
      r := FLOAT(CEILING((ang1 - ang2)/360.0),REAL);
      ang2 := ang2 + 360.0 * r;
    END;

    (* normalize for ang1 between 0 and 360 *)
    r := FLOAT(FLOOR(ang1/360.0),REAL);
    ang1 := ang1 - 360.0 * r;
    ang2 := ang2 - 360.0 * r;

    (* get the end points *)
    p1[0] := e.center[0] + e.radius *
             FLOAT(Math.cos(DegToRad * FLOAT(ang1,LONGREAL)),REAL);
    p1[1] := e.center[1] + e.radius * 
             FLOAT(Math.sin(DegToRad * FLOAT(ang1,LONGREAL)),REAL);
    p2[0] := e.center[0] + e.radius * 
             FLOAT(Math.cos(DegToRad * FLOAT(ang2,LONGREAL)),REAL);
    p2[1] := e.center[1] + e.radius * 
             FLOAT(Math.sin(DegToRad * FLOAT(ang2,LONGREAL)),REAL);

    (* start with the end points as bounds *)
    min[0] := MIN(p1[0], p2[0]);
    min[1] := MIN(p1[1], p2[1]);
    max[0] := MAX(p1[0], p2[0]);
    max[1] := MAX(p1[1], p2[1]);

    (* check if we cross any axis direction, in which case the arc
       extends beyond the end point. *)

    IF ang2 > 360.0 THEN max[0] := e.center[0] + e.radius; END;

    IF (ang1 < 90.0)AND(ang2 > 90.0) THEN max[1] := e.center[1] + e.radius;END;

    IF (ang1 < 180.0)AND(ang2 > 180.0) THEN 
      min[0] := e.center[0] - e.radius;
    END;

    IF (ang1 < 270.0)AND(ang2 > 270.0) THEN 
      min[1] := e.center[1] - e.radius; 
    END;

    RETURN

  END FindArcBBox;

PROCEDURE NbElement(self: T): CARDINAL =
  BEGIN
    RETURN self.nextElement;
  END NbElement;

PROCEDURE InsertElement(self: T; e: Element; pos: CARDINAL) 
  RAISES {BadPosition} =
  BEGIN
    (* it really is a append *)
    IF pos = self.nextElement THEN Append(self,e); RETURN; END;
    (* invalid position *)
    IF pos > self.nextElement THEN RAISE BadPosition; END;

    (* is the vector big enough to add one *)
    IF self.nextElement > LAST(self.elemList^) THEN
      VAR
        (* Increase size by at least 1 each time *)
        l := MAX(NUMBER(self.elemList^) + 1,
                 ROUND(FLOAT(NUMBER(self.elemList^)) * PATH_GROWTH_FACTOR));
        newPath := NEW(ElemList, l);
      BEGIN
        SUBARRAY(newPath^, 0, NUMBER(self.elemList^)) := self.elemList^;
        self.elemList := newPath;
      END (* block *)
    END; (* if *)

    (* Now do the insertion *)

    FOR i := self.nextElement TO pos BY -1 DO
      self.elemList[i] := self.elemList[i-1];
    END;

    self.elemList[pos] := e;
  END InsertElement;

PROCEDURE ReplaceElement(self: T; e: Element; pos: CARDINAL) 
  RAISES {BadPosition} =
  BEGIN
    (* invalid position *)
    IF pos >= self.nextElement THEN RAISE BadPosition; END;

    self.elemList[pos] := e;
  END ReplaceElement;

PROCEDURE RemoveElement(self: T; pos: CARDINAL) 
  RAISES {BadPosition} =
  BEGIN
    (* invalid position *)
    IF pos >= self.nextElement THEN RAISE BadPosition; END;

    DEC(self.nextElement);

    FOR i := pos TO self.nextElement - 1 DO
      self.elemList[i] := self.elemList[i+1];
    END;

    self.elemList[self.nextElement] := NIL;
  END RemoveElement;

PROCEDURE GetElement(self: T; pos: CARDINAL): Element 
  RAISES {BadPosition} =
  BEGIN
    (* invalid position *)
    IF pos >= self.nextElement THEN RAISE BadPosition; END;

    RETURN self.elemList[pos];
  END GetElement;

CONST DegToRad = FLOAT(2.0 * Math.Pi / 360.0, LONGREAL);    

BEGIN
  <* ASSERT PATH_GROWTH_FACTOR > 0.0 *>
  <* ASSERT PATH_INITIAL_ELEMENTS > 0 *>
END RealPath.


