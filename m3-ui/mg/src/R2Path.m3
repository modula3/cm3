(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Stephen Harrison and Steve Glassman *)
(*                                                                           *)
(* Last modified on Fri Aug 19 16:35:10 PDT 1994 by steveg                   *)
(*      modified on Sun Jul 19 12:08:11 PDT 1992 by harrison                 *)

(* After Path.m3---see that file for authors. *)

<*PRAGMA LL*>

MODULE R2Path;

IMPORT Math, Matrix2D, R2, R2Box;

CONST
  PATH_INITIAL_ELEMENTS = 1;
  PATH_GROWTH_FACTOR = 1.61;		 (* The golden gizmo *)

TYPE
  Element = BRANDED OBJECT END;

  LineElem = Element BRANDED OBJECT
    pt: R2.T;
  END;

  MoveElem = Element BRANDED OBJECT
    pt: R2.T;
  END;

  CloseElem = Element BRANDED OBJECT
    pt: R2.T;
  END;

  CurveElem = Element BRANDED OBJECT
    p, q, r: R2.T;
  END;

  ArcElem = Element BRANDED OBJECT
    center: R2.T;
    radius, ang1, ang2: REAL
  END;

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
  END;

PROCEDURE Init(self: T) =
  BEGIN
    self.elemList := NIL;
    self.nextElement := 0;
  END Init;

PROCEDURE MoveTo(self: T; READONLY pt: R2.T) =
  BEGIN
    Append(self, NEW(MoveElem, pt := pt));
  END MoveTo;

PROCEDURE LineTo(self: T; READONLY pt: R2.T) =
  BEGIN
    Append(self, NEW(LineElem, pt := pt));
  END LineTo;

PROCEDURE ArcTo(self: T;
                READONLY center: R2.T;
                READONLY radius, ang1, ang2: REAL) =
  BEGIN
    Append(self, NEW(ArcElem, center := center, radius := radius, ang1 := ang1, ang2 := ang2));
  END ArcTo;

PROCEDURE CurveTo(self: T; READONLY p, q, r: R2.T) =
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

PROCEDURE Translate(self: T; READONLY delta: R2.T): T =
  VAR
    newPath := Copy(self);
  BEGIN
    FOR i := 0 TO newPath.nextElement - 1 DO
      TYPECASE newPath.elemList[i] OF
      | MoveElem(e) =>
          e.pt := R2.Add(e.pt, delta);
      | LineElem(e) =>
          e.pt := R2.Add(e.pt, delta);
      | CloseElem(e) =>
          e.pt := R2.Add(e.pt, delta);
      | CurveElem(e) =>
          e.p := R2.Add(e.p, delta);
          e.q := R2.Add(e.q, delta);
          e.r := R2.Add(e.r, delta);
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

PROCEDURE FindArcPoint(READONLY center: R2.T; READONLY radius, angle: REAL): R2.T =
  BEGIN
    RETURN R2.T{center[0] + radius * FLOAT(Math.cos(FLOAT(angle, LONGREAL))),
		center[1] + radius * FLOAT(Math.sin(FLOAT(angle, LONGREAL)))};
  END FindArcPoint;

PROCEDURE BBox(self: T; READONLY matrix := Matrix2D.Identity): R2Box.T =
  VAR
    bounds := R2Box.Empty;

  PROCEDURE extendBBox(READONLY pt: R2.T) =
    BEGIN
      bounds := R2Box.Extend(bounds, Matrix2D.Transform(matrix, pt));
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
	  extendBBox(FindArcPoint(e.center, e.radius, e.ang1));
	  extendBBox(FindArcPoint(e.center, e.radius, e.ang2));
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

BEGIN
  <* ASSERT PATH_GROWTH_FACTOR > 0.0 *>
  <* ASSERT PATH_INITIAL_ELEMENTS > 0 *>
END R2Path.
