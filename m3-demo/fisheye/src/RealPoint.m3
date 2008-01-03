(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Jun 16 16:46:24 PDT 1992 by muller                   *)

MODULE RealPoint;

IMPORT Axis, Point;

PROCEDURE FromCoords (h, v: REAL): T RAISES {} =
  VAR p: T;
  BEGIN
    p.h := h;
    p.v := v;
    RETURN p;
  END FromCoords;

PROCEDURE FromAxes (axis: Axis.T; n, m: REAL): T RAISES {} =
  VAR p: T;
  BEGIN
    CASE axis OF
    | Axis.T.Hor => p.h := n; p.v := m;
    | Axis.T.Ver => p.h := m; p.v := n;
    END;
    RETURN p;
  END FromAxes;

PROCEDURE Float(p: Point.T): T =
  VAR q: T;
  BEGIN
    q.h := FLOAT(p.h);
    q.v := FLOAT(p.v);
    RETURN q
  END Float;

PROCEDURE Trunc(p: T): Point.T =
  VAR q: Point.T;
  BEGIN
    q.h := TRUNC(p.h);
    q.v := TRUNC(p.v);
    RETURN q
  END Trunc;

PROCEDURE Floor(p: T): Point.T =
  VAR q: Point.T;
  BEGIN
    q.h := TRUNC(p.h); IF FLOAT(q.h) > p.h THEN DEC(q.h) END;
    q.v := TRUNC(p.v); IF FLOAT(q.v) > p.v THEN DEC(q.v) END;
    RETURN q
  END Floor;

PROCEDURE Round(p: T): Point.T =
  VAR q: Point.T;
  BEGIN
    IF p.h < 0.0 THEN q.h := TRUNC(p.h - 0.5) ELSE q.h := TRUNC(p.h + 0.5) END;
    IF p.v < 0.0 THEN q.v := TRUNC(p.v - 0.5) ELSE q.v := TRUNC(p.v + 0.5) END;
    RETURN q
  END Round;

PROCEDURE DistSquare (READONLY p, q: T): REAL RAISES {} =
  VAR dh, dv: REAL;
  BEGIN
    dh := p.h - q.h;
    dv := p.v - q.v;
    RETURN dh * dh + dv * dv;
  END DistSquare;

PROCEDURE Minus (READONLY p: T): T RAISES {} =
  VAR q: T;
  BEGIN
    q.h :=  -p.h;
    q.v :=  -p.v;
    RETURN q;
  END Minus;

PROCEDURE Add (READONLY p, q: T): T RAISES {} =
  VAR r: T;
  BEGIN
    r.h := p.h + q.h;
    r.v := p.v + q.v;
    RETURN r;
  END Add;

PROCEDURE Sub (READONLY p, q: T): T RAISES {} =
  VAR r: T;
  BEGIN
    r.h := p.h - q.h;
    r.v := p.v - q.v;
    RETURN r;
  END Sub;

PROCEDURE Mul (READONLY p: T; n: REAL): T RAISES {} =
  VAR q: T;
  BEGIN
    q.h := p.h * n;
    q.v := p.v * n;
    RETURN q;
  END Mul;

PROCEDURE Div (READONLY p: T; n: REAL): T RAISES {} =
  VAR q: T;
  BEGIN
    q.h := p.h / n;
    q.v := p.v / n;
    RETURN q;
  END Div;

PROCEDURE Mod (READONLY p: T; n: REAL): T RAISES {} =
  VAR q: T;
  BEGIN
    q.h := p.h - FLOAT(TRUNC(p.h/n))*n;
    WHILE q.h > n   DO q.h := q.h - n END;
    WHILE q.h < 0.0 DO q.h := q.h + n END;
    q.v := p.v - FLOAT(TRUNC(p.v/n))*n;
    WHILE q.v > n   DO q.v := q.v - n END;
    WHILE q.v < 0.0 DO q.v := q.v + n END;
    RETURN q
  END Mod;

PROCEDURE Scale (READONLY p: T; num, den: REAL): T RAISES {} =
  VAR q: T;
  BEGIN
    q.h := (p.h * num) / den;
    q.v := (p.v * num) / den;
    RETURN q;
  END Scale;

PROCEDURE Min (READONLY p, q: T): T RAISES {} =
  VAR r: T;
  BEGIN
    r.h := MIN (p.h, q.h);
    r.v := MIN (p.v, q.v);
    RETURN r;
  END Min;

PROCEDURE Max (READONLY p, q: T): T RAISES {} =
  VAR r: T;
  BEGIN
    r.h := MAX (p.h, q.h);
    r.v := MAX (p.v, q.v);
    RETURN r;
  END Max;

PROCEDURE Move (READONLY p, dp: T): T RAISES {} =
  VAR r: T;
  BEGIN
    r.h := p.h + dp.h;
    r.v := p.v + dp.v;
    RETURN r;
  END Move;

PROCEDURE MoveH (READONLY p: T; dh: REAL): T RAISES {} =
  VAR r: T;
  BEGIN
    r.h := p.h + dh;
    r.v := p.v;
    RETURN r;
  END MoveH;

PROCEDURE MoveV (READONLY p: T; dv: REAL): T RAISES {} =
  VAR r: T;
  BEGIN
    r.h := p.h;
    r.v := p.v + dv;
    RETURN r;
  END MoveV;

PROCEDURE MoveHV (READONLY p: T; dh, dv: REAL): T RAISES {} =
  VAR r: T;
  BEGIN
    r.h := p.h + dh;
    r.v := p.v + dv;
    RETURN r;
  END MoveHV;

PROCEDURE Equal (READONLY p, q: T): BOOLEAN RAISES {} =
  BEGIN
    RETURN (p.h = q.h) AND (p.v = q.v);
  END Equal;

PROCEDURE Less (READONLY p, q: T): BOOLEAN RAISES {} =
  BEGIN
    RETURN (p.h < q.h) AND (p.v < q.v);
  END Less;

PROCEDURE LessEq (READONLY p, q: T): BOOLEAN RAISES {} =
  BEGIN
    RETURN (p.h <= q.h) AND (p.v <= q.v);
  END LessEq;

PROCEDURE New (READONLY value: T): REF T =
  VAR r: REF T;
  BEGIN
    r := NEW (REF T);
    r^ := value;
    RETURN r;
  END New;

PROCEDURE NewArray (size: CARDINAL; READONLY value := Origin): REF ARRAY OF T =
  VAR arr: REF ARRAY OF T;
  BEGIN
    arr := NEW (REF ARRAY OF T, size);
    (* Assumes the allocator initializes to Origin automatically: *)
    IF value # Origin THEN
      FOR i := 0 TO size - 1 DO arr[i] := value END;
    END;
    RETURN arr
  END NewArray;

PROCEDURE UntracedNew (READONLY value: T): UNTRACED REF T =
  VAR r: UNTRACED REF T;
  BEGIN
    r := NEW (UNTRACED REF T);
    r^ := value;
    RETURN r;
  END UntracedNew;

PROCEDURE UntracedNewArray (size: CARDINAL;  READONLY value := Origin):
                                                    UNTRACED REF ARRAY OF T =
  VAR arr: UNTRACED REF ARRAY OF T;
  BEGIN
    arr := NEW (UNTRACED REF ARRAY OF T, size);
    (* Assumes the allocator initializes to Origin automatically: *)
    IF value # Origin THEN
      FOR i := 0 TO size - 1 DO arr[i] := value END;
    END;
    RETURN arr
  END UntracedNewArray;

PROCEDURE Compare (READONLY a, b: T): INTEGER =
  BEGIN
    IF (a.h < b.h) THEN
      RETURN  -1;
    ELSIF (a.h > b.h) THEN
      RETURN  +1;
    ELSIF (a.v = b.v) THEN
      RETURN 0;
    ELSIF (a.v < b.v) THEN
      RETURN  -1;
    ELSE
      RETURN  +1;
    END;
  END Compare;

PROCEDURE Lt (READONLY a, b: T): BOOLEAN =
  BEGIN
    RETURN (a.h < b.h) OR ((a.h = b.h) AND (a.v < b.v));
  END Lt;

PROCEDURE Eq (READONLY a, b: T): BOOLEAN =
  BEGIN
    RETURN (a.h = b.h) AND (a.v = b.v);
  END Eq;

PROCEDURE Hash (READONLY a: T): INTEGER =
  BEGIN
    RETURN ROUND(a.h * a.v);
  END Hash;

BEGIN
END RealPoint.
