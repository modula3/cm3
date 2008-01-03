(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Wed May 12 12:16:38 PDT 1993 by swart      *)
(*      modified on Mon Nov 18 22:13:40 PST 1991 by gnelson    *)
(*      modified on Thu Nov  2 18:28:28 1989 by muller         *)
(*      modified on Mon Oct  2 09:30:53 1989 by kalsow         *)
(*      modified on Thu Dec  3  0:04:41 PST 1987 by luca       *)

MODULE Point;

IMPORT Word, Axis;

PROCEDURE FromCoords (h, v: INTEGER): T RAISES {} =
  VAR p: T;
  BEGIN
    p.h := h;
    p.v := v;
    RETURN p;
  END FromCoords;

PROCEDURE FromAxes (axis: Axis.T; n, m: INTEGER): T RAISES {} =
  VAR p: T;
  BEGIN
    CASE axis OF
    | Axis.T.Hor => p.h := n; p.v := m;
    | Axis.T.Ver => p.h := m; p.v := n;
    END;
    RETURN p;
  END FromAxes;

PROCEDURE DistSquare (READONLY p, q: T): INTEGER RAISES {} =
  VAR dh, dv: INTEGER;
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

PROCEDURE Mul (READONLY p: T; n: INTEGER): T RAISES {} =
  VAR q: T;
  BEGIN
    q.h := p.h * n;
    q.v := p.v * n;
    RETURN q;
  END Mul;

PROCEDURE Div (READONLY p: T; n: INTEGER): T RAISES {} =
  VAR q: T;
  BEGIN
    q.h := p.h DIV n;
    q.v := p.v DIV n;
    RETURN q;
  END Div;

PROCEDURE Mod (READONLY p: T; n: INTEGER): T RAISES {} =
  VAR q: T;
  BEGIN
    q.h := p.h MOD n;
    IF q.h < 0 THEN q.h := q.h + n END;
    q.v := p.v MOD n;
    IF q.v < 0 THEN q.v := q.v + n END;
    RETURN q;
  END Mod;

PROCEDURE Scale (READONLY p: T; num, den: INTEGER): T RAISES {} =
  VAR q: T;
  BEGIN
    q.h := (p.h * num) DIV den;
    q.v := (p.v * num) DIV den;
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

PROCEDURE MoveH (READONLY p: T; dh: INTEGER): T RAISES {} =
  VAR r: T;
  BEGIN
    r.h := p.h + dh;
    r.v := p.v;
    RETURN r;
  END MoveH;

PROCEDURE MoveV (READONLY p: T; dv: INTEGER): T RAISES {} =
  VAR r: T;
  BEGIN
    r.h := p.h;
    r.v := p.v + dv;
    RETURN r;
  END MoveV;

PROCEDURE MoveHV (READONLY p: T; dh, dv: INTEGER): T RAISES {} =
  VAR r: T;
  BEGIN
    r.h := p.h + dh;
    r.v := p.v + dv;
    RETURN r;
  END MoveHV;

PROCEDURE Transpose(READONLY p: T; ax := Axis.T.Ver): T =
  BEGIN
    IF ax = Axis.T.Hor THEN RETURN p ELSE RETURN T{p.v,p.h} END
  END Transpose;
  
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

PROCEDURE Compare (READONLY a, b: T): [-1..1] =
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

PROCEDURE Hash (READONLY a: T): INTEGER =
  BEGIN
    RETURN Word.Xor (a.h, a.v);
  END Hash;

BEGIN
END Point.


