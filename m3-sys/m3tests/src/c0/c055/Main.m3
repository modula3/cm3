(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: coverage of arithmetic operations and relations *)

UNSAFE MODULE Main;

TYPE
  A = UNTRACED REF INTEGER;
  SE = {sa, sb, sc, sd, se};
  SS = SET OF SE; (* small set *)
  LS = SET OF [0..255];

VAR
  i, j, k: INTEGER;
  l, m, n: CARDINAL;
  x, y, z: REAL;
  X, Y, Z: LONGREAL;
  U, V, W: EXTENDED;
  a, b, c : A;
  e, f, g : SE;
  s, t, u : SS;
  sx, tx, ux : LS;
  B, C, D : BOOLEAN;

PROCEDURE CheckPlus () =
  BEGIN
    i := + j;
    l := + m;
    x := + y;
    X := + Y;
    U := + V;
    CONST
      i : INTEGER   = + (-23);
      l : CARDINAL  = + 23;
      x : REAL      = + 234.34;
      X : LONGREAL  = + 234.234d3;
      U : EXTENDED  = + 234.2345x4;
    BEGIN EVAL i; EVAL l;  EVAL x;  EVAL X;  EVAL U; END;
  END CheckPlus;

PROCEDURE CheckAdd () =
  BEGIN
    i := j + k;
    l := m + n;
    x := z + y;
    X := X + Y;
    U := U + V;
    a := b + 23;
    u := s + t;
    ux := sx + tx;
    CONST
      i : INTEGER  = 23 + (-34);
      l : CARDINAL = (-34) + 123;
      x : REAL     = 4.0 + 45.34;
      X : LONGREAL = 23.34d1 + 234.45d-1;
      U : EXTENDED = 23.34x1 + 234.45x-1;
      u : SS       = SS{SE.sa, SE.sb} + SS{SE.sc ,SE.sb};
      ux : LS      = LS{0, 254} + LS{63, 65};
      BEGIN EVAL i; EVAL l;  EVAL x;  EVAL X;  EVAL U;  EVAL u;  EVAL ux; END;
  END CheckAdd;
    
PROCEDURE CheckNegate () =
  BEGIN
    i := - j;
    l := - m;
    x := - y;
    X := - Y;
    U := - V;
    CONST
      i : INTEGER  = - 23;
      l : CARDINAL = - (-23);
      x : REAL     = - 234.34;
      X : LONGREAL = - 234.234d3;
      U : EXTENDED = - 234.2345x3;
      BEGIN EVAL i; EVAL l;  EVAL x;  EVAL X;  EVAL U;  END;
  END CheckNegate;
    
PROCEDURE CheckSubtract () =
  BEGIN
    i := j - k;
    l := m - n;
    x := z - y;
    X := X - Y;
    U := U - V;
    a := b - 21;
    i := b - c;
    u := s - t;
    ux := sx - tx;
    CONST
      i : INTEGER  = 23 - 34;
      l : CARDINAL = 72 - 23;
      x : REAL     = 4.0 - 45.34;
      X : LONGREAL = 23.34d1 - 234.45d-1;
      U : EXTENDED = 23.34x1 - 234.45x-1;
      u : SS       = SS{SE.sa, SE.sb} - SS{SE.sc ,SE.sb};
      ux : LS      = LS{0, 254} - LS{63, 65};
      BEGIN EVAL i; EVAL l;  EVAL x;  EVAL X;  EVAL U;  EVAL u;  EVAL ux; END;
  END CheckSubtract;
    
PROCEDURE CheckMultiply () =
  BEGIN
    i := j * k;
    l := m * n;
    x := z * y;
    X := X * Y;
    U := U * V;
    u := s * t;
    ux := sx * tx;
    CONST
      i : INTEGER  = 23 * (-34);
      l : CARDINAL = 34 * 23;
      x : REAL     = 4.0 * 45.34;
      X : LONGREAL = 23.34d1 * 234.45d-1;
      U : EXTENDED = 23.34x1 * 234.45x-1;
      u : SS       = SS{SE.sa, SE.sb} * SS{SE.sc ,SE.sb};
      ux : LS      = LS{0, 254} * LS{63, 65};
      BEGIN EVAL i; EVAL l;  EVAL x;  EVAL X;  EVAL U;  EVAL u;  EVAL ux;  END;
  END CheckMultiply;
    
PROCEDURE CheckDivide () =
  BEGIN
    x := z / y;
    X := X / Y;
    U := U / V;
    u := s / t;
    ux := sx / tx;
    CONST
      x : REAL     = 4.0 / 45.34;
      X : LONGREAL = 23.34d1 / 234.45d-1;
      U : EXTENDED = 23.34x1 / 234.45x-1;
      u : SS       = SS{SE.sa, SE.sb} / SS{SE.sc ,SE.sb};
      ux : LS      = LS{0, 254} / LS{63, 65};
      BEGIN EVAL x; EVAL X;  EVAL U; EVAL u;  EVAL ux;  END;
  END CheckDivide;
    
PROCEDURE CheckDiv () =
  BEGIN
    i := j DIV k;
    l := m DIV n;
    CONST
      i : INTEGER  = -23 DIV 34;
      l : CARDINAL = 34 DIV 23;
      BEGIN EVAL i; EVAL l; END;
  END CheckDiv;
    
PROCEDURE CheckMod () =
  BEGIN
    i := j MOD k;
    l := m MOD n;
    x := z MOD y;
    X := X MOD Y;
    U := U MOD V;
    CONST
      i : INTEGER  = -23 MOD 34;
      l : CARDINAL = 34 MOD 23;
      x : REAL     = 4.0 MOD 45.34;
      X : LONGREAL = 23.34d1 MOD 234.45d-1;
      U : EXTENDED = 23.34x1 MOD 234.45x-1;
      BEGIN EVAL i; EVAL l;  EVAL x;  EVAL X;  EVAL U;  END;
  END CheckMod;
    
PROCEDURE CheckAbs () =
  BEGIN
    i := ABS (k);
    l := ABS (m);
    x := ABS (y);
    X := ABS (Y);
    U := ABS (V);
    CONST
      i : INTEGER  = ABS (-34);
      l : CARDINAL = ABS (-23);
      x : REAL     = ABS (- 45.34);
      X : LONGREAL = ABS (-45.34d12);
      U : EXTENDED = ABS (-45.34x12);
      BEGIN EVAL i; EVAL l;  EVAL x;  EVAL X;  EVAL U;  END;
  END CheckAbs;
    
PROCEDURE CheckFloat () =
  BEGIN
    x := FLOAT (j);
    x := FLOAT (m);
    x := FLOAT (z);
    x := FLOAT (X);
    x := FLOAT (U);
    CONST
      x1 = FLOAT (23);
      x2 = FLOAT (-34);
      x3 = FLOAT (4.0);
      x4 = FLOAT (23.34d1);
      x5 = FLOAT (23.34x1);
      BEGIN EVAL x1;  EVAL x2;  EVAL x3;  EVAL x4;  EVAL x5;  END;
  END CheckFloat;
    
PROCEDURE CheckFloatReal () =
  BEGIN
    x := FLOAT (j, REAL);
    x := FLOAT (m, REAL);
    x := FLOAT (z, REAL);
    x := FLOAT (X, REAL);
    x := FLOAT (U, REAL);
    CONST
      x1 = FLOAT (23, REAL);
      x2 = FLOAT (-34, REAL);
      x3 = FLOAT (4.0, REAL);
      x4 = FLOAT (23.34d1, REAL);
      x5 = FLOAT (23.34x1, REAL);
      BEGIN EVAL x1;  EVAL x2;  EVAL x3;  EVAL x4;  EVAL x5;  END;
  END CheckFloatReal;
    
PROCEDURE CheckFloatLongreal () =
  BEGIN
    X := FLOAT (j, LONGREAL);
    X := FLOAT (m, LONGREAL);
    X := FLOAT (z, LONGREAL);
    X := FLOAT (Y, LONGREAL);
    X := FLOAT (U, LONGREAL);
    CONST
      X1 = FLOAT (23, LONGREAL);
      X2 = FLOAT (-34, LONGREAL);
      X3 = FLOAT (4.0, LONGREAL);
      X4 = FLOAT (23.34d1, LONGREAL);
      X5 = FLOAT (23.345x1, LONGREAL);
      BEGIN EVAL X1;  EVAL X2;  EVAL X3;  EVAL X4;  EVAL X5;  END;
  END CheckFloatLongreal;
    
PROCEDURE CheckFloatExtended () =
  BEGIN
    U := FLOAT (j, EXTENDED);
    U := FLOAT (m, EXTENDED);
    U := FLOAT (z, EXTENDED);
    U := FLOAT (X, EXTENDED);
    U := FLOAT (V, EXTENDED);
    CONST
      X1 = FLOAT (23, EXTENDED);
      X2 = FLOAT (-34, EXTENDED);
      X3 = FLOAT (4.0, EXTENDED);
      X4 = FLOAT (23.34d1, EXTENDED);
      X5 = FLOAT (23.345x1, EXTENDED);
      BEGIN EVAL X1;  EVAL X2;  EVAL X3;  EVAL X4;  EVAL X5;  END;
  END CheckFloatExtended;
    
PROCEDURE CheckFloor () =
  BEGIN
    i := FLOOR (x);
    i := FLOOR (X);
    i := FLOOR (U);
    CONST
      i1 = FLOOR (23.23);
      i2 = FLOOR (23.34d-3);
      i3 = FLOOR (23.345x-3);
      BEGIN EVAL i1;  EVAL i2;  EVAL i3;  END;
  END CheckFloor;
    
PROCEDURE CheckCeiling () =
  BEGIN
    i := CEILING (x);
    i := CEILING (X);
    i := CEILING (U);
    CONST
      i1 = CEILING (23.23);
      i2 = CEILING (23.34d-3);
      i3 = CEILING (23.345x-3);
      BEGIN EVAL i1;  EVAL i2;  EVAL i3;   END;
  END CheckCeiling;
    
PROCEDURE CheckRound () =
  BEGIN
    i := ROUND (x);
    i := ROUND (X);
    i := ROUND (U);
    CONST
      i1 = ROUND (23.23);
      i2 = ROUND (23.34d-3);
      i3 = ROUND (23.345x-3);
      BEGIN EVAL i1;  EVAL i2;  EVAL i3;   END;
  END CheckRound;
    
PROCEDURE CheckTrunc () =
  BEGIN
    i := TRUNC (x);
    i := TRUNC (X);
    i := TRUNC (U);
    CONST
      i1 = TRUNC (23.23);
      i2 = TRUNC (23.34d-3);
      i3 = TRUNC (23.345x-3);
      BEGIN EVAL i1;  EVAL i2;  EVAL i3;   END;
  END CheckTrunc;
    
PROCEDURE CheckMax () =
  BEGIN
    i := MAX (j, k);
    l := MAX (m, n);
    x := MAX (z, y);
    X := MAX (X, Y);
    U := MAX (U, V);
    CONST
      i : INTEGER  = MAX (23, 34);
      l : CARDINAL = MAX (-34, 23);
      x : REAL     = MAX (4.0, 45.34);
      X : LONGREAL = MAX (23.34d1, 234.45d-1);
      U : EXTENDED = MAX (23.345x1, 234.456x-1);
      BEGIN EVAL i; EVAL l;  EVAL x;  EVAL X;  EVAL U;  END;
  END CheckMax;
    
PROCEDURE CheckMin () =
  BEGIN
    i := MIN (j, k);
    l := MIN (m, n);
    x := MIN (z, y);
    X := MIN (X, Y);
    U := MIN (U, V);
    CONST
      i : INTEGER  = MIN (23, -34);
      l : CARDINAL = MIN (34, 23);
      x : REAL     = MIN (4.0, 45.34);
      X : LONGREAL = MIN (23.34d1, 234.45d-1);
      U : EXTENDED = MIN (23.345x1, 234.456x-1);
      BEGIN EVAL i; EVAL l;  EVAL x;  EVAL X;  EVAL U;  END;
  END CheckMin;
    
PROCEDURE CheckRelOp () =
  BEGIN
    B := (i < j)  AND (i <= j) AND (i > j) AND
         (i >= j) AND (i = j)  AND (i # j);
    B := (l < m)  AND (l <= m) AND (l > m) AND
         (l >= m) AND (l = m)  AND (l # m);
    B := (x < y)  AND (x <= y) AND (x > y) AND
         (x >= y) AND (x = y)  AND (x # y);
    B := (X < Y)  AND (X <= Y) AND (X > Y) AND
         (X >= Y) AND (X = Y)  AND (X # Y);
    B := (U < V)  AND (U <= V) AND (U > V) AND
         (U >= V) AND (U = V)  AND (U # V);
    B := (a < b)  AND (a <= b) AND (a > b) AND
         (a >= b) AND (a = b)  AND (a # b);
    B := (s < t)  AND (s <= t) AND (s > t) AND
         (s >= t) AND (s = t)  AND (s # t);
    B := (sx < tx)  AND (sx <= tx) AND (sx > tx) AND
         (sx >= tx) AND (sx = tx)  AND (sx # tx);
    B := (e IN s);
    B := (i IN sx);
    
    CONST
      B1 = (2 < 3)            AND (2 <= 3)           AND (2 > 3) AND
           (2 >= 3)           AND (2 = 3)            AND (2 # 3);
      B2 = (5 < -23)          AND (5 <= -23)         AND (5 > -23) AND
           (5 >= -23)         AND (5 = -23)          AND (5 # -23);
      B3 = (2.3 < 5.7)        AND (2.3 <= 5.7)       AND (2.3 > 5.7) AND
           (2.3 >= 5.7)       AND (2.3 = 5.7)        AND (2.3 # 5.7);
      B4 = (2.3d2 < 34.5d-3)  AND (2.3d2 <= 34.5d-3) AND (2.3d2 > 34.5d-3) AND
           (2.3d2 >= 34.5d-3) AND (2.3d2 = 34.5d-3)  AND (2.3d2 # 34.5d-3);
      B5 = (2.3x2 < 34.5x-3)  AND (2.3x2 <= 34.5x-3) AND (2.3x2 > 34.5x-3) AND
           (2.3x2 >= 34.5x-3) AND (2.3x2 = 34.5x-3)  AND (2.3x2 # 34.5x-3);
      B6 = (SS{SE.sa, SE.sb} <  SS{SE.sc, SE.sb}) AND
           (SS{SE.sa, SE.sb} <= SS{SE.sc, SE.sb}) AND
           (SS{SE.sa, SE.sb} >  SS{SE.sc, SE.sb}) AND
           (SS{SE.sa, SE.sb} >= SS{SE.sc, SE.sb}) AND
           (SS{SE.sa, SE.sb} =  SS{SE.sc, SE.sb}) AND
           (SS{SE.sa, SE.sb} #  SS{SE.sc, SE.sb});
      B6x= (LS{0, 234} <  LS{37, 234}) AND
           (LS{0, 234} <= LS{37, 234}) AND
           (LS{0, 234} >  LS{37, 234}) AND
           (LS{0, 234} >= LS{37, 234}) AND
           (LS{0, 234} =  LS{37, 234}) AND
           (LS{0, 234} #  LS{37, 234});
      B7 = (SE.sa IN SS{SE.sa, SE.sb});
      B7x = (234 IN LS{0, 2, 234, 255});
      BEGIN EVAL B1;  EVAL B2;  EVAL B3; EVAL B4; EVAL B5;
            EVAL B6;  EVAL B6x; EVAL B7; EVAL B7x;  END;
  END CheckRelOp;
    
PROCEDURE CheckBoolOp () =
  BEGIN
    B := NOT C;
    B := C AND D;
    B := C OR D;
    CONST
      B1 = NOT TRUE;
      B2 = TRUE AND FALSE;
      B3 = FALSE OR TRUE;
      BEGIN EVAL B1;  EVAL B2;  EVAL B3;  END;
  END CheckBoolOp;
    

BEGIN
  CheckPlus ();
  CheckAdd ();
  CheckNegate ();
  CheckSubtract ();
  CheckMultiply ();
  CheckDivide ();
  CheckDiv ();
  CheckMod ();
  CheckAbs ();
  CheckFloat ();
  CheckFloatReal ();
  CheckFloatLongreal ();
  CheckFloatExtended ();
  CheckFloor ();
  CheckCeiling ();
  CheckRound ();
  CheckTrunc ();
  CheckMax ();
  CheckMin ();
  CheckRelOp ();
  CheckBoolOp ();
  EVAL Z;
  EVAL W;
  EVAL f;
  EVAL g;
END Main.
