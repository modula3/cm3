(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Tue Aug 22 11:53:41 PDT 1995 by najork                   *)
(*       Created on Wed Mar 16 21:47:10 PST 1994 by najork                   *)


INTERFACE Matrix4;

IMPORT Point3;

TYPE T   = ARRAY [0 .. 3] OF Row;
     Row = ARRAY [0 .. 3] OF REAL;

CONST Id = T {Row {1.0, 0.0, 0.0, 0.0},
              Row {0.0, 1.0, 0.0, 0.0},
              Row {0.0, 0.0, 1.0, 0.0},
              Row {0.0, 0.0, 0.0, 1.0}};

PROCEDURE Multiply (READONLY M, N : T) : T;

PROCEDURE Identity () : T;
PROCEDURE Translate (READONLY M : T; x, y, z : REAL) : T;
PROCEDURE Scale (READONLY M : T; x, y, z : REAL) : T;
PROCEDURE RotateX (READONLY M : T; theta : REAL) : T;
PROCEDURE RotateY (READONLY M : T; theta : REAL) : T;
PROCEDURE RotateZ (READONLY M : T; theta : REAL) : T;
PROCEDURE TransformPoint3 (READONLY M : T; READONLY p : Point3.T) : Point3.T;

PROCEDURE TransformUnitCube (p, a, b, c : Point3.T) : T;

(* This function is useful to map prototypes of geometric objects
   (circles, spheres, disks, cylinders, etc) onto actual instances.
   "TransformUnitCube(p,a,b,c)" returns a matrix "M", such that
\begin{verbatim}
   TransformPoint3(M,Point3.T{0.0,0.0,0.0}) = p
   TransformPoint3(M,Point3.T{1.0,0.0,0.0}) = a
   TransformPoint3(M,Point3.T{0.0,1.0,0.0}) = b
   TransformPoint3(M,Point3.T{0.0,0.0,1.0}) = c
\end{verbatim}

   The above 4 equations over points define a system of linear equations, 
   which can be solved statically (i.e.\ no gaussian elimination is needed 
   at run time). So, calls to "TransformUnitCube" are very cheap.
*)

PROCEDURE UnitSphereMaxSquishFactor (READONLY M : T) : REAL;

EXCEPTION Error;

PROCEDURE Decomp (M : T; VAR tx, ty, tz, s : REAL) : T RAISES {Error};
(* "Decompose(M,tx,ty,tz,s,angX,angY,angZ)" takes a matrix "M", which must 
   have been constructed by using only translations, rotations, and uniform(!)
   scalings, and returns values "tx", "ty", "tz", "s", and "R" such that 
   "M = T(tx,ty,tz) S(s) R" holds. If the initial matrix "M" was indeed 
   valid, then "R" is an orthogonal matrix. If "M" was not valid, then
   "Error" is raised. *)

PROCEDURE Transpose (READONLY M : T) : T;
(* "Transpose(M)" takes a matrix "M" and returns its transpose. Note that 
   for an orthonormal matrix, its transpose is also its inverse. *)

PROCEDURE Invert (A : T) : T RAISES {Error};
(* "Invert(M)" takes a matrix M and returns its inverse.  If M is singular,
   the exception "Error" is raised. *)

PROCEDURE Equal (READONLY A, B : T) : BOOLEAN;

PROCEDURE Orthonormal (READONLY M : T) : BOOLEAN;
(* "Orthonormal(M)" is true if the columns of "M" form an orthonormal basis. *)

PROCEDURE OrthoProjMatrix (height, aspect, near, far: REAL): T;
(* This procedure returns what PEX calls a "view mapping matrix", and what
   OpenGL calls a "projection transformation matrix". The projection is
   orthographic. *)

PROCEDURE PerspProjMatrix (fovy, distance, aspect, near, far: REAL): T;
(* Returns a projection matrix for a perspective projection. *)

PROCEDURE LookatViewMatrix (from, to, up: Point3.T): T;
(* Returns a viewing transformation matrix.  We place three 
   (pretty reasonable) restrictions on the arguments:
     (1) "from" differs from "to"
     (2) "up" is non-zero
     (3) "(from - to)" and "up" are not collinear

   "LookatViewMatrix" is similar to Digital PEXlib's "PEXLookatViewMatrix" 
   function and to OpenGL's "gluLookAt" function. *)


END Matrix4.
