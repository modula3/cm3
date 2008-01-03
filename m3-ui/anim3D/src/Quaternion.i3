(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Tue Jun 21 21:10:21 PDT 1994 by najork                   *)
(*       Created on Tue Jun 21 16:14:58 PDT 1994 by najork                   *)


INTERFACE Quaternion;

IMPORT Matrix4;

TYPE 
  T = RECORD
    a, b, c, d : REAL;
  END;

PROCEDURE FromMatrix4 (READONLY M : Matrix4.T) : T;
(* "FromMatrix4(M)" takes an orthonormal matrix "M" and returns the 
   corresponding quaternion "q". "q" will have unit norm. *)

PROCEDURE ToMatrix4 (q : T) : Matrix4.T;
(* "ToMatrix4(q)" takes a unit norm quaternion and returns an orthonormal 
   matrix "M" that corresponds to this quaternion. *)

PROCEDURE Interpolate (q : T; f : REAL) : T;
(* "Interpolate (q,f)" takes a unit norm quaternion "q = (a,b,c,d)" and a real
   "f" between 0 and 1, and returns a unit norm quaternion. Mapping 
   "Interpolate" over the range [0,1] and applying "ToMatrix4" to the results 
   yields a series of matrices that describe a ``smooth'' rotation, with 
   "Matrix4.Id" as the initial matrix and "ToMatrix(q)" as the final matrix. *)

END Quaternion.
