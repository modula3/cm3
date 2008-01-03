(* Copyright (C) 1994, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(* Last modified on Fri Nov  4 16:05:56 PST 1994 by kalsow    *)

INTERFACE SilArc;

IMPORT Point, SilObject;

TYPE
  T <: Tx;
  Tx = SilObject.T OBJECT METHODS
    init (READONLY c, r, p0: Point.T;  width: INTEGER): T;
  END;

END SilArc.


