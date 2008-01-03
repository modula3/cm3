(* Copyright (C) 1994, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(* Last modified on Fri Nov  4 16:50:19 PST 1994 by kalsow    *)

INTERFACE SilBox;

IMPORT Point, SilObject;

TYPE
  T <: Tx;
  Tx = SilObject.T OBJECT METHODS
    init (READONLY p0, p1: Point.T;  width: INTEGER): T;
  END;

END SilBox.


