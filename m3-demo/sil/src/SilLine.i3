(* Copyright (C) 1994, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(* Last modified on Fri Nov  4 15:08:07 PST 1994 by kalsow    *)

INTERFACE SilLine;

IMPORT Point, SilObject;

TYPE
  T <: Tx;
  Tx = SilObject.T OBJECT METHODS
    init (READONLY a, b: Point.T;  width: INTEGER): T;
  END;

END SilLine.


