(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: Import.i3,v 1.2 2001-09-19 15:03:34 wagner Exp $ *)

INTERFACE Import;
TYPE
  T <: Public;
  Public = OBJECT METHODS
    init(): T;
    addModule(MN: TEXT); (* add module if it is not already added *)
    addType(m3typeName: TEXT); (* addModule(MN) if m3typeName = "MN.*" *)
    toDeclaration(): TEXT; (* for each MN: IMPORT MN; *)
  END;
END Import.
