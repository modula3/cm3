(* $Id$ *)

(*
  Copyright (c) 2008, Generation Capital Ltd.  All rights reserved.

  Author: Mika Nystrom <mika@alum.mit.edu>
*)

MODULE SchemeUtilsFormat EXPORTS SchemeUtils;
FROM Scheme IMPORT Object;
IMPORT RTBrand, RTName;

PROCEDURE DebugFormat(x : Object) : TEXT =
  (* for debugging, something not really needed in the Java version since
     everything has a .toString there *)
  VAR brand := "not branded";

  BEGIN
    WITH tc = TYPECODE(x) DO
      TRY
        brand := "BRANDED " & RTBrand.GetByTC(tc)
      EXCEPT
        RTBrand.NotBranded => (* skip *)
      END;

      RETURN "<Modula-3 object : " & RTName.GetByTC(tc) & 
             ", " & brand & ">" (* ho hum... *)
    END
  END DebugFormat;

BEGIN END SchemeUtilsFormat.
