(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Fri Sep 30 10:51:20 PDT 1994 by najork                   *)
(*       Created on Wed Feb  9 14:50:42 PST 1994 by najork                   *)


MODULE LightGO;

IMPORT BooleanProp, BooleanPropPrivate, Color, ColorProp, ColorPropPrivate, 
       GO, GOPrivate;

REVEAL 
  T = GO.T BRANDED OBJECT
  OVERRIDES
    needsTransparency := NeedsTransparency;
  END;



PROCEDURE NeedsTransparency (<* UNUSED *> self : T; 
                             <* UNUSED *> t    : REAL) : BOOLEAN =
  BEGIN
    RETURN FALSE;
  END NeedsTransparency;


(*****************************************************************************)
(* Convenience Procedures                                                    *)
(*****************************************************************************)


PROCEDURE SetColour (o : GO.T; v : Color.T) =
  BEGIN
    o.setProp (Colour.bind (ColorProp.NewConst (v)));
  END SetColour;


PROCEDURE SetSwitch (o : GO.T; v : BOOLEAN) =
  BEGIN
    o.setProp (Switch.bind (BooleanProp.NewConst (v)));
  END SetSwitch;


BEGIN
  Colour := NEW (ColorProp.Name).init (Color.White);
  Switch := NEW (BooleanProp.Name).init (TRUE);
END LightGO.
