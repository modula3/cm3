(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Jun 23 20:56:55 PDT 1994 by najork                   *)
(*       Created on Tue Jan 20 14:00:00 PST 1994 by najork                   *)


MODULE ObColor;

IMPORT Color, ColorName, ObLib, ObLibUI, ObValue, Obliq, SynLocation;

PROCEDURE M3ToObliq (val : Color.T) : T =
  BEGIN
    RETURN NEW (T, what := "<a Color.T>", color := val);
  END M3ToObliq;


PROCEDURE ObliqToM3 (val : ObValue.Val) : Color.T RAISES {ObValue.Error} =
  BEGIN
    TYPECASE val OF
    | ObValue.ValText (node) => 
      TRY 
        RETURN ColorName.ToRGB (node.text);
      EXCEPT 
        ColorName.NotFound => 
        Obliq.RaiseError ("unknown color name");
        <* ASSERT FALSE *>
      END;
    | ObLibUI.ValColor (node) =>
      RETURN node.color;
    ELSE 
      Obliq.RaiseError ("expected color or text");
      <* ASSERT FALSE *>
    END;
  END ObliqToM3;


PROCEDURE GetArg (args    : ObValue.ArgArray; 
                  idx     : INTEGER; 
                  package : ObLib.T; 
                  opCode  : ObLib.OpCode; 
                  loc     : SynLocation.T) : Color.T RAISES {ObValue.Error} =
  BEGIN
    TYPECASE args[idx] OF
    | ObValue.ValText (node) => 
      TRY 
        RETURN ColorName.ToRGB(node.text);
      EXCEPT 
        ColorName.NotFound => RETURN Color.Black;
      END;
    | T (node) =>
      RETURN node.color;
    ELSE 
      ObValue.BadArgType (idx, "color", package.name, opCode.name, loc);
      RETURN Color.Black;   (* ... only to suppress compiler warning *)
    END;
  END GetArg;


BEGIN
END ObColor.
