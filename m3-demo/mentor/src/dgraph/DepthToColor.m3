(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE DepthToColor;

IMPORT Color, PaintOp, ColorName;

CONST
  TextStack = ARRAY OF TEXT{"LightRed", "Green", "LightBlue", "Yellow"};

PROCEDURE Map (depth: INTEGER): PaintOp.T =
  <* FATAL ColorName.NotFound *>
  VAR 
    rgb: Color.T;
  BEGIN
    IF depth < 0 THEN
      RETURN PaintOp.Fg
    ELSE
      rgb := ColorName.ToRGB(TextStack[depth]);
      RETURN PaintOp.FromRGB(rgb.r, rgb.g, rgb.b);
      (*
            VAR rgb := RGB.T{0.0, 0.0, 0.0};
                div := depth DIV 3;
                mod := depth MOD 3;
            BEGIN
              rgb[mod] := 1.0;
              IF div <= 9 THEN rgb[(mod+1) MOD 3] := FLOAT(div)/10.0;
              ELSIF div < 19 THEN rgb[(mod+2) MOD 3] := FLOAT(div-10)/10.0;
              END;
              RETURN PaintOpCache.FromRGB(rgb);
            END;
      *)
    END;
  END Map;

BEGIN
END DepthToColor.
