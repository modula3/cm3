(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Created by stolfi on Mon Jun  6 20:44:08 PDT 1988           *)
(* Last modified on Sat Nov 28 10:46:22 PST 1992 by mhb        *)
(*      modified on Tue Feb 11 21:39:42 PST 1992 by muller     *)
(*      modified on Thu Oct 25  0:16:01 PDT 1990 by stolfi     *)

MODULE Color;

PROCEDURE Brightness (READONLY rgb: T): REAL =
  BEGIN
    RETURN MIN(1.0, 
              MAX(0.0, 0.239 * rgb.r + 0.686 * rgb.g + 0.075 * rgb.b))
  END Brightness;

PROCEDURE ToHSV (READONLY rgb: T): HSV =
  VAR min, max, h, s, v: REAL;
  BEGIN
    max := MAX(MAX(rgb.r, rgb.g), rgb.b);
    min := MIN(MIN(rgb.r, rgb.g), rgb.b);
    v := max;
    IF max # 0.0 THEN s := (max - min) / max ELSE s := 0.0 END;
    IF s = 0.0 THEN
      h := 0.0
    ELSE
      IF rgb.r = max THEN
        h := (rgb.g - rgb.b) / (max - min)
      ELSIF rgb.g = max THEN
        h := 2.0 + (rgb.b - rgb.r) / (max - min)
      ELSE
        h := 4.0 + (rgb.r - rgb.g) / (max - min)
      END;
      h := h / 6.0;
      IF h < 0.0 THEN h := h + 1.0 END;
    END;
    RETURN HSV{h, s, v}
  END ToHSV;

PROCEDURE FromHSV (READONLY hsv: HSV): T =
  VAR
    h := hsv.h;
    s := hsv.s;
    v := hsv.v;
  BEGIN
    IF v = 0.0 THEN
      RETURN Black
    ELSIF s = 0.0 THEN
      RETURN T{v, v, v}
    ELSE
      IF h = 1.0 THEN h := 0.0 END;
      VAR
        i := TRUNC(h * 6.0);
        f := h * 6.0 - FLOAT(i);
        p := v * (1.0 - s);
        q := v * (1.0 - s * f);
        t := v * (1.0 - s * (1.0 - f));
      BEGIN
        CASE i OF
        | 0 => RETURN T{v, t, p}
        | 1 => RETURN T{q, v, p}
        | 2 => RETURN T{p, v, t}
        | 3 => RETURN T{p, q, v}
        | 4 => RETURN T{t, p, v}
        | 5 => RETURN T{v, p, q}
        ELSE <* ASSERT FALSE *>
        END
      END
    END
  END FromHSV;

BEGIN
END Color.

