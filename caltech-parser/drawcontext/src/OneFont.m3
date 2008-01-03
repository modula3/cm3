(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: OneFont.m3,v 1.2 2001-09-19 15:30:31 wagner Exp $ *)

MODULE OneFont;
IMPORT Font;
IMPORT Fmt;
CONST
  Sizes = ARRAY OF CARDINAL{8, 9, 10, 12, 14, 18, 24, 36, 48, 72};

VAR
  Fonts: REF ARRAY OF Font.T;

PROCEDURE FromSize(ptSize: CARDINAL): Font.T =
  VAR
    dmin := LAST(INTEGER);
    d: INTEGER;
    fmin: Font.T;
  BEGIN
    FOR i := 0 TO LAST(Sizes) DO
      d := ABS(Sizes[i] - ptSize);
      IF d < dmin THEN
        dmin := d;
        fmin := Fonts[i];
      ELSE
        RETURN fmin;
      END;
    END;
    RETURN fmin;
  END FromSize;

BEGIN
  Fonts := NEW(REF ARRAY OF Font.T, NUMBER(Sizes));
  FOR i := 0 TO LAST(Sizes) DO
    Fonts[i] := Font.FromName(
       ARRAY OF TEXT{"-*-times-medium-r-*-*-*-" &
       Fmt.Int(Sizes[i]) & "0-100-100-*-*-*-*"});
  END;
END OneFont.
