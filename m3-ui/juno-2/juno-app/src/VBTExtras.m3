(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Wed Dec 21 19:09:28 PST 1994 by heydon                   *)

MODULE VBTExtras;

IMPORT VBT, Font, Palette, Rect, ScrnFont, ScrnFontExtras;
IMPORT   VBTRep, PlttFrnds;

(* Most of this code was copied from "VBT.m3". *)

PROCEDURE TightBoundingBox(v: VBT.Leaf; txt: TEXT; fnt: Font.T): Rect.T =
  BEGIN
    LOOP
      LOCK v DO
        IF v.st = NIL THEN
          RETURN ScrnFontExtras.TightBoundingBox(txt, NIL)
        END;
        VAR sf: ScrnFont.T := NIL; BEGIN
          IF fnt.fnt < NUMBER(v.st.fonts^) THEN
            sf := v.st.fonts[fnt.fnt]
          END;
          IF sf # NIL AND sf # PlttFrnds.noFont THEN
            RETURN ScrnFontExtras.TightBoundingBox(txt, sf)
          END
        END
      END;
      VAR st: VBT.ScreenType; BEGIN
        LOCK v DO st := v.st END;
        IF st # NIL THEN EVAL Palette.ResolveFont(st, fnt) END
      END
    END
  END TightBoundingBox;

BEGIN
END VBTExtras.
