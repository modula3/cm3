(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman, Mark Manasse and Greg Nelson           *)
(* Last modified on Mon Feb  1 12:17:57 PST 1993 by msm      *)
(*      modified on Mon Feb 24 13:57:05 PST 1992 by muller   *)
(*      modified on Tue Oct 22 21:34:39 PDT 1991 by gnelson  *)
<*PRAGMA LL*>

MODULE Font;

IMPORT Palette, PlttFrnds, ScrnFont, ScreenType, TrestleComm, Text;

TYPE TextList = REF ARRAY OF TEXT;

PROCEDURE FromName (READONLY names: ARRAY OF TEXT; useXft : BOOLEAN := TRUE): T =
  VAR tl := NEW(TextList, NUMBER(names));
  BEGIN
    FOR i := 0 TO LAST(names) DO tl[i] := names[i] END;
    LOCK PlttFrnds.con DO
      IF PlttFrnds.con.fonts # NIL THEN
        FOR i := 0 TO PlttFrnds.con.nextFont - 1 DO
          TYPECASE PlttFrnds.con.fonts[i] OF
            NULL =>              (* skip *)
          | Closure (cl) =>
              IF NUMBER(cl.names^) = NUMBER(tl^) THEN
                VAR match := TRUE;
                BEGIN
                  FOR j := 0 TO LAST(tl^) DO
                    match := match AND Text.Equal(cl.names[j], tl[j])
                  END;
                  IF match THEN RETURN T{i} END
                END
              END
          ELSE
          END
        END
      END
    END;
    RETURN Palette.FromFontClosure(NEW(Closure, names := tl, useXft := useXft));
  END FromName;

TYPE Closure = Palette.FontClosure OBJECT
    names: TextList;
    useXft : BOOLEAN := FALSE;
  OVERRIDES
    apply := Apply
  END;

PROCEDURE Apply(cl: Closure; st: ScreenType.T): ScrnFont.T =
  BEGIN
    FOR i := FIRST(cl.names^) TO LAST(cl.names^) DO
      TRY
        RETURN st.font.lookup(cl.names[i], cl.useXft)
      EXCEPT
        TrestleComm.Failure, ScrnFont.Failure => (*skip*)
      END
    END;
    RETURN Palette.ResolveFont(st, BuiltIn)
  END Apply;

BEGIN END Font.
