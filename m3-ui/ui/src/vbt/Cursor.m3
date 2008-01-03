(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman, Mark Manasse and Greg Nelson           *)
(* Last modified on Mon Jan 25 18:21:08 PST 1993 by msm      *)
(*      modified on Mon Feb 24 13:56:54 PST 1992 by muller   *)
(*      modified on Sat Nov  2 17:21:06 PST 1991 by gnelson  *)
<*PRAGMA LL*>

MODULE Cursor;

IMPORT Palette, PlttFrnds, ScrnCursor, ScreenType, TrestleComm, Text;

PROCEDURE FromRaw (READONLY r: Raw): T =
  VAR rr := r;
  BEGIN
    FixRGB(rr.color1);
    FixRGB(rr.color2);
    FixRGB(rr.color3);
    LOCK PlttFrnds.con DO
      IF PlttFrnds.con.cursors # NIL THEN
        FOR i := 0 TO PlttFrnds.con.nextCursor - 1 DO
          TYPECASE PlttFrnds.con.cursors[i] OF
            NULL =>              (* skip *)
          | Closure (cl) => IF cl.raw = rr THEN RETURN T{i} END
          ELSE
          END
        END
      END
    END;
    RETURN Palette.FromCursorClosure(NEW(Closure, raw := rr))
  END FromRaw;

PROCEDURE FixRGB(VAR c: RGB) =
(* adjust c.gray so that it is in [0..1], and c.bw so 
   that it is not UseIntensity. *)
  BEGIN
    IF c.gray < 0.0 THEN
      c.gray := MIN(1.0, MAX(0.0, 0.2390 * c.r + 0.6860 * c.g + 0.0750 * c.b))
    END;
    IF c.bw = BW.UseIntensity THEN
      IF c.r = 0.0 AND c.g = 0.0 AND c.b = 0.0 THEN
        c.bw := BW.UseFg
      ELSE
        c.bw := BW.UseBg
      END
    END
  END FixRGB;

TYPE Closure = Palette.CursorClosure OBJECT
    raw: Raw;
  OVERRIDES
    apply := Apply
  END;

PROCEDURE Apply(cl: Closure; st: ScreenType.T): ScrnCursor.T =
  BEGIN
    TRY
      RETURN st.cursor.load(cl.raw)
    EXCEPT
      TrestleComm.Failure => RETURN Palette.ResolveCursor(st, DontCare)
    END
  END Apply;

PROCEDURE FromName (READONLY names: ARRAY OF TEXT): T =
  VAR tl := NEW(REF ARRAY OF TEXT, NUMBER(names));
  BEGIN
    FOR i := 0 TO LAST(names) DO tl[i] := names[i] END;
    LOCK PlttFrnds.con DO
      IF PlttFrnds.con.cursors # NIL THEN
        FOR i := 0 TO PlttFrnds.con.nextCursor - 1 DO
          TYPECASE PlttFrnds.con.cursors[i] OF
            NULL =>              (* skip *)
          | NameClosure (cl) =>
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
    RETURN Palette.FromCursorClosure(NEW(NameClosure, names := tl))
  END FromName;

TYPE NameClosure = Palette.CursorClosure OBJECT
    names: REF ARRAY OF TEXT;
  OVERRIDES
    apply := NameApply
  END;

PROCEDURE NameApply(cl: NameClosure; st: ScreenType.T): ScrnCursor.T =
  VAR res: ScrnCursor.T;
  BEGIN
    FOR i := FIRST(cl.names^) TO LAST(cl.names^) DO
      TRY
        res := st.cursor.lookup(cl.names[i]);
        IF res # NIL THEN RETURN res END;
      EXCEPT
        TrestleComm.Failure=> (*skip*)
      END
    END;
    RETURN Palette.ResolveCursor(st, DontCare)
  END NameApply;
  
 BEGIN END Cursor.
