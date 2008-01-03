(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman, Mark Manasse and Greg Nelson           *)
(* Last modified on Tue Jan 31 09:51:17 PST 1995 by kalsow   *)
(*      modified on Mon Jan 25 18:19:36 PST 1993 by msm      *)
(*      modified on Mon Feb 24 13:57:40 PST 1992 by muller   *)
(*      modified on Sat Nov  2 17:21:17 PST 1991 by gnelson  *)
<*PRAGMA LL*>

MODULE Pixmap;

IMPORT Palette, PlttFrnds, ScrnPixmap, ScreenType, TrestleComm;

PROCEDURE FromBitmap (bits: Raw): T =
  BEGIN
    IF bits.depth # 1 THEN Crash() END;
    LOCK PlttFrnds.con DO
      IF PlttFrnds.con.pixmaps # NIL THEN
        FOR i := 0 TO PlttFrnds.con.nextPixmap - 1 DO
          TYPECASE PlttFrnds.con.pixmaps[i] OF
            NULL =>
          | Closure (cl) => IF cl.bits = bits THEN RETURN T{i} END
          ELSE
          END
        END
      END
    END;
    RETURN Palette.FromPixmapClosure(NEW(Closure, bits := bits))
  END FromBitmap;

TYPE Closure = Palette.PixmapClosure OBJECT
    bits: Raw;
  OVERRIDES
    apply := Apply
  END;

PROCEDURE Apply(cl: Closure; st: ScreenType.T): ScrnPixmap.T =
  BEGIN
    TRY
      RETURN st.pixmap.load(cl.bits)
    EXCEPT
      TrestleComm.Failure => RETURN Palette.ResolvePixmap(st, Solid)
    END
  END Apply;

EXCEPTION FatalError;

PROCEDURE Crash() =
  <*FATAL FatalError*>
  BEGIN
    RAISE FatalError
  END Crash;
  
BEGIN END Pixmap.
