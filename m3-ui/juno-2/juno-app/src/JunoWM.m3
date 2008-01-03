(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Jun 16 17:21:21 PDT 1997 by heydon                   *)
<* PRAGMA LL *>

UNSAFE MODULE JunoWM;

IMPORT VBT, Trestle, TrestleComm, TrestleImpl, VBTClass, XParam;
IMPORT  Axis, Point, StableVBT;

<* LL.sup < VBT.mu *>
PROCEDURE Install(w: VBT.T; disp, geom: TEXT; applName: TEXT)
    RAISES {Error, TrestleComm.Failure} =
  VAR trsl := Trestle.Connect(disp); scrId: INTEGER; BEGIN
    TrestleImpl.SetDefault(trsl);
    LOCK VBT.mu DO
      Trestle.Attach(w, trsl);
      Trestle.Decorate(w, applName := applName);
      TRY
        VAR geomRec: XParam.Geometry; BEGIN
          IF geom # NIL THEN
            geomRec := XParam.ParseGeometry(geom);
            IF geomRec.size = XParam.Missing THEN
              VAR shapes := VBTClass.GetShapes(w, FALSE); BEGIN
                geomRec.size.h := shapes[Axis.T.Hor].pref;
                geomRec.size.v := shapes[Axis.T.Ver].pref
              END
            ELSE
              StableVBT.SetShape(w, geomRec.size.h, geomRec.size.v)
            END
          END;
          IF disp = NIL
            THEN scrId := Trestle.ScreenOf(w, Point.Origin).id
            ELSE scrId := XParam.ParseDisplay(disp).screen
          END;
          Trestle.Overlap(w, scrId, XParam.Position(trsl, scrId, geomRec))
        END
      EXCEPT
        XParam.Error (info) =>
          VAR arg: TEXT; BEGIN
            TYPECASE info OF <* NOWARN *>
            | XParam.GeometryInfo => arg := "-geometry"
            | XParam.DisplayInfo => arg := "-display"
            END;
            RAISE Error("illegal " & arg & " argument: " & info.spec)
          END
      END
    END (* LOCK *)
  END Install;

BEGIN
END JunoWM.
