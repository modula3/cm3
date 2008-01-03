(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue May 17 16:54:17 PDT 1994 by mhb         *)
(*      modified on Tue Jun 16 23:45:19 PDT 1992 by muller      *)
(*      modified on  by sclafani    *)
(*      modified on Wed Apr  8  3:32:26 PDT 1992 by sclafani    *)

MODULE Driver EXPORTS Main;

IMPORT Cube, CubeBundle, AutoRepeat, FloatMode, Fmt, FormsVBT,
       Lex, Rd, Region, Rsrc, Scan, Text, Thread, Trestle,
       TrestleComm, VBT;

<* FATAL FormsVBT.Error, FormsVBT.Unimplemented *>

TYPE 
  Repeater = AutoRepeat.T OBJECT
    cube: Cube.T;
  OVERRIDES
    repeat := Repeat
  END;

TYPE
  CubeVBT = VBT.Leaf OBJECT
    started := FALSE;
    repeater: Repeater;
  OVERRIDES
    mouse   := MouseCode;
    misc    := MiscCode;
    repaint := Repaint;
    reshape := Reshape;
  END;

VAR
  cube: Cube.T;
  fv: FormsVBT.T;

PROCEDURE Spin (
    <* UNUSED *> fv   : FormsVBT.T;
    <* UNUSED *> name : Text.T;
    <* UNUSED *> cl   : REFANY;
    <* UNUSED *> time : VBT.TimeStamp) =
  BEGIN
    Cube.Advance (cube);
    Cube.Display (cube);
  END Spin;

PROCEDURE Repeat (r: Repeater) =
  BEGIN
    Cube.Advance (r.cube);
    Cube.Display (r.cube);
  END Repeat;

PROCEDURE MouseCode (v: CubeVBT; READONLY cd: VBT.MouseRec) =
  BEGIN
    IF cd.clickType = VBT.ClickType.FirstDown THEN
      IF v.started THEN
        AutoRepeat.Stop(v.repeater);
        v.started := FALSE;
      ELSE
        AutoRepeat.Continue(v.repeater);
        v.started := TRUE;
      END;
    END
  END MouseCode;

PROCEDURE MiscCode (v: CubeVBT; READONLY cd: VBT.MiscRec) =
  BEGIN
    IF cd.type = VBT.Deleted THEN
      AutoRepeat.Stop(v.repeater);
      v.started := FALSE;
    END;
  END MiscCode;

PROCEDURE Repaint (
    <* UNUSED *>          v   : CubeVBT;
    <* UNUSED *> READONLY rgn : Region.T) =
  BEGIN
    Cube.Display (cube)
  END Repaint;

PROCEDURE Reshape (
    <* UNUSED *>          v  : CubeVBT; 
    <* UNUSED *> READONLY cd : VBT.ReshapeRec) =
  BEGIN
    FormsVBT.MakeEvent (fv, "screen", 0);
  END Reshape;

PROCEDURE SetView (
                 fv   : FormsVBT.T;
    <* UNUSED *> name : Text.T;
    <* UNUSED *> cl   : REFANY;
    <* UNUSED *> time : VBT.TimeStamp) =
  VAR mu, theta, phi: REAL;
  BEGIN
    IF GetReal (fv, "mu") <= 0.0 THEN
      FormsVBT.PutInteger (fv, "mu", 1);
      (* PutReal (fv, "mu", 1.0) *)
    END;
    mu    := GetReal (fv, "mu");
    theta := GetReal (fv, "theta");
    phi   := GetReal (fv, "phi");
    Cube.SetView (cube, mu, theta, phi);
    Cube.Display (cube);
  END SetView;

PROCEDURE SetStyle (
                 fv   : FormsVBT.T;
    <* UNUSED *> name : Text.T;
    <* UNUSED *> cl   : REFANY;
    <* UNUSED *> time : VBT.TimeStamp) =
  VAR style: INTEGER;
  BEGIN
    style := ORD( NOT FormsVBT.GetBoolean (fv, "wireframe"));
    Cube.SetStyle (cube, style);
    Cube.Display (cube);
  END SetStyle;

PROCEDURE SetImage (
                 fv   : FormsVBT.T;
    <* UNUSED *> name : Text.T;
    <* UNUSED *> cl   : REFANY;
    <* UNUSED *> time : VBT.TimeStamp) =
  VAR dblBuffer: BOOLEAN; screen: REAL;
  BEGIN
    dblBuffer := FormsVBT.GetBoolean (fv, "buffer");
    screen := GetReal (fv, "screen");
    IF screen <= 0.0 THEN
      screen := 1.0;
      PutReal (fv, "screen", screen);
    END;
    Cube.SetImage (cube, dblBuffer, screen);
    Cube.Display (cube);
  END SetImage;

PROCEDURE SetProjection (
                 fv   : FormsVBT.T;
    <* UNUSED *> name : Text.T;
    <* UNUSED *> cl   : REFANY;
    <* UNUSED *> time : VBT.TimeStamp) =
  VAR persp: BOOLEAN; d: REAL;
  BEGIN
    persp := FormsVBT.GetBoolean (fv, "persp");
    d := GetReal (fv, "d");
    IF d <= 0.0 THEN d := 1.0; PutReal (fv, "d", d); END;
    Cube.SetProjection (cube, persp, d);
    Cube.Display (cube);
  END SetProjection;

PROCEDURE SetSpin (
                 fv   : FormsVBT.T;
    <* UNUSED *> name : Text.T;
    <* UNUSED *> cl   : REFANY;
    <* UNUSED *> time : VBT.TimeStamp) =
  VAR rotate: INTEGER;
  BEGIN
    rotate := FormsVBT.GetInteger (fv, "rotate");
    Cube.SetSpin (cube, rotate);
    Cube.Display (cube);
  END SetSpin;

PROCEDURE GetReal (fv: FormsVBT.T; name: Text.T): REAL =
  BEGIN
    TRY
      RETURN Scan.Real (FormsVBT.GetText (fv, name));
    EXCEPT
    | Lex.Error, FloatMode.Trap => RETURN 0.0;
    END;
  END GetReal;

PROCEDURE PutReal (fv: FormsVBT.T; name: Text.T; value: REAL) =
  BEGIN
    FormsVBT.PutText(fv, name, Fmt.Real(value));
  END PutReal;

PROCEDURE Main () =
  <* FATAL Rd.Failure, Rsrc.NotFound, Thread.Alerted, TrestleComm.Failure *>
  VAR cubeVBT: CubeVBT;
  BEGIN
    fv := NEW(FormsVBT.T).initFromRsrc(
            "cube.fv",
            Rsrc.BuildPath("$CUBEPATH", CubeBundle.Get()));
    FormsVBT.AttachProc(fv, "mu", SetView);
    FormsVBT.AttachProc(fv, "theta", SetView);
    FormsVBT.AttachProc(fv, "phi", SetView);
    FormsVBT.AttachProc(fv, "wireframe", SetStyle);
    FormsVBT.AttachProc(fv, "solid", SetStyle);
    FormsVBT.AttachProc(fv, "screen", SetImage);
    FormsVBT.AttachProc(fv, "buffer", SetImage);
    FormsVBT.AttachProc(fv, "ortho", SetProjection);
    FormsVBT.AttachProc(fv, "persp", SetProjection);
    FormsVBT.AttachProc(fv, "d", SetProjection);
    FormsVBT.AttachProc(fv, "rotate", SetSpin);
    FormsVBT.AttachProc(fv, "spin", Spin);

    cubeVBT := NEW(CubeVBT);
    cube := Cube.New(cubeVBT);
    FormsVBT.MakeEvent(fv, "mu", 0);
    FormsVBT.MakeEvent(fv, "screen", 0);
    FormsVBT.MakeEvent(fv, "ortho", 0);
    FormsVBT.MakeEvent(fv, "rotate", 0);
    FormsVBT.MakeEvent(fv, "wireframe", 0);
    FormsVBT.PutGeneric(fv, "cube", cubeVBT);

    cubeVBT.repeater := NEW(Repeater, cube := cube).init();
    AutoRepeat.Stop(cubeVBT.repeater);

    Trestle.Install(fv);
    Trestle.AwaitDelete(fv);
  END Main;

BEGIN
  Main ();
END Driver.
