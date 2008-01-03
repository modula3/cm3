MODULE Drag EXPORTS Main;

IMPORT Trestle, VBT, TextVBT, RigidVBT, ShadowedFeedbackVBT, HVSplit, HVBar;
IMPORT Axis, Shadow, DragSwitchVBT, Fmt;
FROM Colors IMPORT lblue, dblue;

CONST
  shsize = 5.0;                  (* shadow size in points *)
  min    = 20.0;                 (* size of demo area in millimeters *)

TYPE
  SF = ShadowedFeedbackVBT.T;
  DS = DragSwitchVBT.T OBJECT
       OVERRIDES
         callback := Callback;
         during   := During
       END;

PROCEDURE Callback (v: DS; READONLY cd: VBT.MouseRec) =
  BEGIN
    TextVBT.Put(display, "callback called");
  END Callback;

PROCEDURE During (v: DS; READONLY cd: VBT.PositionRec) =
  VAR
    point := cd.cp.pt;
    txt := "current position (" & Fmt.Int(point.h) & "," & Fmt.Int(point.v)
             & ")";
  BEGIN
    TextVBT.Put(display, txt);
  END During;

VAR
  shadow := Shadow.New(size := shsize, light := lblue, dark := dblue);
  switch := NEW(SF).init(ch := TextVBT.New("DragSwitch"), shadow := shadow);
  drag    := NEW(DS).init(switch);
  display := TextVBT.New("");
  work    := RigidVBT.FromHV(TextVBT.New("Drag Demo"), 2.0 * min, min);
  main := HVSplit.Cons(
            Axis.T.Ver, drag, HVBar.New(), display, HVBar.New(), work);

BEGIN
  Trestle.Install(main);
  Trestle.AwaitDelete(main)
END Drag.
