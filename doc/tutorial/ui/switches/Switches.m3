MODULE Switches EXPORTS Main;

IMPORT Trestle, VBT, TextVBT, RigidVBT, ShadowedFeedbackVBT, Shadow,
       HVSplit, HVBar;
IMPORT GuardedBtnVBT, QuickSwitchVBT, TrillSwitchVBT, Axis, PaintOp, Fmt;
FROM Colors IMPORT lblue, dblue;

TYPE
  S = ShadowedFeedbackVBT.T;     (* A shorter synonym *)
  TrillSwitch = TrillSwitchVBT.T OBJECT
                  callbackcount := 0;
                OVERRIDES
                  callback := Trill;
                END;

PROCEDURE Guard (v: GuardedBtnVBT.T; READONLY cd: VBT.MouseRec) =
  BEGIN
    TextVBT.Put(display, "callback of GuardedBtn called");
  END Guard;

PROCEDURE Quick (v: QuickSwitchVBT.T; READONLY cd: VBT.MouseRec) =
  BEGIN
    TextVBT.Put(display, "callback of QuickSwitch called");
  END Quick;

PROCEDURE Trill (v: TrillSwitch; READONLY cd: VBT.MouseRec) =
  BEGIN
    INC(v.callbackcount);
    TextVBT.Put(display, "callback of TrillSwitch called "
                           & Fmt.Int(v.callbackcount) & " times");
  END Trill;

CONST
  shsize = 5.0;                  (* shadow size in points *)
  size   = 40.0;                 (* size of textVBT in millimeters *)
VAR
  sh      := Shadow.New(size := shsize, light := lblue, dark := dblue);
  gfb     := NEW(S).init(ch := TextVBT.New("GuardedBtn"), shadow := sh);
  qfb     := NEW(S).init(ch := TextVBT.New("QuickSwitch"), shadow := sh);
  tfb     := NEW(S).init(ch := TextVBT.New("TrillSwitch"), shadow := sh);
  guard   := NEW(GuardedBtnVBT.T, callback := Guard).init(gfb);
  quick   := NEW(QuickSwitchVBT.T, callback := Quick).init(qfb);
  trill   := NEW(TrillSwitch).init(tfb);
  display := TextVBT.New("");
  rtext   := RigidVBT.FromHV(TextVBT.New("Switches"), size, size / 3.0);
  hsplit := HVSplit.Cons(
              Axis.T.Hor, guard, HVBar.New(), quick, HVBar.New(), trill);
  main := HVSplit.Cons(
            Axis.T.Ver, hsplit, HVBar.New(), display, HVBar.New(), rtext);
BEGIN
  Trestle.Install(main);
  Trestle.AwaitDelete(main);
END Switches.
