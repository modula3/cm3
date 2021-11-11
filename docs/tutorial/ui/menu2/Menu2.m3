MODULE Menu2 EXPORTS Main;

IMPORT Trestle, VBT, TextVBT, MenuSwitchVBT, ShadowedFeedbackVBT, Shadow;
IMPORT HVSplit, Axis, AnchorBtnVBT, Rect, RigidVBT;
IMPORT Wr, Stdio;
FROM Colors IMPORT lblue, dblue;

CONST
  shsize = 5.0;                  (* shadow size in points *)
  hmin   = 15.0;
  vmin   = 10.0;                 (* button size in millimeters *)


PROCEDURE Cut (v: MenuSwitchVBT.T; READONLY cd: VBT.MouseRec) =
  BEGIN
    Wr.PutText(Stdio.stdout, "Cut\n");
    Wr.Flush(Stdio.stdout);
  END Cut;

PROCEDURE Paste (v: MenuSwitchVBT.T; READONLY cd: VBT.MouseRec) =
  BEGIN
    Wr.PutText(Stdio.stdout, "Paste\n");
    Wr.Flush(Stdio.stdout);
  END Paste;

VAR
  sh      := Shadow.New(size := shsize, light := lblue, dark := dblue);
  fcut    := ShadowedFeedbackVBT.NewMenu(TextVBT.New("Cut"), sh);
  fpaste  := ShadowedFeedbackVBT.NewMenu(TextVBT.New("Paste"), sh);
  cut     := NEW(MenuSwitchVBT.T, callback := Cut).init(fcut);
  paste   := NEW(MenuSwitchVBT.T, callback := Paste).init(fpaste);
  menu    := HVSplit.Cons(Axis.T.Ver, cut, paste);
  edit    := TextVBT.New("Edit");
  fanchor := NEW(ShadowedFeedbackVBT.T).init(ch := edit, shadow := sh);
  anchor  := AnchorBtnVBT.New(ch := fanchor, menu := menu);
  main    := RigidVBT.FromHV(anchor, hmin, vmin);
BEGIN
  AnchorBtnVBT.Set(anchor, n := 0, hfudge := hmin, vfudge := -vmin / 2.0);
  Trestle.Install(main);
  Trestle.AwaitDelete(main);
END Menu2.
