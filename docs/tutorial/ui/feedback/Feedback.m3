MODULE Main;

IMPORT Trestle, VBT, TextVBT, SwitchVBT, ShadowedFeedbackVBT,
       BorderedFeedbackVBT;
IMPORT HVSplit, HVBar, Axis, Font, Shadow, PaintOp, Pts;
FROM Colors IMPORT lblue, dblue, red, white;

CONST
  times14 = "-*-times-*-r-*-*-14-*-*-*-*-*-*-*";
  size    = 1.0;                 (* border size in millimeters. *)

PROCEDURE NoAction (v: SwitchVBT.T; READONLY cd: VBT.MouseRec) =
  BEGIN (* Do nothing *)
  END NoAction;

VAR
  font  := Font.FromName(ARRAY OF TEXT{times14});
  pts   := Pts.FromMM(2.0 * size); (* shadow size in points *)
  text1 := TextVBT.New("Push1", fnt := font);
  text2 := TextVBT.New("Push2", fnt := font);
  sh    := Shadow.New(size := pts, light := lblue, dark := dblue);
  sfb   := NEW(ShadowedFeedbackVBT.T).init(ch := text1, shadow := sh);
  sw1   := NEW(SwitchVBT.T, callback := NoAction).init(sfb);
  pair  := PaintOp.Pair(white, red);
  bfb := NEW(BorderedFeedbackVBT.T).init(
           ch := text2, size := size, op := pair);
  sw2  := NEW(SwitchVBT.T, callback := NoAction).init(bfb);
  main := HVSplit.Cons(Axis.T.Hor, sw1, HVBar.New(op := PaintOp.Bg), sw2);
BEGIN
  Trestle.Install(main);
  Trestle.AwaitDelete(main)
END Main.
