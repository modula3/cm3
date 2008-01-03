MODULE Checkbox EXPORTS Main;

IMPORT Trestle, VBT, TextVBT, RigidVBT, Shadow, Font, BooleanVBT;
IMPORT SwitchVBT, BiFeedbackVBT, ShadowedFeedbackVBT, MarginFeedbackVBT;
FROM Colors IMPORT lblue, dblue;

CONST
  shsize  = 8.0;                 (* shadow size in points *)
  times14 = "-*-times-*-r-*-*-14-*-*-*-*-*-*-*";

PROCEDURE Check (v: BooleanVBT.T; READONLY cd: VBT.MouseRec) =
  BEGIN
    IF BooleanVBT.Get(v) THEN
      TextVBT.Put(text, "Uncheck");
    ELSE
      TextVBT.Put(text, "Check");
    END;
  END Check;

VAR
  font   := Font.FromName(ARRAY OF TEXT{times14});
  sh     := Shadow.New(size := shsize, light := lblue, dark := dblue);
  text   := TextVBT.New(" Check ", fnt := font);
  v      := RigidVBT.FromHV(text, 20.0, 10.0);
  sfb    := NEW(ShadowedFeedbackVBT.T).init(ch := v, shadow := sh);
  mfb    := MarginFeedbackVBT.NewBox(ch := sfb);
  bfb    := NEW(BiFeedbackVBT.T).init(ch := mfb);
  switch := NEW(SwitchVBT.T).init(bfb);
  main   := NEW(BooleanVBT.T, callback := Check).init(switch);
BEGIN
  Trestle.Install(main);
  Trestle.AwaitDelete(main)
END Checkbox.

