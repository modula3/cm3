MODULE RadioButtons EXPORTS Main;

IMPORT Trestle, VBT, TextVBT, SwitchVBT, ChoiceVBT;
IMPORT MarginFeedbackVBT, BiFeedbackVBT, ShadowedFeedbackVBT;
IMPORT Shadow, Axis, HVSplit, HVBar;
FROM Colors IMPORT lblue, dblue;

TYPE
  S = SwitchVBT.T OBJECT
      METHODS
        init (ch: VBT.T; s: Shadow.T): S := Init
      END;

PROCEDURE Init (self: S; ch: VBT.T; s: Shadow.T): S =
  VAR
    m := MarginFeedbackVBT.NewBullet(
           NEW(ShadowedFeedbackVBT.T).init(ch, s));
  BEGIN
    EVAL SwitchVBT.T.init(self, f := NEW(BiFeedbackVBT.T).init(m));
    RETURN (self);
  END Init;

CONST shsize = 5.0;              (* shadow size in points *)

PROCEDURE kanu (v: ChoiceVBT.T; READONLY cd: VBT.MouseRec) =
  BEGIN
    TextVBT.Put(text, "90.1 FM");
  END kanu;

PROCEDURE wbaa (v: ChoiceVBT.T; READONLY cd: VBT.MouseRec) =
  BEGIN
    TextVBT.Put(text, "106.1 AM");
  END wbaa;

PROCEDURE kera (v: ChoiceVBT.T; READONLY cd: VBT.MouseRec) =
  BEGIN
    TextVBT.Put(text, "90.5 FM");
  END kera;

VAR
  text    := TextVBT.New("frequency");
  sh      := Shadow.New(size := shsize, light := lblue, dark := dblue);
  switch1 := NEW(S).init(TextVBT.New("KANU"), sh);
  switch2 := NEW(S).init(TextVBT.New("WBAA"), sh);
  switch3 := NEW(S).init(TextVBT.New("KERA"), sh);
  group   := NEW (ChoiceVBT.Group);
  choice1 := NEW(ChoiceVBT.T, callback := kanu).init(switch1, group);
  choice2 := NEW(ChoiceVBT.T, callback := wbaa).init(switch2, group);
  choice3 := NEW(ChoiceVBT.T, callback := kera).init(switch3, group);
  main := HVSplit.Cons(Axis.T.Ver, choice1, HVBar.New(), choice2,
                       HVBar.New(), choice3, HVBar.New(), text);
BEGIN
  Trestle.Install(main);
  Trestle.AwaitDelete(main)
END RadioButtons.
