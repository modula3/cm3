MODULE React EXPORTS Main;
IMPORT Trestle, VBT, TextVBT, SwitchVBT, ChoiceVBT, ReactivityVBT;
IMPORT MarginFeedbackVBT, BiFeedbackVBT, ShadowedFeedbackVBTASShVBT;
IMPORT Shadow, Axis, HVSplit, HVBar;
FROM Colors IMPORT lblue, dblue;

TYPE
  S = SwitchVBT.T OBJECT
      METHODS
        init (ch: VBT.T; s: Shadow.T): S := Init
      END;

PROCEDURE Init (self: S; ch: VBT.T; s: Shadow.T): S =
  VAR m := MarginFeedbackVBT.NewCheck(NEW(ShVBT.T).init(ch, s));
  BEGIN
    EVAL SwitchVBT.T.init(self, f := NEW(BiFeedbackVBT.T).init(m));
    RETURN (self);
  END Init;

PROCEDURE ActiveAction (v: ChoiceVBT.T; READONLY cd: VBT.MouseRec) =
  BEGIN
    ReactivityVBT.Set(button, ReactivityVBT.State.Active);
  END ActiveAction;

PROCEDURE PassiveAction (v: ChoiceVBT.T; READONLY cd: VBT.MouseRec) =
  BEGIN
    ReactivityVBT.Set(button, ReactivityVBT.State.Passive);
  END PassiveAction;

PROCEDURE DormantAction (v: ChoiceVBT.T; READONLY cd: VBT.MouseRec) =
  BEGIN
    ReactivityVBT.Set(button, ReactivityVBT.State.Dormant);
  END DormantAction;

PROCEDURE VanishAction (v: ChoiceVBT.T; READONLY cd: VBT.MouseRec) =
  BEGIN
    ReactivityVBT.Set(button, ReactivityVBT.State.Vanish);
  END VanishAction;

VAR
  shadow := Shadow.New(size := 5.0, light := lblue, dark := dblue);
  button := ReactivityVBT.New(
              NEW(SwitchVBT.T).init(
                NEW(ShVBT.T).init(TextVBT.New("React"), shadow)));
  switch1 := NEW(S).init(TextVBT.New("Active"), shadow);
  switch2 := NEW(S).init(TextVBT.New("Passive"), shadow);
  switch3 := NEW(S).init(TextVBT.New("Dormant"), shadow);
  switch4 := NEW(S).init(TextVBT.New("Vanish"), shadow);
  group   := NEW (ChoiceVBT.Group);
  choice1 := NEW(ChoiceVBT.T, callback := ActiveAction).init(
               switch1, group);
  choice2 := NEW(ChoiceVBT.T, callback := PassiveAction).init(
               switch2, group);
  choice3 := NEW(ChoiceVBT.T, callback := DormantAction).init(
               switch3, group);
  choice4 := NEW(ChoiceVBT.T, callback := VanishAction).init(
               switch4, group);
  main := HVSplit.Cons(Axis.T.Ver, choice1, choice2, choice3, choice4,
                       HVBar.New(), button);
BEGIN
  Trestle.Install(main);
  Trestle.AwaitDelete(main)
END React.
