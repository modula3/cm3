MODULE Overlay EXPORTS Main;

IMPORT Trestle, VBT, BorderedVBT, TextVBT, ButtonVBT;
IMPORT HVSplit, TSplit, Axis, Font, RigidVBT;
FROM Pictures IMPORT stampVBT, wind_millVBT;

CONST
  size    = 25.0;                (* millimeters *)
  times14 = "-*-times-*-r-*-*-14-*-*-*-*-*-*-*";

PROCEDURE NextAction (v: ButtonVBT.T; READONLY cd: VBT.MouseRec) =
  VAR current := TSplit.GetCurrent(ts);
  BEGIN
    IF (current = ch1) THEN
      TSplit.SetCurrent(ts, ch2);
    ELSIF (current = ch2) THEN
      TSplit.SetCurrent(ts, ch1);
    END;
  END NextAction;

VAR
  font := Font.FromName(ARRAY OF TEXT{times14});
  ch1  := stampVBT;
  ch2  := wind_millVBT;
  ts   := TSplit.Cons(ch1, ch2);
  bt   := ButtonVBT.New(TextVBT.New("Next", fnt := font), NextAction);
  main := HVSplit.Cons(Axis.T.Hor, ts, BorderedVBT.New(bt));

BEGIN
  Trestle.Install(main);
  Trestle.AwaitDelete(main);
END Overlay.
