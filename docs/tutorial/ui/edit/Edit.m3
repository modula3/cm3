MODULE Edit EXPORTS Main;

IMPORT Trestle, VBT, TextPort, Axis, Font, PaintOp, HVBar, HVSplit,
       RigidVBT;
FROM TextPort IMPORT Model;
IMPORT Wr, Stdio;
FROM Colors IMPORT red, blue;

CONST
  vmin    = 40.0;                (* Size in millimeters *)
  vmax    = 100.0;               (* Size in millimeters *)
  hmin    = 40.0;                (* Size in millimeters *)
  hmax    = 100.0;               (* Size in millimeters *)
  times14 = "-*-times-*-r-*-*-14-*-*-*-*-*-*-*";

VAR
  font    := Font.FromName(ARRAY OF TEXT{times14});
  red_sch := PaintOp.MakeColorScheme(bg := PaintOp.Bg, fg := red);
  single := NEW(TextPort.T).init(
              singleLine := TRUE, colorScheme := red_sch, font := font,
              model := Model.Emacs);
  blue_sch := PaintOp.MakeColorScheme(bg := PaintOp.Bg, fg := blue);
  multi := NEW(TextPort.T).init(
             singleLine := FALSE, colorScheme := blue_sch, font := font,
             model := Model.Emacs);
  split := HVSplit.Cons(Axis.T.Ver, single, HVBar.New(), multi);
  main  := RigidVBT.FromHV(split, hmin, vmin, hmax, vmax);

BEGIN
  TextPort.SetText(single, "Single Line Editor");
  TextPort.SetText(multi, "Multi Line Editor");
  Trestle.Install(main);
  Trestle.AwaitDelete(main);
  (* If either text has been modified, then print it out when we're
     done. *)
  IF TextPort.IsModified(single) THEN
    Wr.PutText(Stdio.stdout, TextPort.GetText(single) & "\n");
  END;
  IF TextPort.IsModified(multi) THEN
    Wr.PutText(Stdio.stdout, TextPort.GetText(multi) & "\n");
  END;
END Edit.
