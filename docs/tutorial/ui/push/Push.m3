MODULE Push EXPORTS Main;
IMPORT Trestle, VBT, TextVBT, RigidVBT, ButtonVBT, BorderedVBT, HVSplit,
       Axis;

(* action of button when pushed *)
PROCEDURE QuitAction (self: ButtonVBT.T; READONLY cd: VBT.MouseRec) =
  BEGIN
    Trestle.Delete(main);        (* NB.  "main" is visible here. *)
  END QuitAction;

CONST
  horz = 30.0;                   (* horizontal size of "hello" window in
                                    millimeters *)
  vert = 10.0;                   (* vertical size of "hello" window in
                                    millimeters *)

VAR
  hello := RigidVBT.FromHV(TextVBT.New("Hello World"), horz, vert);
  quit  := ButtonVBT.New(ch := TextVBT.New("Quit"), action := QuitAction);
  main  := HVSplit.Cons(Axis.T.Ver, hello, BorderedVBT.New(quit));

BEGIN
  Trestle.Install(main);
  Trestle.AwaitDelete(main);
END Push.
