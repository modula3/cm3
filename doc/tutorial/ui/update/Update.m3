MODULE Update EXPORTS Main;

IMPORT Trestle, VBT;
IMPORT TextVBT, ButtonVBT, RigidVBT, BorderedVBT, TextureVBT, HVSplit;
IMPORT Axis;                     (* Trestle auxiliary library module. *)
IMPORT Fmt;                      (* Standard Modula-3 library module. *)

PROCEDURE DoInc (b: ButtonVBT.T; READONLY cd: VBT.MouseRec) =
  BEGIN
    INC(num);                    (* Change to non-local variable! *)
    TextVBT.Put(text, Fmt.Int(num));
  END DoInc;

PROCEDURE DoExit (b: ButtonVBT.T; READONLY cd: VBT.MouseRec) =
  BEGIN
    Trestle.Delete(main);
  END DoExit;

CONST
  bs = 2.0;                      (* Size of border around main window (in
                                    millimeters). *)
  ws = 20.0;                     (* Width of window around text (in
                                    millimeters). *)

VAR
  num   := 97;                   (* Value for display in text VBT. *)
  item1 := ButtonVBT.New(TextVBT.New("Inc"), DoInc);
  item2 := ButtonVBT.New(TextVBT.New("Exit"), DoExit);
  bar   := HVSplit.Cons(Axis.T.Hor, item1, item2);
  line  := RigidVBT.FromHV(TextureVBT.New(), hMin := 0.0, vMin := bs);
  text  := TextVBT.New(Fmt.Int(num));
  work  := RigidVBT.FromHV(text, hMin := ws, vMin := ws / 2.0);
  main  := BorderedVBT.New(HVSplit.Cons(Axis.T.Ver, bar, line, work), bs);

BEGIN
  Trestle.Install(main);
  Trestle.AwaitDelete(main);
END Update.
