MODULE Script EXPORTS Main;
IMPORT Trestle, VBT, ButtonVBT, TextVBT, HVSplit, TypescriptVBT;
IMPORT Axis;
IMPORT Wr;

PROCEDURE QuitAction (self: ButtonVBT.T; READONLY cd: VBT.MouseRec) =
  BEGIN
    Trestle.Delete(main);
  END QuitAction;

PROCEDURE DoAction (self: ButtonVBT.T; READONLY cd: VBT.MouseRec) =
  BEGIN
    Wr.PutText(wr, "More lines.\n");
    Wr.PutText(wr, "Some more lines.\n");
  END DoAction;

VAR
  port := NEW(TypescriptVBT.Port).init(readOnly := TRUE);
  scr  := NEW(TypescriptVBT.T);
  wr: TypescriptVBT.Writer;
  do                       := ButtonVBT.New(TextVBT.New("do"), DoAction);
  quit := ButtonVBT.New(TextVBT.New("quit"), QuitAction);
  bs   := HVSplit.Cons(Axis.T.Hor, do, quit);
  main := HVSplit.Cons(Axis.T.Ver, scr, bs);

BEGIN
  scr.port := port;
  EVAL scr.init();
  wr := TypescriptVBT.GetWr(scr);
  Wr.PutText(wr, "Line one\n");
  Wr.PutText(wr, "Line two\n");
  Wr.PutText(wr, "Line one\n");
  Wr.PutText(wr, "Line two\n");
  Wr.PutText(wr, "Line one\n");
  Wr.PutText(wr, "Line two\n");
  Wr.PutText(wr, "Line one\n");
  Wr.PutText(wr, "Line two\n");
  Wr.Flush(wr);
  Trestle.Install(main);
  Trestle.AwaitDelete(main);
END Script.
