MODULE Main;
IMPORT RTIO, OSWin32, WinBase, Word;

VAR ver := WinBase.GetVersion();
BEGIN
  RTIO.PutHex(ver);
  RTIO.PutText("\n");
  RTIO.PutInt(Word.And(16_FF, ver));
  RTIO.PutText(".");
  RTIO.PutInt(Word.And(16_FF, Word.RightShift(ver, 8)));
  RTIO.PutText(".");
  RTIO.PutInt(Word.And(16_7FFF, Word.RightShift(ver, 16)));
  RTIO.PutText("\nWin95:");
  RTIO.PutInt(ORD(OSWin32.Win95()));
  RTIO.PutText("\n");
  RTIO.Flush();
END Main.
