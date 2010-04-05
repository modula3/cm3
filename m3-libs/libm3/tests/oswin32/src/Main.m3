MODULE Main;
IMPORT RTIO, OSWin32, WinBase, Word;

VAR ver := WinBase.GetVersion();
BEGIN
  RTIO.PutHex(ver);
  RTIO.PutText("\n");
  RTIO.PutInt(Word.And(16_FF, ver));
  RTIO.PutText(".");
  RTIO.PutInt(Word.And(16_FF, ver DIV 16_100));
  RTIO.PutText(".");
  RTIO.PutInt(Word.And(16_7FFF, ver DIV 16_10000));
  RTIO.PutText("\nWin95:");
  RTIO.PutInt(ORD(OSWin32.Win95()));
  RTIO.PutText("\n");
  RTIO.Flush();
END Main.
