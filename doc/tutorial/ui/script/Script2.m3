MODULE Script EXPORTS Main;
IMPORT Trestle, VBT;
IMPORT TypescriptVBT;
IMPORT Wr;
VAR
  port := NEW(TypescriptVBT.Port).init(readOnly := TRUE);
  scr  := NEW(TypescriptVBT.T);
  wr: TypescriptVBT.Writer;
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
  Wr.PutText(wr, "Line one\n");
  Wr.PutText(wr, "Line two\n");
  Wr.Flush(wr);
  Trestle.Install(scr);
  Trestle.AwaitDelete(scr);
END Script.

