UNSAFE MODULE Main;
IMPORT Date, RTIO;

VAR d: Date.T;

BEGIN
  RTIO.PutAddr(ADR(d)); RTIO.PutText(" T\n");
  RTIO.PutAddr(ADR(d.year)); RTIO.PutText(" year\n");
  RTIO.PutAddr(ADR(d.month)); RTIO.PutText(" month\n");
  RTIO.PutAddr(ADR(d.day)); RTIO.PutText(" day\n");
  RTIO.PutAddr(ADR(d.hour)); RTIO.PutText(" hour\n");
  RTIO.PutAddr(ADR(d.minute)); RTIO.PutText(" minute\n");
  RTIO.PutAddr(ADR(d.second)); RTIO.PutText(" second\n");
  RTIO.PutAddr(ADR(d.offset)); RTIO.PutText(" offset\n");
  RTIO.PutAddr(ADR(d.zone)); RTIO.PutText(" zone\n");
  RTIO.PutAddr(ADR(d.weekDay)); RTIO.PutText(" weekday\n");

  RTIO.PutInt(BYTESIZE(d)); RTIO.PutText(" T\n");
  RTIO.PutInt(BYTESIZE(d.year)); RTIO.PutText(" year\n");
  RTIO.PutInt(BYTESIZE(d.month)); RTIO.PutText(" month\n");
  RTIO.PutInt(BYTESIZE(d.day)); RTIO.PutText(" day\n");
  RTIO.PutInt(BYTESIZE(d.hour)); RTIO.PutText(" hour\n");
  RTIO.PutInt(BYTESIZE(d.minute)); RTIO.PutText(" minute\n");
  RTIO.PutInt(BYTESIZE(d.second)); RTIO.PutText(" second\n");
  RTIO.PutInt(BYTESIZE(d.offset)); RTIO.PutText(" offset\n");
  RTIO.PutInt(BYTESIZE(d.zone)); RTIO.PutText(" zero\n");
  RTIO.PutInt(BYTESIZE(d.weekDay)); RTIO.PutText(" weekday\n");

  RTIO.Flush();
END Main.
