UNSAFE MODULE Main;
IMPORT Date, RTIO, Time;

VAR d: Date.T;

CONST T = RTIO.PutText;
CONST A = RTIO.PutAddr;
CONST I = RTIO.PutInt;
CONST F = RTIO.PutF;
CONST Flush = RTIO.Flush;

PROCEDURE NL()=
BEGIN
  T("\n");
  Flush();
END NL;

BEGIN
  T("&d "); A(ADR(d)); NL();
  T("&year "); A(ADR(d.year)); NL();
  T("&month "); A(ADR(d.month)); NL();
  T("&day "); A(ADR(d.day)); NL();
  T("&hour "); A(ADR(d.hour)); NL();
  T("&minute "); A(ADR(d.minute)); NL();
  T("&second "); A(ADR(d.second)); NL();
  T("&offset "); A(ADR(d.offset)); NL();
  T("&zone "); A(ADR(d.zone)); NL();
  T("&weekday "); A(ADR(d.weekDay)); NL();

  T("size T "); I(BYTESIZE(d)); NL();
  T("size year "); I(BYTESIZE(d.year)); NL();
  T("size month "); I(BYTESIZE(d.month)); NL();
  T("size day "); I(BYTESIZE(d.day)); NL();
  T("size hour "); I(BYTESIZE(d.hour)); NL();
  T("size minute "); I(BYTESIZE(d.minute)); NL();
  T("size second "); I(BYTESIZE(d.second)); NL();
  T("size offset "); I(BYTESIZE(d.offset)); NL();
  T("size zone "); I(BYTESIZE(d.zone)); NL();
  T("size weekday "); I(BYTESIZE(d.weekDay)); NL();

  T("now "); F(Time.Now()); NL();

  T("or possibly the posix value printed from Win32 ");
  F(Time.Now() - 11644473600.0d0);
  NL();

  d := Date.FromTime(Time.Now(), z := Date.Local);
  T("local year "); I(d.year); NL();
  T("local month "); I(ORD(d.month)); NL();
  T("local day "); I(d.day); NL();
  T("local hour "); I(d.hour); NL();
  T("local minute "); I(d.minute); NL();
  T("local second "); I(d.second); NL();
  T("local offset "); I(d.offset); NL();
  T("local zone "); T(d.zone); NL();
  T("local weekday "); I(ORD(d.weekDay)); NL();

  d := Date.FromTime(Time.Now(), z := Date.UTC);
  T("utc year "); I(d.year); NL();
  T("utc month "); I(ORD(d.month)); NL();
  T("utc day "); I(d.day); NL();
  T("utc hour "); I(d.hour); NL();
  T("utc minute "); I(d.minute); NL();
  T("utc second "); I(d.second); NL();
  T("utc offset "); I(d.offset); NL();
  T("utc zone "); T(d.zone); NL();
  T("utc weekday "); I(ORD(d.weekDay)); NL();

  Flush();

END Main.
