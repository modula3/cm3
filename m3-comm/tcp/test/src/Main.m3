UNSAFE MODULE Main;
IMPORT
 IP, RTIO, Cstdint;
TYPE
 uint8_t = Cstdint.uint8_t;
 Address4 = IP.Address4;

PROCEDURE BoolToText(a: BOOLEAN) : TEXT =
  BEGIN
    RETURN ARRAY OF TEXT{"FALSE", "TRUE"}[ORD(a)];
  END BoolToText;

PROCEDURE PrintBool(f: BOOLEAN) =
BEGIN
  RTIO.PutText(BoolToText(f));
END PrintBool;

PROCEDURE PrintAddress(address: Address4) =
BEGIN
  RTIO.PutInt(LOOPHOLE(ADR(address), UNTRACED REF uint8_t)^);
  RTIO.PutText(".");
  RTIO.PutInt(LOOPHOLE(ADR(address) + 1, UNTRACED REF uint8_t)^);
  RTIO.PutText(".");
  RTIO.PutInt(LOOPHOLE(ADR(address) + 2, UNTRACED REF uint8_t)^);
  RTIO.PutText(".");
  RTIO.PutInt(LOOPHOLE(ADR(address) + 3, UNTRACED REF uint8_t)^);
END PrintAddress;

PROCEDURE A() =
VAR
  ep: IP.EP;
  address: IP.Address4;
  ok := FALSE;
  host := "";
  service := "";
  text := "";
BEGIN

  ok := IP.GetHostByName("www.google.com", address);

  RTIO.PutText("GetHostByName:");
  PrintBool(ok);
  RTIO.PutText("\n");
  PrintAddress(address);
  RTIO.PutText("\n");

  RTIO.PutText("GetHostAddr:");
  PrintAddress(IP.GetHostAddr());
  RTIO.PutText("\n");

  RTIO.PutText("GetCanonicalByName:");
  RTIO.PutText(IP.GetCanonicalByName("192.168.0.1"));
  RTIO.PutText("\n");

  RTIO.PutText("GetCanonicalByName:");
  RTIO.PutText(IP.GetCanonicalByName("www.google.com"));
  RTIO.PutText("\n");

  RTIO.PutText("GetCanonicalByAddr:");
  TRY
      text := IP.GetCanonicalByAddr(IP.GetHostAddr());
      IF text = NIL THEN text := "NIL"; END;
      RTIO.PutText(text);
  EXCEPT
  ELSE
    RTIO.PutText("exception");
  END;
  RTIO.PutText("\n");

  RTIO.PutText("GetAddrInfo+GetNameInfo:192.168 ");
  ep := IP.GetAddrInfo("192.168.0.1", "https");
  IP.GetNameInfo(ep, host, service);
  RTIO.PutText("host:");
  RTIO.PutText(host);
  RTIO.PutText(" service:");
  RTIO.PutText(service);
  RTIO.PutText("\n");

  RTIO.PutText("GetAddrInfo+GetNameInfo:goog ");
  ep := IP.GetAddrInfo("www.google.com", "https");
  IP.GetNameInfo(ep, host, service);
  RTIO.PutText("host:");
  RTIO.PutText(host);
  RTIO.PutText(" service:");
  RTIO.PutText(service);
  RTIO.PutText("\n");

  RTIO.Flush();


END A;

BEGIN
  A();
END Main.
