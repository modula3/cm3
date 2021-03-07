MODULE Main;
IMPORT OSConfig, RTIO;

PROCEDURE a() =
BEGIN
  RTIO.PutText("HostName:");
  RTIO.PutText(OSConfig.HostName());
  RTIO.PutText("\nHostArchitecture:");
  RTIO.PutText(OSConfig.HostArchitecture());
  RTIO.PutText("\nOSName:");
  RTIO.PutText(OSConfig.OSName());
  RTIO.PutText("\nOSVersion:");
  RTIO.PutText(OSConfig.OSVersion());
  RTIO.PutText("\nUserName:");
  RTIO.PutText(OSConfig.UserName());
  RTIO.PutText("\nUserHome:");
  RTIO.PutText(OSConfig.UserHome());
  RTIO.PutText("\n");
  RTIO.Flush();
END a;

BEGIN
  a();
END Main.
