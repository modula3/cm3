MODULE GZipRd;

IMPORT CommandRd, Pathname, OSError;

PROCEDURE Open (p: Pathname.T; ): T RAISES {OSError.E} =
  BEGIN
    RETURN CommandRd.Open("gunzip", ARRAY OF TEXT{"--stdout", p});
  END Open;

BEGIN
END GZipRd.
