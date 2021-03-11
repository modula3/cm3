(* $Id$ *)
MODULE WordUtils;

PROCEDURE FromRails(READONLY rails: Rails): T =
  VAR
    acc := 0;
  BEGIN
    FOR i := 0 TO LAST(rails)  DO
      acc := acc + ORD(rails[i]) *i;
    END;
    RETURN acc;
  END FromRails;

BEGIN
END WordUtils.
