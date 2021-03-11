MODULE RefanyF;

PROCEDURE Hash(<*UNUSED*>ref: T): INTEGER =
  BEGIN
    RETURN 0;
  END Hash;

PROCEDURE Format(<*UNUSED*>ref: T): TEXT =
  BEGIN
    RETURN "<REFANY>";
  END Format;

BEGIN
END RefanyF.
