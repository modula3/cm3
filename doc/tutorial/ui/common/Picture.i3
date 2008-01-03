INTERFACE Picture;

IMPORT VBT, Text, Rd;

PROCEDURE GetPixmap (rd: Rd.T): VBT.T;
PROCEDURE Get (file: Text.T): VBT.T;

VAR
  stampVBT    : VBT.T;
  wind_millVBT: VBT.T;

END Picture.
