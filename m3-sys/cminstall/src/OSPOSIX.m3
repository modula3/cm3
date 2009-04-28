(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

MODULE OSPOSIX EXPORTS OS;

IMPORT Text2, Msg;
FROM Wr IMPORT EOL;

PROCEDURE GetDiskSpace (<*UNUSED*> dir: TEXT): INTEGER =
  BEGIN
    RETURN LAST (INTEGER); (* big disk! *)
  END GetDiskSpace;

PROCEDURE GetShortFilename (longFilename: TEXT): TEXT RAISES {} =
  BEGIN
    RETURN longFilename;
  END GetShortFilename;

CONST
  WarnWS1 =
    "CM3 currently cannot handle filenames with white space very well." & EOL &
    "The name `";

  WarnWS2 =
    "' has been shortened to `";

  WarnWS3 = "'." & EOL &
    "The installation will only work if you set up an appropriate link.";

PROCEDURE FilenameWithoutSpaces (fn: TEXT): TEXT =
  VAR res := fn;
  BEGIN
    IF Text2.FindChars(fn) THEN
      res := Text2.RemoveChars(fn);
      Msg.Warn(WarnWS1 & fn, WarnWS2 & res, WarnWS3);
    END;
    RETURN res;
  END FilenameWithoutSpaces;

BEGIN
END OSPOSIX.
