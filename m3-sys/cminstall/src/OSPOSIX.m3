(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

MODULE OSPOSIX EXPORTS OS;

IMPORT Text2, Msg, System, FSUtils, Lex, Scan, FloatMode, Thread;
FROM Wr IMPORT EOL;

<* FATAL FloatMode.Trap, Thread.Alerted *>

PROCEDURE GetDiskSpace (dir: TEXT): INTEGER =
  VAR
    a1 := "df -k ";
    a2 := " | tail -1 | awk '{print $4}' >df-k";
    data: TEXT;
    res := LAST(INTEGER); (* big disk! *)
  BEGIN
    TRY
      EVAL System.ExecuteList(a1 & dir & a2);
      data := FSUtils.FileContents("df-k");
      res := Scan.Int(data) DIV 1024;
    EXCEPT
      System.ExecuteError(e) =>
      Msg.Warn("Cannot determine free disk space: " & e);
    | Lex.Error =>
      Msg.Warn("Cannot parse result of disk space query");
    | FSUtils.E(e) =>
      Msg.Warn("Cannot read file with free disk space: " & e);
    END;
    RETURN res;
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
