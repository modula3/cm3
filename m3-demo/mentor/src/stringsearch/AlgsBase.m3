(* Copyright (C) 1994, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* Last modified on Tue Jan 31 15:40:28 PST 1995 by kalsow   *)
(*      modified on Tue May 24 09:15:33 PDT 1994 by najork   *)
(*      modified on Wed Aug  5 12:13:00 PDT 1992 by guarino  *)
(*      modified on Sat Aug  1 23:03:33 PDT 1992 by broder   *)


MODULE AlgsBase;

IMPORT VBT, Text, FormsVBT, StringSearchAlgClass, ZeusPanel;

PROCEDURE GetData (alg: StringSearchAlgClass.T; VAR p, s: TEXT) =
  BEGIN
    p := "";
    s := "";
    LOCK VBT.mu DO
      TRY
        p := FormsVBT.GetText(alg.data, "pattern");
        s := FormsVBT.GetText(alg.data, "text");
      EXCEPT
      | FormsVBT.Error (msg) => 
	ZeusPanel.ReportError(msg); 
	RETURN;
      | FormsVBT.Unimplemented =>
        ZeusPanel.ReportError("FormsVBT.Unimplemented in Get.Data");
        RETURN;
      END;
    END;
    IF Text.Length(p) = 0 THEN
      ZeusPanel.ReportError("Error: Empty pattern");
    END;
  END GetData;

BEGIN
END AlgsBase.

