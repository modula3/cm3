(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* Last modified on Fri Jun  3 12:36:34 1994 by luca                   *)

MODULE SynLocation;
IMPORT Text, SynWr, Fmt;

VAR setupDone := FALSE;

PROCEDURE PackageSetup() =
  BEGIN
    IF NOT setupDone THEN
      setupDone := TRUE;
      SynWr.Setup();
      Setup();
    END;
  END PackageSetup;

PROCEDURE Setup() =
  BEGIN
    noLocation := NEW(NoLocation);
  END Setup;

REVEAL
  T = BRANDED "Location" OBJECT END;

TYPE
  NoLocation =
    T BRANDED OBJECT END;

  SomeLocation =
    T BRANDED OBJECT
      where: TEXT;
    END;

  LineLocation =
    T BRANDED OBJECT
      fileName: TEXT;
      line, char: INTEGER;
    END;

  CharLocation =
    T BRANDED OBJECT
      fileName: TEXT;
      begChar, endChar: INTEGER;
    END;

PROCEDURE NewLocation(where: TEXT): T =
  BEGIN
    RETURN NEW(SomeLocation, where:=where);
  END NewLocation;

PROCEDURE NewCharLocation(READONLY begInfo, endInfo: Info): T =
  BEGIN
    RETURN 
      NEW(CharLocation, fileName:=begInfo.fileName, 
	begChar:=begInfo.char, endChar:=endInfo.char);
  END NewCharLocation;

PROCEDURE NewLineLocation(READONLY info: Info): T =
  BEGIN
    RETURN NEW(LineLocation, fileName:=info.fileName, 
	line:=info.line, char:=info.lineChar);
  END NewLineLocation;

PROCEDURE PrintLocation(swr: SynWr.T; location: T) =
  BEGIN
    TYPECASE location OF
    | NULL =>
    | NoLocation =>
    | SomeLocation(loc) =>
      IF NOT Text.Empty(loc.where) THEN
        SynWr.Text(swr, "(" & loc.where & ")", loud:=TRUE);
      END;
    | CharLocation(loc) =>
      IF Text.Empty(loc.fileName) THEN
        IF loc.begChar=loc.endChar THEN
          SynWr.Text(swr, "(char " & Fmt.Int(loc.begChar) & ")", loud:=TRUE);
        ELSE
          SynWr.Text(swr, "(chars " & Fmt.Int(loc.begChar) 
	  & ".." & Fmt.Int(loc.endChar) & ")", loud:=TRUE);
	END;
      ELSE
        SynWr.Text(swr, "(file " & loc.fileName, loud:=TRUE);
        IF loc.begChar=loc.endChar THEN
          SynWr.Text(swr, ") (char " & Fmt.Int(loc.begChar) & ")", loud:=TRUE);
        ELSE
          SynWr.Text(swr, ") (chars " & Fmt.Int(loc.begChar) 
	  & ".." & Fmt.Int(loc.endChar) & ")", loud:=TRUE);
	END;
      END;
    | LineLocation(loc) =>
      IF Text.Empty(loc.fileName) THEN
        SynWr.Text(swr, 
          "(input line " & Fmt.Int(loc.line) 
	  & ", char " & Fmt.Int(loc.char) & ")", loud:=TRUE);
      ELSE
        SynWr.Text(swr, "(file " & loc.fileName 
            & ") (line " & Fmt.Int(loc.line) 
	    & ", char " & Fmt.Int(loc.char) & ")", loud:=TRUE);
      END;
    ELSE SynWr.Text(swr, "<unknown location style>", loud:=TRUE);
    END;
  END PrintLocation;

PROCEDURE PrintLineDifference(swr: SynWr.T; location: T; 
    currentLine: INTEGER) =
  VAR relLine: INTEGER;
  BEGIN
    TYPECASE location OF
    | NULL =>
    | LineLocation(loc) =>
        IF Text.Empty(loc.fileName) THEN
	  relLine := loc.line-(currentLine+1);
	  IF relLine=-1 THEN
            SynWr.Text(swr, "(last input line, char " 
	      & Fmt.Int(loc.char) & ")", loud:=TRUE);
	  ELSE
            SynWr.Text(swr, "(input line " & Fmt.Int(relLine) 
	      & ", char " & Fmt.Int(loc.char) & ")", loud:=TRUE);
	  END;
        END;
    ELSE SynWr.Text(swr, "<unknown location style>", loud:=TRUE);
    END;
  END PrintLineDifference;

BEGIN
END SynLocation.
