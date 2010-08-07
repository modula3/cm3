(*--------------------------------------------------------------------------*)
MODULE M3Dir EXPORTS Main;

IMPORT OSError, File, Text, TextSeq, RegularFile, Terminal, Fmt, FmtTime,
       ParseParams, Pathname, Stdio, Wr, Process;

IMPORT SMsg AS Msg, FSUtils, PathRepr, System, Creation;
IMPORT FS;

(*--------------------------------------------------------------------------*)
PROCEDURE ListDir(pn : Pathname.T) = 
  VAR
    iter : FS.Iterator;
    fn   : TEXT;
    stat : File.Status;
    type : TEXT;
    size : TEXT;
    time : TEXT;
    line : TEXT;
  BEGIN
    IF NOT FSUtils.IsDir(pn) THEN
      Msg.Error(pn & " seems to be no directory");
      RETURN;
    END;
    TRY
      iter := FS.Iterate(pn);
    EXCEPT
      OSError.E(e) => Msg.Fatal("cannot get iterator for dir " &
        pn & ": " & System.AtomListToText(e));
    END;
    TRY
      IF longList THEN
        WHILE iter.nextWithStatus(fn, stat) DO
          IF stat.type = FS.DirectoryFileType THEN
            type := "d";
          ELSIF stat.type = RegularFile.FileType THEN
            type := "f";
          ELSIF stat.type = Terminal.FileType THEN
            type := "t";
          ELSE
            type := "?";
          END;
          time := FmtTime.Long(stat.modificationTime);
          size := Fmt.LongInt(stat.size);
          line := Fmt.F("%-32s %10s %1s %s", fn, size, type, time);
          Msg.T(line);
        END;
      ELSE
        WHILE iter.next(fn) DO
          Msg.T(fn);
        END;
      END;
    EXCEPT
      OSError.E(e) => Msg.Fatal("iterator.next failed in " &
        pn & ": " & System.AtomListToText(e));
    END;
  END ListDir;

(*--------------------------------------------------------------------------*)
PROCEDURE ProcessParameters() =
  BEGIN
    WITH pp = NEW(ParseParams.T).init(Stdio.stderr) DO
      TRY
	(* help option *)
	IF pp.keywordPresent("-h") OR pp.keywordPresent("-help") THEN
	  VAR help := "usage: m3dir <path>\n"; BEGIN
            TRY Wr.PutText(Stdio.stdout, help) EXCEPT ELSE END;
	    Process.Exit(0);
	  END;
	END;
	(* some message and trace options *)
	IF    pp.keywordPresent("-v") THEN
	  Msg.vFlag := TRUE;
	END;
	IF    pp.keywordPresent("-d") THEN
	  Msg.dFlag := TRUE;
	END;

	(* main options *)
	IF    pp.keywordPresent("-l") THEN
	  longList := TRUE;
	END;

	IF pp.keywordPresent("-recursive") OR pp.keywordPresent("-r") THEN
          recursive := TRUE;
        END;
	IF pp.keywordPresent("-interactive") OR pp.keywordPresent("-i") THEN
          interactive := TRUE;
        END;

        IF pp.keywordPresent("-created") THEN
          Msg.tFlag := TRUE;
          Msg.T(Creation.Date & " on " & Creation.System);
          Process.Exit(0);
        END;
	(* add more options before this line *)
	pp.skipParsed();
	nTargets := NUMBER(pp.arg^) - pp.next;
        (* build parameters *)
	targets := NEW(TextSeq.T).init(nTargets);
	FOR i := 1 TO nTargets DO
	  VAR t := pp.getNext(); BEGIN
	    IF Text.GetChar(t, 0) = '-' THEN
	      Msg.Fatal("unrecognized option: " & t);
	    ELSE
              targets.addhi(PathRepr.Native(t));
	    END;
	  END;
	END;
        pp.finish();
      EXCEPT
        ParseParams.Error => Msg.Fatal("parameter error");
      END;
    END;
    (* all command line parameters handled *)
  END ProcessParameters;

(*--------------------------------------------------------------------------*)
VAR
  longList := FALSE;
  recursive := FALSE;
  interactive := FALSE;
  nTargets : CARDINAL;
  targets : TextSeq.T;
BEGIN
  Msg.tFlag := TRUE;
  ProcessParameters();
  IF nTargets = 0 THEN
    ListDir(".");
  ELSIF nTargets = 1 THEN
    ListDir(targets.get(0));
  ELSE
    FOR i := 0 TO nTargets - 1 DO
      WITH dir = targets.get(i) DO
        Msg.T(" --- " & dir & " --- ");
        ListDir(dir);
        Msg.T("");
      END;
    END;
  END;
END M3Dir.
