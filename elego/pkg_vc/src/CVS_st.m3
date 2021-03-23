(*---------------------------------------------------------------------------*)
MODULE CVS_st EXPORTS CVS;

IMPORT Text, TextRd, TextUtils, TextSeq, TextTextTbl, ASCII, Atom,
       FileRevision, FileRevisionSeq, Pipe, Process, OSError, FileWr, Wr,
       File, RegularFile, FileRd, Rd, Pathname, Thread, Env, Fmt, TextRefTbl,
       MxConfig, Random, RdExtras, RTCollector, AtomList, TextTextSeqTbl;
IMPORT TextExtras AS TextEx, SMsg AS Msg;
IMPORT APN AS APN, FileObj, OpResultCache;
IMPORT System, MsgX, MsgIF, OSSpecials, FSUtils, ProcessEnv, PathRepr;
IMPORT PkgVCUtils, CVSLockInfo, CVSLockInfoSeq, TextLockInfoTbl;
IMPORT APNSeq AS APNSeq;
IMPORT (* FSFixed AS *) FS;
FROM TextReadingUtils IMPORT GetToken;

(*---------------------------------------------------------------------------*)
CONST CVSDIR = "CVS";

REVEAL T = Public BRANDED OBJECT 
    cvs       : APN.T;
    dcvs      : APN.T;
    cache     : OpResultCache.T;
    cvsRoot   : TEXT := NIL;
    dcvsRoot  : TEXT := NIL;
    editor    : TEXT;
    cvsignore : TEXT;
    msgif     : MsgIF.T;
    mu        : MUTEX;
    tmpdir    : TEXT;
    dcvsWS    : BOOLEAN := FALSE;
    cvsWS     : BOOLEAN := FALSE;
    preferCVS : BOOLEAN := TRUE;
    cvst      : TEXT := "cvs";
    CVST      : TEXT := "CVS";
    mcvs      : APN.T;
  METHODS
    cvs_call(READONLY args       :  ARRAY OF TEXT; 
             VAR      pid        :  Process.T;
             VAR      out        :  Rd.T;
                      wd         :  Pathname.T := NIL;
                      errfile    :  TEXT := NIL;
                      stdInText  := "a";
                      stdInCount := 100) RAISES {OSError.E} := CVS_Call;
    getCommitMessage(desc := ""; pkg := "") : TEXT := GetCommitMessage;
    checkWS(dir : Pathname.T := NIL) RAISES {OSError.E} := CheckWS;
  OVERRIDES
    init := Init;
    setCVSPath := SetCVSPath;
    setCVSROOT := SetCVSROOT;
    setCVSEDITOR := SetCVSEDITOR;
    setCVSIgnore := SetCVSIgnore;
    setDCVSPath := SetDCVSPath;
    setDCVSROOT := SetDCVSROOT;
    setPreferCVS := SetPreferCVS;
    allFiles := AllFiles;
    logFile := LogFile;
    listSnap := ListSnap;
    snapExists := SnapExists;
    snapList := SnapList;
    readStickySnapFile := ReadStickySnapFile;
    status := Status;
    changeDesc := ChangeDesc;
    setLabel := SetLabel;
    getLabel := GetLabel;
    revisionsAndLabels := RevisionsAndLabels;
    tags := Tags;
    tagsAndSnaps := TagsAndSnaps;
    stateList := StateList;
    taggedRevisions := TaggedRevisions;
    revisionTags := RevisionTags;
    checkout := Checkout;
    commit := Commit;
    update := Update;
    merge := Merge;
    diff := Diff;
    annotate := Annotate;
    tag := Tag;
    tagAgain := TagAgain;
    snapshot := Snapshot;
    modified := Modified;
    upToDate := UpToDate;
    conflicts := Conflicts;
    import := Import;
    add := Add;
    remove := Remove;
    currentStickyTags := CurrentStickyTags;
    flushCache := FlushCache;
    watch := Watch;
    editors := Editors;
    editorInfo := EditorInfo;
    edit := Edit;
    unedit := Unedit;
  END;

(*---------------------------------------------------------------------------*)
PROCEDURE Init(self : T; msgif : MsgIF.T := NIL) : T =
  BEGIN
    self.mu := NEW(MUTEX);
    self.msgif := msgif;
    self.cvs     := APN.New("cvs");
    self.dcvs    := APN.New("dcvs");
    self.cvsRoot := NIL;
    self.editor  := Env.Get("CVSEDITOR");
    IF self.editor = NIL THEN
      self.editor := Env.Get("CVS_EDITOR");
    END;
    IF self.editor = NIL THEN
      self.editor := Env.Get("VISUAL");
    END;
    IF self.editor = NIL THEN
      self.editor := Env.Get("EDITOR");
    END;
    IF self.editor = NIL THEN
      self.editor := "emacsclient";
    END;
    self.cache := OpResultCache.New();
    self.cvsignore := "*.bak *~ *.gz *.o *.obj *.exe *.zip *.tgz *.so *.So";

    self.tmpdir := Env.Get("TMP");
    IF self.tmpdir = NIL THEN
      self.tmpdir := Env.Get("TMPDIR");
    END;
    IF self.tmpdir = NIL THEN
      self.tmpdir := Env.Get("TEMP");
    END;
    IF self.tmpdir # NIL AND NOT FSUtils.IsDir(self.tmpdir) THEN
      MsgX.Warning(self.msgif, 
          "Invalid setting of environment variables TMP, TEMP, or TMPDIR");
      self.tmpdir := NIL;
    END;
    IF self.tmpdir = NIL THEN
      VAR
        try := ARRAY [0..8] OF TEXT {
          "/var/tmp",
          "/usr/tmp",
          "/tmp",
          "/temp",
          "c:/tmp",
          "c:/temp",
          "d:/tmp",
          "d:/temp",
          "."
        };
        (* FIXME: on Windows we should inquire for the system drive *)
        i := 0;
      BEGIN
        WHILE i < 9 AND NOT FSUtils.IsDir(PathRepr.Native(try[i])) DO
          INC (i);
        END;
        IF i < 9 THEN
          self.tmpdir := PathRepr.Native(try[i]);
        END;
      END;
    END;
    IF self.tmpdir = NIL THEN
      self.tmpdir := "";
    ELSE
      tmp := self.tmpdir;
    END;
    RETURN self;
  END Init;

(*---------------------------------------------------------------------------*)
PROCEDURE CheckWS(self : T; dir : Pathname.T := NIL) RAISES {OSError.E} =
  BEGIN
    IF dir # NIL AND Text.Empty(dir) THEN
      dir := NIL;
    END;
    IF dir = NIL THEN
      dir := Process.GetWorkingDirectory();
    END;
    MsgX.D(self.msgif, "CheckWS(" & dir & ")");
    IF NOT self.cvsWS AND NOT self.dcvsWS THEN
      IF FSUtils.IsDir(Pathname.Join(dir, "DCVS", NIL)) THEN
        self.dcvsWS := TRUE;
      ELSE (* no CVS or DCVS subdirectory found *)
        self.cvsWS := self.preferCVS;
        self.dcvsWS := NOT self.preferCVS;
      END;
    END;
    IF self.dcvsWS THEN
      self.cvst := "dcvs";
      self.CVST := "DCVS";
      self.mcvs := self.dcvs;
    ELSIF self.cvsWS THEN
      self.cvst := "cvs";
      self.CVST := "CVS";
      self.mcvs := self.cvs;
    END;
  END CheckWS;

(*---------------------------------------------------------------------------*)
PROCEDURE SetCVSROOT(self : T; cvsroot : TEXT) =
  BEGIN
    LOCK self.mu DO
      self.cvsRoot := cvsroot;
    END;
  END SetCVSROOT;

(*---------------------------------------------------------------------------*)
PROCEDURE SetCVSEDITOR(self : T; cvseditor : TEXT) =
  BEGIN
    LOCK self.mu DO
      self.editor := cvseditor;
    END;
  END SetCVSEDITOR;

(*---------------------------------------------------------------------------*)
PROCEDURE SetCVSIgnore(self : T; ignorePatterns : TEXT) =
  BEGIN
    LOCK self.mu DO
      self.cvsignore := ignorePatterns;
    END;
  END SetCVSIgnore;

(*---------------------------------------------------------------------------*)
TYPE 
  NullRd = Thread.Closure OBJECT
    rd    : FileRd.T;
    msgif : MsgIF.T := NIL;
  OVERRIDES
    apply := NullRdProc;
  END;

(*---------------------------------------------------------------------------*)
PROCEDURE SetDCVSROOT(self : T; dcvsroot : TEXT) =
  BEGIN
    LOCK self.mu DO
      self.dcvsRoot := dcvsroot;
    END;
  END SetDCVSROOT;

(*---------------------------------------------------------------------------*)
PROCEDURE NullRdProc(self : NullRd) : REFANY =
  VAR 
    buf  : ARRAY [0..511] OF CHAR;
    line : TEXT;
  BEGIN
    TRY
      WHILE NOT Rd.EOF(self.rd) DO
        IF Msg.dFlag THEN
          line := Rd.GetLine(self.rd);
          MsgX.D(self.msgif, "  /dev/null: " & line);
        ELSE
          EVAL Rd.GetSub(self.rd, buf);
        END;
      END;
    EXCEPT 
      Rd.EndOfFile => (* skip *)
    | Rd.Failure => 
      MsgX.Fatal2(self.msgif, "NullRdProc", "reader failure");
    | Thread.Alerted =>
      MsgX.Fatal2(self.msgif, "NullRdProc", "thread alerted");
    ELSE 
      MsgX.Error2(self.msgif, "NullRdProc", "exception while consuming");
    END;
    TRY 
      IF self.rd # NIL THEN
        Rd.Close(self.rd); 
      END;
    EXCEPT ELSE 
      MsgX.Error2(self.msgif, "NullRdProc", "exception while closing");
    END;
    RETURN NIL;
  END NullRdProc;

(*---------------------------------------------------------------------------*)
PROCEDURE ArrayToText(READONLY a : ARRAY OF TEXT) : TEXT =
  VAR res : TEXT := "";
  BEGIN
    FOR i := FIRST(a) TO LAST(a) DO
      IF i = FIRST(a) THEN
        res := a[FIRST(a)]; 
      ELSE
        res := res & " " & a[i];
      END;
    END;
    RETURN res;
  END ArrayToText;

(*---------------------------------------------------------------------------*)
PROCEDURE CvsLock() =
  BEGIN
    IF useCvsLock THEN
      Thread.Acquire(cvsLock);
    END;
    IF cvsDisableVM THEN
      RTCollector.Disable();
    END;
  END CvsLock;

(*---------------------------------------------------------------------------*)
PROCEDURE CvsUnlock() =
  BEGIN
    IF cvsDisableVM THEN
      RTCollector.Enable();
    END;
    IF useCvsLock THEN
      Thread.Acquire(cvsLock);
    END;
  END CvsUnlock;

(*---------------------------------------------------------------------------*)
PROCEDURE CVS_Call(self                :  T; 
                   READONLY args       :  ARRAY OF TEXT; 
                   VAR      pid        :  Process.T;
                   VAR      out        :  Rd.T;
                            wd         :  Pathname.T := NIL;
                            errfile    :  TEXT := NIL;
                            stdInText  := "a";
                            stdInCount := 100) RAISES {OSError.E} =
  VAR 
    childOutWr, selfOutRd : Pipe.T; errWr : File.T; wdir := ".";
    env : ProcessEnv.T; sysenv : REF ARRAY OF TEXT := NIL;
    cvsRootText := "NIL";
    stdinFile : File.T := NIL;
  BEGIN
    IF wd # NIL AND Text.Empty(wd) THEN
      wd := NIL;
    END;
    IF wd = NIL THEN
      wdir := Process.GetWorkingDirectory();
    ELSE
      wdir := wd;
    END;
    self.checkWS(wdir);
    IF self.cvsWS AND self.cvsRoot # NIL THEN
      cvsRootText := self.cvsRoot;
    ELSIF self.dcvsWS AND self.dcvsRoot # NIL THEN
      cvsRootText := self.dcvsRoot;
    END;
    IF noAction THEN
      MsgX.T(self.msgif, self.CVST & " call: " & self.mcvs.denotation() & " " &
        ArrayToText(args) & " wdir = " & wdir & " " & self.CVST & "ROOT = " & 
        cvsRootText);
      RETURN;
    END;
    MsgX.D(self.msgif, self.CVST & " call: " & self.mcvs.denotation() & " " & 
      ArrayToText(args) & " wdir = " & wdir & " " & self.CVST & "ROOT = " &
      cvsRootText);
    WITH b = APN.LastBase(self.mcvs).denotation(),
         pos = TextUtils.Pos(b, "cvs", FALSE) DO
      IF pos # 0 AND pos # 1 AND Text.GetChar(b, 0) # 'd' THEN
        RAISE OSError.E(AtomList.List2(
                            Atom.FromText("not accepted as cvs executable:"),
                            Atom.FromText(self.mcvs.denotation())));
      END;
    END;
    env := ProcessEnv.Current();
    IF self.cvsWS THEN
      IF self.cvsRoot # NIL THEN
        ProcessEnv.Set(env, "CVSROOT", self.cvsRoot);
      END;
      ProcessEnv.Set(env, "CVSIGNORE", self.cvsignore);
    ELSIF self.dcvsWS THEN
      IF self.dcvsRoot # NIL THEN
        ProcessEnv.Set(env, "DCVSROOT", self.dcvsRoot);
      END;
      ProcessEnv.Set(env, "DCVSIGNORE", self.cvsignore);
    END;
    sysenv := ProcessEnv.SystemRepr(env);
    Pipe.Open(hr := selfOutRd, hw := childOutWr);
    IF errfile # NIL THEN
      TRY
        errWr := FS.OpenFile(errfile);
      EXCEPT
        OSError.E(a) => MsgX.Error2(self.msgif, self.CVST & "_Call", 
                                    "cannot open file " & errfile &
                                    " (" & Atom.ToText(a.head) & ")");
      END;
    ELSE
      IF Msg.dFlag THEN
        VAR hin, hout, herr : File.T; BEGIN
          Process.GetStandardFileHandles(hin, hout, herr);
          errWr := herr; 
        END;
      ELSE
        errWr := nullDev;
      END;
    END;
    CvsLock();
    stdinFile := GetPseudoStdinFD(self, stdInText, stdInCount);
    pid := Process.Create(self.mcvs.denotation(), args, sysenv, wd, 
                          stdinFile, childOutWr, errWr);
    TRY
      childOutWr.close();
    EXCEPT
      OSError.E(a) => MsgX.Error2(self.msgif, self.CVST & "_Call", 
                                  "OSError closing pipes in parent"&
                                  " (" & Atom.ToText(a.head) & ")");
    END;
    IF errfile # NIL AND errWr # NIL THEN
      TRY
        errWr.close();
      EXCEPT ELSE
        MsgX.Error2(self.msgif, self.CVST & "_Call", 
                    "exception closing error file");
      END;
    END;
    out := NEW(FileRd.T).init(selfOutRd);
    (* we try to keep the file open for future use...
    IF stdinFile # NIL THEN
      TRY stdinFile.close() EXCEPT ELSE END;
    END;
    *)
  END CVS_Call;

(*---------------------------------------------------------------------------*)
PROCEDURE FileName() : TEXT =
  VAR
    m   : INTEGER;
    c   : CHAR;
    res : TEXT := "";
  BEGIN
    FOR i := 1 TO 8 DO
      REPEAT
        m := random.integer(65,122);
      UNTIL VAL(m, ASCII.Range) IN ASCII.Letters;
      c := VAL(m, CHAR);
      res := res & Text.FromChar(c);
    END;
    RETURN res;
  END FileName;

(*---------------------------------------------------------------------------*)
PROCEDURE ErrlogFilename(self : T; defaultName := "errlog") : TEXT =
  VAR 
    pid := Fmt.Int(Process.GetMyID());
    tmp := self.tmpdir;
    suf := Text.FromChar(fileSuffixC);
  BEGIN
    INC(fileSuffixC);
    IF fileSuffixC > 'z' THEN
      fileSuffixC := 'A';
    END;
    IF fileSuffixC > 'Z' THEN
      fileSuffixC := 'a';
    END;
    IF tmp = NIL THEN
      RETURN defaultName & pid & suf;
    ELSE
      RETURN Pathname.Join(PathRepr.Native(tmp), FileName() & pid & suf, NIL);
    END;
  END ErrlogFilename;

(*---------------------------------------------------------------------------*)
PROCEDURE RemoveErrlogFile(self : T; fn : TEXT) =
  BEGIN
    IF NOT FSUtils.Exists(fn) THEN RETURN END;
    IF FSUtils.IsFile(fn) THEN
      TRY
        FS.DeleteFile(fn);
      EXCEPT
        OSError.E => MsgX.Error(self.msgif, "Cannot remove file " & fn);
      END;
    ELSE
      MsgX.Fatal(self.msgif, 
                 "internal error: cannot remove non-regular file " & fn);
    END;
  END RemoveErrlogFile;

(*---------------------------------------------------------------------------*)
PROCEDURE StdinFilename(text : TEXT; defaultName := "PseudoStdinCVS") : TEXT =
  VAR 
    pid := Fmt.Int(Process.GetMyID());
  BEGIN
    IF tmp = NIL THEN
      RETURN defaultName & pid & "-" & text;
    ELSE
      RETURN Pathname.Join(PathRepr.Native(tmp), defaultName & pid & 
             "-" & text, NIL);
    END;
  END StdinFilename;

(*---------------------------------------------------------------------------*)
PROCEDURE GetPseudoStdinFD(self : T; text := "a"; n := 100) : File.T = 
  VAR
    ref       : REFANY;
    stdinFile : File.T;
    stdinFN   : TEXT;
  BEGIN
    LOCK stdinLock DO
      stdinFN := StdinFilename(text);
      IF ftab.get(text, ref) AND ISTYPE(ref, File.T) THEN
        MsgX.D(self.msgif, "found old File.T object " & stdinFN);
        stdinFile := NARROW(ref, File.T);
        TRY 
          EVAL stdinFile.status();
          MsgX.D(self.msgif, "using existing pseudo stdin file " & stdinFN);
          RETURN stdinFile;
        EXCEPT
          OSError.E => (* skip *)
        END;
        TRY stdinFile.close(); EXCEPT ELSE END;
      END;
      MsgX.D(self.msgif, "trying to create new pseudo stdin file " & 
	stdinFN);
      TRY
	WITH wr = FileWr.Open(stdinFN) DO
	  FOR i := 1 TO n DO
	    Wr.PutText(wr, text & "\r");
	  END;
	  Wr.Close(wr);
	END;
      EXCEPT ELSE
	IF FSUtils.IsFile(stdinFN) THEN
	  MsgX.Warning(self.msgif, "cannot create pseudo stdin file " & 
		       stdinFN & ", using existing one");
	ELSE
	  MsgX.Error(self.msgif, "cannot create pseudo stdin file " & 
		     stdinFN);
	  RETURN NIL;
	END;
      END;
      TRY
        MsgX.D(self.msgif, "opening pseudo stdin file " & stdinFN);
        stdinFile := FS.OpenFileReadonly(stdinFN);
      EXCEPT ELSE
        MsgX.Error(self.msgif, "cannot open pseudo stdin file " & stdinFN);
        RETURN NIL;
      END;
      EVAL ftab.put(text, stdinFile);
      RETURN stdinFile;
    END;
  END GetPseudoStdinFD;

(*---------------------------------------------------------------------------*)
PROCEDURE EvalCVSErrors(msgif : MsgIF.T; errfile : TEXT; VAR res : TEXT; 
                        terminate := TRUE) : BOOLEAN =
  VAR 
    rd : FileRd.T;
    line : TEXT;
    error := FALSE;

  (*-------------------------------------------------------------------------*)
  PROCEDURE Matches(READONLY t : TEXT) : BOOLEAN =
    VAR i : CARDINAL := 0;
    BEGIN
      IF TextEx.FindSub(line, t, i) THEN
        RETURN TRUE;
      END;
      RETURN FALSE;
    END Matches;

  (*-------------------------------------------------------------------------*)
  PROCEDURE MatchError(READONLY t : TEXT) : BOOLEAN =
    VAR i : CARDINAL := 0;
    BEGIN
      IF TextEx.FindSub(line, t, i) THEN
        IF Msg.dFlag THEN
          MsgX.Error2(msgif, "MatchError", "(D)CVS error: " & line);
        END;
        RETURN TRUE;
      END;
      RETURN FALSE;
    END MatchError;

  (*-------------------------------------------------------------------------*)
  BEGIN (* EvalCVSErrors *)
    IF res = NIL THEN
      res := "";
    END;
    TRY
      rd := FileRd.Open(errfile);
      WHILE NOT Rd.EOF(rd) DO
        line := Rd.GetLine(rd);
        IF NOT (Matches("cvs update: Updating") OR
                Matches("cvs status: Examining") OR
                Matches("cvs log: Logging") OR
                Matches("cvs tag: Tagging") OR
                Matches("conflicts found") OR
                Matches("Warning:") OR
	        Matches("no comparison available") OR
                FALSE) THEN
          IF
            (* This is way too general; it catches perfectly
               harmless messages like
               "cvs server: scheduling file `strerror.c' for addition"
               as CVS errors! *)
            (*
              MatchError("error") OR
              MatchError("failure") OR
              MatchError("Invalid") OR
              MatchError("invalid") OR
              MatchError("illegal") OR
              MatchError("fatal") OR
            *)
            MatchError("ERROR: ") OR
            MatchError("conflict: ") OR
            MatchError("Help!") OR
            MatchError("not supported") OR
            MatchError("*PANIC*") OR
            MatchError("Sorry, ") OR
            MatchError("unrecognized") OR
            MatchError("ore dumped") OR
            MatchError("is ambiguous") OR
            MatchError("server bug:") OR
            MatchError("update aborted") OR

            MatchError("Service unavailable") OR
            MatchError("Usage: ") OR
            MatchError("nothing known about") OR
            MatchError("added independently by second party") OR
            MatchError("has already been entered") OR
            MatchError("should be removed and is still there") OR
            MatchError("not added; must be a direct sub-directory") OR
            MatchError("already exists") OR
            MatchError("already contains") OR
            MatchError("usage is restricted to members of the group") OR
            MatchError("-q or -Q must be specified before") OR
            MatchError("only two -j options can be specified") OR
            MatchError("are mutually exclusive") OR
            MatchError("must specify ") OR
            MatchError("must not specify") OR
            MatchError("must be a symbolic tag") OR

            MatchError("there is no repository") OR
            MatchError("does not match") OR
            MatchError("it is in the way") OR
            MatchError("should be removed and is still there") OR
            MatchError("is modified but no longer in the repository") OR
            MatchError("premature end of file from server") OR
            MatchError("server bug:") OR
            MatchError("Checksum received before last one was used") OR
            MatchError("bad entries line") OR
            MatchError("bad revisions") OR
            MatchError("unexpected end of file from server") OR
            MatchError("gzip process exited") OR
            MatchError("can't stat patch file") OR
            MatchError("unable to remove") OR
            MatchError("dying gasps from") OR
            MatchError("does not support") OR
            MatchError("server doesn't support") OR
            MatchError("you are unknown to this system") OR
            MatchError("is not a branch") OR
            MatchError("had a conflict and has not been modified") OR
            MatchError("still contains conflict indicators") OR
            MatchError("boy, I'm confused") OR
            MatchError("was initially added on branch") OR
            MatchError("there is a version in") OR

            MatchError("not allocate") OR
            MatchError("not reallocate") OR
            MatchError("not open") OR
            MatchError("not create") OR
            MatchError("not close") OR
            MatchError("not read") OR
            MatchError("not write") OR
            MatchError("not find") OR
            MatchError("not remove") OR
            MatchError("not add") OR
            MatchError("not seek") OR
            MatchError("not chdir") OR
            MatchError("not mkdir") OR
            MatchError("not commit") OR
            MatchError("not fdopen") OR
            MatchError("not stat") OR
            MatchError("not fstat") OR
            MatchError("not fsync") OR
            MatchError("not dup2") OR
            MatchError("not exec") OR
            MatchError("not fork") OR
            MatchError("not utime") OR
            MatchError("not lock") OR
            MatchError("not return") OR
            MatchError("not unlock") OR
            MatchError("not change") OR
            MatchError("not make") OR
            MatchError("not check out") OR
            MatchError("not check in") OR
            MatchError("not resurrect") OR
            MatchError("not diff") OR
            MatchError("not merge") OR
            MatchError("not admin") OR
            MatchError("not resurrect") OR
            MatchError("not restore") OR
            MatchError("not be added") OR
            MatchError("not start server via rsh") OR
            MatchError("not get working directory") OR
            MatchError("not specify both a message and a log file") OR
            MatchError("not check out files into the repository itself") OR
            MatchError("not stub branch") OR
            MatchError("not reparse") OR

            MatchError("ouldn't stat") OR
            MatchError("ouldn't open") OR
            MatchError("ouldn't tag") OR
            MatchError("ouldn't exec") OR

            MatchError("an't stat") OR
            MatchError("an't getwd") OR
            MatchError("an't chdir") OR
            MatchError("an't parse") OR
            MatchError("an't set") OR

            MatchError("failed") OR
            MatchError("no more than two revisions/dates can be specified") OR
            MatchError("I know nothing about") OR
            MatchError("is not in file") OR
            MatchError("no revision for date") OR
            MatchError("is not a valid report type") OR
            MatchError("is not a known time zone") OR
            MatchError("Only one report type allowed") OR
            MatchError("null dir and file name") OR
            MatchError("history file is empty") OR
            MatchError("No tag found") OR
            MatchError("No module, file or repository with") OR
            MatchError("No records selected") OR
            MatchError("Hey! What is this junk?") OR
            MatchError("missing CVSROOT environment variable") OR
            MatchError("missing DCVSROOT environment variable") OR
            MatchError("Set it or specify the '-d' option") OR
            MatchError("unknown lock status") OR
            MatchError("unknown file status") OR
            MatchError("Log message unchanged or not specified") OR
            MatchError("Unknown input") OR
            MatchError("gzip compression level must be between 1 and 9") OR
            MatchError("You don't have") OR
            MatchError("bad regular expression") OR
            MatchError("second date must come after first date") OR
            MatchError("does not appear to be a valid rcs file") OR
            MatchError("unable to reopen") OR
            MatchError("unable to parse") OR
            MatchError("unable to release") OR
            MatchError("unable to remove") OR
            MatchError("unable to write") OR
            MatchError("has non-visible graphic characters") OR
            MatchError("must not contain the characters") OR
            MatchError("must start with a letter") OR
            MatchError("still in working directory") OR
            MatchError("there is no version here") OR
            MatchError("must set the CVSROOT environment variable") OR
            MatchError("must set the DCVSROOT environment variable") OR
            MatchError("there is no repository") OR
            MatchError("please correct this problem") OR
            MatchError("end of file reading mode") OR
            MatchError("premature end of file from client") OR
            MatchError("short read from client") OR
            MatchError("end of file reading size") OR
            MatchError("Virtual memory exhausted") OR
            MatchError("ut of memory") OR
            MatchError("skipping removed but un-commited file") OR
            MatchError("exists; use") OR
            MatchError("no such directory") OR
            MatchError("no repository module") THEN
            error := TRUE;
            res := res & "\nCVS error: " & line;
          END (* IF *);
        END (* IF *);
      END (* WHILE *);
    EXCEPT
      Rd.EndOfFile => (* skip *)
    | Rd.Failure   => 
      MsgX.Fatal2(msgif, "EvalCVSErrors", "error reading " & errfile);
    | OSError.E(a) => 
      MsgX.Fatal2(msgif, "EvalCVSErrors", "error opening " & errfile &
      " (" & Atom.ToText(a.head) & ")"); 
    | Thread.Alerted => 
      MsgX.Fatal2(msgif, "EvalCVSErrors", "thread alerted");
    END;
    TRY
      IF rd # NIL THEN
        Rd.Close(rd);
      END;
    EXCEPT 
    | Rd.Failure   => 
      MsgX.Fatal2(msgif, "EvalCVSErrors", "error closing " & errfile);
    | OSError.E(a) => 
      MsgX.Fatal2(msgif, "EvalCVSErrors", "OSError closing " & 
        errfile & " (" & Atom.ToText(a.head) & ")"); 
    ELSE
      MsgX.Fatal2(msgif, "EvalCVSErrors", "exception closing error reader");
    END;
    TRY
      IF NOT Msg.dFlag THEN
        (* the output of CVS stderr is left in place if we are debugging *)
        FS.DeleteFile(errfile);
      END;
    EXCEPT
    | OSError.E(a) => MsgX.Error2(msgif, "EvalCVSErrors", "error removing " 
      & errfile & " (" & Atom.ToText(a.head) & ")"); 
    END;
    IF error AND terminate THEN
      MsgX.Fatal(msgif, "(D)CVS error(s) encountered; cannot continue.\n" &
        res);
    END;
    RETURN NOT error;
  END EvalCVSErrors;

(*---------------------------------------------------------------------------*)
PROCEDURE AddEntries(self : T; dir, fn : Pathname.T;
                     list : FileRevisionSeq.T; VAR dirlist : TextSeq.T;) =
  VAR
    rd    : FileRd.T := NIL;
    line  : TEXT;
    fname : Pathname.T;
    name  : TEXT;
    rev   : TEXT;
    c     : CHAR;
    i, j  : INTEGER;
  BEGIN
    Msg.D("AddEntries(" & dir & ", " & fn & ")");
    IF dirlist = NIL THEN
      dirlist := NEW(TextSeq.T).init();
    END;
    TRY
      TRY
	rd := FileRd.Open(fn);
	WHILE NOT Rd.EOF(rd) DO
	  line := Rd.GetLine(rd);
	  IF line # NIL AND Text.Length(line) > 1 THEN
	    c := Text.GetChar(line, 0);
	    IF c = 'A' THEN
	      (* new directory entry, different format :-( *)
              IF Text.Length(line) > 2 THEN
                line := Text.Sub(line, 2);
                c := Text.GetChar(line, 0);
              END;
            END;
	    IF c = 'D' THEN
	      (* directory entry *)
	      c := Text.GetChar(line, 1);
	      i := Text.FindChar(line, c, 2);
	      IF i > 0 THEN
		name := Text.Sub(line, 2, i - 2);
		(* MsgX.Debug(self.msgif, "  dir name = " & 
                   name & "  dir = " & dir); *)
		TRY
		  fname := FSUtils.CanonicalPathname(
			       Pathname.Join(dir, name, NIL));
                  IF debugFindCVSFiles THEN
                    Msg.D("    dir  " & name & " " & fname);
                  END;
		  dirlist.addhi(fname);
		EXCEPT ELSE
		  MsgX.Error(self.msgif, "CanonicalPathname failed for " & 
                    name & " in dir " & dir);
		END;
	      END;
	    ELSE
	      (* ordinary file *)
	      i := Text.FindChar(line, c, 1);
	      IF i > 0 THEN
                name := Text.Sub(line, 1, i - 1);
                (* MsgX.Debug(self.msgif, "  fn name = " & name & 
                   "  dir = " & dir); *)
                TRY
                  fname := FSUtils.CanonicalPathname(
                               Pathname.Join(dir, name, NIL));
                IF debugFindCVSFiles THEN
                  Msg.D("    file " & name & " " & fname);
                END;
                EXCEPT ELSE
                  MsgX.Error(self.msgif, "CanonicalPathname failed for " &
                    name & " in dir " & dir);
                END;
		INC(i);
		j := Text.FindChar(line, c, i);
		IF j > 0 THEN
		  rev := Text.Sub(line, i, j - i);
		  list.addhi(NEW(FileRevision.T, 
				 file := APN.New(fname), revision := rev));
		END;
	      END;
            END;
	  END;
	END;
      EXCEPT 
	OSError.E(l)   => 
        MsgX.Error(self.msgif, "error during evaluation of " & 
          fn & ": " & System.AtomListToText(l));
      | Rd.Failure     => 
        MsgX.Error(self.msgif, "reader failure during evaluation of " &
          fn);
      | Thread.Alerted => 
        MsgX.Error(self.msgif, "thread alerted during evaluation of " &
          fn);
      | Rd.EndOfFile => (* skip *)
      END;
    FINALLY
      TRY
        IF rd # NIL THEN
          Rd.Close(rd);
        END;
      EXCEPT ELSE END;
    END;
  END AddEntries;

(*---------------------------------------------------------------------------*)
PROCEDURE FindCVSFiles(self : T; dir : Pathname.T; 
                       list : FileRevisionSeq.T; recurse := TRUE) 
  RAISES {OSError.E} =
  VAR
    stat    : File.Status;
    entries : TEXT;
    name    : TEXT;
    dirlist : TextSeq.T := NIL;
  BEGIN
    Msg.D("FindCVSFiles(" & dir & ")");
    TRY
      stat := FS.Status(dir);
    EXCEPT 
      OSError.E(l)   => MsgX.Error(self.msgif, "cannot stat " & 
        dir & ": " & System.AtomListToText(l));
      RETURN;
    END;
    IF stat.type # FS.DirectoryFileType THEN
      RETURN;
    END;
    self.checkWS(dir);
    (* add entries from CVS/Entries, if existent *)
    TRY
      entries := Pathname.Join(dir, CVSDIR, NIL);
      entries := Pathname.Join(entries, "Entries", NIL);
      stat := FS.Status(entries);
      IF stat.type = RegularFile.FileType THEN
        AddEntries(self, dir, entries, list, dirlist);
      END;
    EXCEPT 
      OSError.E => (* skip; completely okay *)
    END;
    TRY
      entries := Pathname.Join(dir, CVSDIR, NIL);
      entries := Pathname.Join(entries, "Entries", "Log");
      stat := FS.Status(entries);
      IF stat.type = RegularFile.FileType THEN
        AddEntries(self, dir, entries, list, dirlist);
      END;
    EXCEPT 
      OSError.E => (* skip; completely okay *)
    END;
    IF dirlist = NIL THEN RETURN END;
    (* traverse all subdirectories known to CVS *)
    FOR i := 0 TO dirlist.size() - 1 DO
      name := dirlist.get(i);
      IF debugFindCVSFiles THEN
        Msg.D("  subdir " & name);
      END;
      TRY
        stat := FS.Status(name);
        IF stat.type = FS.DirectoryFileType AND name # NIL 
          AND recurse AND NOT Text.Equal(name, CVSDIR) THEN
          FindCVSFiles(self, name, list);
        END;
      EXCEPT ELSE
        MsgX.Error(self.msgif, "stat failed for dir " & name);
      END;
    END;
  END FindCVSFiles;

(*---------------------------------------------------------------------------*)
PROCEDURE SetCVSPath(self : T; cvsPath : APN.T) =
  BEGIN
    LOCK self.mu DO
      self.cvs := cvsPath;
    END;
  END SetCVSPath;

(*---------------------------------------------------------------------------*)
PROCEDURE SetDCVSPath(self : T; dcvsPath : APN.T) =
  BEGIN
    LOCK self.mu DO
      self.dcvs := dcvsPath;
    END;
  END SetDCVSPath;

(*---------------------------------------------------------------------------*)
PROCEDURE SetPreferCVS(self : T; val : BOOLEAN := TRUE) =
  BEGIN
    IF val THEN
      Msg.D("SetPreferCVS(TRUE)");
    ELSE
      Msg.D("SetPreferCVS(FALSE)");
    END;
    self.preferCVS := val;
  END SetPreferCVS;

(*---------------------------------------------------------------------------*)
PROCEDURE AllFiles(self : T; dir : APN.T) : FileRevisionSeq.T =
  VAR res := NEW(FileRevisionSeq.T).init(200);
  BEGIN
    LOCK self.mu DO
      TRY
        FindCVSFiles(self, dir.denotation(), res);
      EXCEPT
        OSError.E(e) =>
        MsgX.Error(self.msgif, "os error during file system operation: " &
                   Atom.ToText(e.head));
      END;
      RETURN res;
    END;
  END AllFiles;

(*---------------------------------------------------------------------------*)
PROCEDURE LogFile(self : T; fn : APN.T; heedErrors := TRUE;
                  headerOnly := FALSE; local := FALSE) : TEXT RAISES {E} =
  VAR 
    outRd   :  Rd.T;
    pid     :  Process.T;
    args    :  REF ARRAY OF TEXT;
    res     := "";
    wd      :  APN.T;
    fnp     := fn.denotation();
    errfn   := ErrlogFilename(self);
    closeRd := TRUE;
    ret     :  INTEGER;
    errmsg  :  TEXT := NIL;
    cacheOpName : TEXT;
  BEGIN
    cacheOpName := "LogFile";
    IF headerOnly THEN
      cacheOpName := "LogFileH";
    END;
    IF local THEN
      cacheOpName := cacheOpName & "L";
    END;
    LOCK self.mu DO
    IF self.cache.contains(cacheOpName, fnp) THEN
      RETURN self.cache.getText(cacheOpName, fnp);
    END;
    IF headerOnly AND local THEN
      args := NEW(REF ARRAY OF TEXT, 4);
      args^[0] := "log";
      args^[1] := "-h";
      args^[2] := "-l";
      args^[3] := Pathname.Last(fn.denotation(APN.Type.Native));
    ELSIF headerOnly AND NOT local THEN
      args := NEW(REF ARRAY OF TEXT, 3);
      args^[0] := "log";
      args^[1] := "-h";
      args^[2] := Pathname.Last(fn.denotation(APN.Type.Native));
    ELSIF NOT headerOnly AND local THEN
      args := NEW(REF ARRAY OF TEXT, 3);
      args^[0] := "log";
      args^[1] := "-l";
      args^[2] := Pathname.Last(fn.denotation(APN.Type.Native));
    ELSE
      args := NEW(REF ARRAY OF TEXT, 2);
      args^[0] := "log";
      args^[1] := Pathname.Last(fn.denotation(APN.Type.Native));
    END;
    wd := APN.New(Pathname.Prefix(fn.denotation(APN.Type.Native)));
    TRY
      CVS_Call(self, args^, pid, outRd, wd.denotation(), errfn);
      IF outRd # NIL THEN
        res := Rd.GetText(outRd, LAST(CARDINAL));
      END;
    EXCEPT 
    | Rd.Failure     => 
      errmsg := "reader failure";
      MsgX.Error2(self.msgif, "LogFile", errmsg);
    | OSError.E      => 
      errmsg := "execution of " & self.mcvs.denotation() & " failed";
      MsgX.Error2(self.msgif, "LogFile", errmsg);
                        closeRd := FALSE;
    | Thread.Alerted => 
      errmsg := "exception while reading log";
      MsgX.Error2(self.msgif, "LogFile", errmsg);
    END;
    IF noAction THEN RETURN "empty log" END;
    TRY
      IF closeRd AND outRd # NIL THEN
        Rd.Close(outRd);
        ret := Process.Wait(pid);
        CvsUnlock();
      END;
      MsgX.D(self.msgif, self.cvst & " exited with " & Fmt.Int(ret));
      (* does not work reliably 
      IF ret # 0 THEN
        errmsg := self.cvst & " exited with " & Fmt.Int(ret);
      END;
      *)
    EXCEPT ELSE 
      MsgX.Error2(self.msgif, "LogFile", "exception in finalization");
    END;
    TRY
      IF heedErrors THEN
        IF errmsg # NIL THEN
          RAISE E(errmsg);
        END;
        IF NOT EvalCVSErrors(self.msgif, errfn, res) THEN
          RemoveErrlogFile(self, errfn);
          RAISE E(res);
        END;
      END;
      self.cache.putText(cacheOpName, fnp, res);
    FINALLY
      RemoveErrlogFile(self, errfn);
    END;
    RETURN res;
    END (* lock *)
  END LogFile;

(*---------------------------------------------------------------------------*)
PROCEDURE ListSnap(self : T; pattern : TEXT; heedErrors := TRUE) : TEXT
  RAISES {E} =
  VAR 
    outRd   :  Rd.T;
    pid     :  Process.T;
    args    :  REF ARRAY OF TEXT;
    res     := "";
    wd      :  TEXT;
    errfn   := ErrlogFilename(self);
    closeRd := TRUE;
    ret     :  INTEGER;
    errmsg  :  TEXT := NIL;
    cacheOpName : TEXT;
  BEGIN
    IF self.dcvsRoot = NIL THEN RETURN res END;
    cacheOpName := "ListSnap";
    LOCK self.mu DO
    IF self.cache.contains(cacheOpName, pattern) THEN
      RETURN self.cache.getText(cacheOpName, pattern);
    END;
    args := NEW(REF ARRAY OF TEXT, 2);
    args^[0] := "lssnap";
    args^[1] := pattern;
    wd := ".";
    TRY
      CVS_Call(self, args^, pid, outRd, wd, errfn);
      IF outRd # NIL THEN
        res := Rd.GetText(outRd, LAST(CARDINAL));
      END;
    EXCEPT 
    | Rd.Failure     => 
      errmsg := "reader failure";
      MsgX.Error2(self.msgif, "ListSnap", errmsg);
    | OSError.E      => 
      errmsg := "execution of " & self.mcvs.denotation() & " failed";
      MsgX.Error2(self.msgif, "ListSnap", errmsg);
      closeRd := FALSE;
    | Thread.Alerted => 
      errmsg := "exception while reading log";
      MsgX.Error2(self.msgif, "ListSnap", errmsg);
    END;
    IF noAction THEN RETURN "empty log" END;
    TRY
      IF closeRd AND outRd # NIL THEN
        Rd.Close(outRd);
        ret := Process.Wait(pid);
        CvsUnlock();
      END;
      MsgX.D(self.msgif, self.cvst & " exited with " & Fmt.Int(ret));
      (* does not work reliably 
      IF ret # 0 THEN
        errmsg := self.cvst & " exited with " & Fmt.Int(ret);
      END;
      *)
    EXCEPT ELSE
      MsgX.Error2(self.msgif, "ListSnap", "exception in finalization");
    END;
    TRY
      IF heedErrors THEN
        IF errmsg # NIL THEN
          RAISE E(errmsg);
        END;
        IF NOT EvalCVSErrors(self.msgif, errfn, res) THEN
          RemoveErrlogFile(self, errfn);
          RAISE E(res);
        END;
      END;
      self.cache.putText(cacheOpName, pattern, res);
    FINALLY
      RemoveErrlogFile(self, errfn);
    END;
    RETURN res;
    END (* lock *)
  END ListSnap;

(*---------------------------------------------------------------------------*)
PROCEDURE SnapExists(self : T; pattern : TEXT; heedErrors := TRUE) : BOOLEAN
  RAISES {E} =
  VAR list: TEXT;
  BEGIN
    IF self.dcvsRoot = NIL THEN RETURN FALSE END;
    list := ListSnap(self, pattern, heedErrors);
    MsgX.D(self.msgif, "SnapExists(" & pattern & ") -> " & list, level := 3);
    RETURN Text.Length(list) > 0;
  END SnapExists;

(*---------------------------------------------------------------------------*)
PROCEDURE SnapList(self : T; pattern : TEXT; heedErrors := TRUE;
                   longList := FALSE) : TextSeq.T
  RAISES {E} =
  VAR 
    list: TEXT;
    elem: TEXT;
    res:  TextSeq.T;
    k: INTEGER;
  BEGIN
    IF self.dcvsRoot = NIL THEN RETURN NEW(TextSeq.T).init(0) END;
    list := ListSnap(self, pattern, heedErrors);
    res := TextUtils.Tokenize(list);
    FOR i := 0 TO res.size() -1 DO
      elem := res.get(i);
      IF NOT longList THEN
        k := Text.FindChar(elem, '/', 1);
        IF k < Text.Length(elem) THEN
          elem := Text.Sub(elem, k + 1);
          res.put(i, elem);
        END;
      END;
      MsgX.D(self.msgif, "found snap " & elem, level := 3);
    END;
    RETURN res;
  END SnapList;

(*---------------------------------------------------------------------------*)
PROCEDURE Status(self : T; fn : APN.T; VAR rev, stat:  TEXT) RAISES {E} =
  VAR 
    outRd   :  Rd.T;
    pid     :  Process.T;
    args    :  ARRAY [1..3] OF TEXT;
    line    := "";
    found   := FALSE;
    i, j    : CARDINAL := 0;
    wd      : APN.T;
    errfn   := ErrlogFilename(self);
    closeRd := TRUE;
    ret     :  INTEGER;
    res     :  TEXT := "";
  BEGIN
    LOCK self.mu DO
    args[1] := "-q";
    args[2] := "stat";
    args[3] := APN.Last(fn).denotation();
    wd := APN.Prefix(fn);
    TRY
      CVS_Call(self, args, pid, outRd, wd.denotation(), errfn);
      IF noAction THEN
        rev := "unknown"; stat := "unknown";
      ELSE
        WHILE NOT Rd.EOF(outRd) AND NOT found DO
          line := Rd.GetLine(outRd);
          i := 0; j := 0;
          IF Text.Equal("File: ", Text.Sub(line, 0, 6)) THEN
            i := 10;
            IF TextEx.FindSub(line, "Status: ", i) THEN
              stat := TextUtils.Compress(Text.Sub(line, i+8));
            END;
          ELSIF TextEx.FindSub(line, "Working revision:", j) THEN
            i := j+17;
            IF TextEx.FindCharSet(line, ASCII.Spaces, i) THEN
              rev := TextUtils.Compress(Text.Sub(line, j+17, i-(j+17)));
              found := TRUE;
            END;
	  END;
	END;
      END;
    EXCEPT
      Rd.EndOfFile => (* skip *)
    | Rd.Failure   => MsgX.Error2(self.msgif, "Status", "reader failure");
    | OSError.E    => MsgX.Error2(self.msgif, "Status", self.CVST &
                                  " call failed"); 
                      closeRd := FALSE;
    | Thread.Alerted => 
      MsgX.Error2(self.msgif, "Status", "exception while reading");
    END;
    IF noAction THEN RETURN END;
    TRY
      IF closeRd AND outRd # NIL THEN
        Rd.Close(outRd);
        ret := Process.Wait(pid);
        CvsUnlock();
      END;
      MsgX.D(self.msgif, self.cvst & " exited with " & Fmt.Int(ret));
    EXCEPT ELSE 
      MsgX.Error2(self.msgif, "Status", "exception while finalizing");
    END;
    IF NOT EvalCVSErrors(self.msgif, errfn, res) THEN
      RemoveErrlogFile(self, errfn);
      RAISE E(res);
    END;
    RemoveErrlogFile(self, errfn);
    IF NOT found THEN
      rev  := "???";
      stat := "???"
    END;
    END (* lock *)
  END Status;

(*---------------------------------------------------------------------------*)
PROCEDURE SetLabel(self : T; fn : APN.T; rev, label: TEXT) : BOOLEAN =
  VAR
    outRd   :  Rd.T;
    pid     :  Process.T;
    args    :  ARRAY [1..3] OF TEXT;
    ret     :  Process.ExitCode;
    wd      : APN.T;
    errfn   := ErrlogFilename(self);
  BEGIN
    LOCK self.mu DO
    label := TextUtils.SubstChar(label, ' ', '_');
    TRY
      args[1] := "admin";
      args[2] := "-s" & label & ":" & rev;
      args[3] := APN.Last(fn).denotation();
      wd := APN.Prefix(fn);
      CVS_Call(self, args, pid, outRd, wd.denotation(), errfn);
      IF NOT noAction THEN
        TRY EVAL Rd.GetText(outRd, LAST(CARDINAL)) EXCEPT ELSE END;
        TRY
          IF outRd # NIL THEN
            Rd.Close(outRd);
          END;
          ret := Process.Wait(pid);
          CvsUnlock();
        EXCEPT ELSE
          MsgX.Error2(self.msgif, "SetLabel", "exception while finalizing");
        END;
      END;
    EXCEPT
      OSError.E(a) => MsgX.Error2(self.msgif, "SetLabel", "OSError (skipped)" &
        " (" & Atom.ToText(a.head) & ")");
    END;
    IF noAction THEN RETURN TRUE END;
    RemoveErrlogFile(self, errfn);
    RETURN ret = 0; (* FIXME: Does cvs admin -s return a valid exit code? *)
    END (* lock *);
  END SetLabel;

(*---------------------------------------------------------------------------*)
PROCEDURE RevisionsAndLabels(self : T; rlog : TEXT) : TextTextTbl.T 
  RAISES {E} =
  VAR
    line  : TEXT;
    rev   : TEXT;
    state : TEXT;
    t     : TEXT;
    rd    : TextRd.T;
    i, j  : CARDINAL;
    res   := NEW(TextTextTbl.Default).init(100);
  BEGIN
    rd := TextRd.New(rlog);
    TRY
      WHILE NOT Rd.EOF(rd) DO
        line := Rd.GetLine(rd);
        IF Text.Equal("----", Text.Sub(line, 0, 4)) THEN
          line := Rd.GetLine(rd);
          i := 0;
          IF TextEx.FindSub(line, "revision ", i) AND (i = 0) THEN
            rev := TextUtils.Compress(Text.Sub(line, 9));
            line := Rd.GetLine(rd);
            i := 0;
            IF TextEx.FindSub(line, "date: ", i) AND (i = 0) THEN
              IF TextEx.FindSub(line, "state: ", i) THEN
                INC(i, 7);
                j := i;
                IF TextEx.FindChar(line, ';', i) THEN
                  state := TextUtils.Compress(Text.Sub(line, j, i - j));
                ELSE
                  state := TextUtils.Compress(Text.Sub(line, j, 
                                                       LAST(CARDINAL)));
                  TRY 
                    t := RdExtras.GetText(rd, terminate := ASCII.Set{';'});
                    state := state & t;
                  EXCEPT ELSE
                    MsgX.Error2(self.msgif, "CVS.RevisionsAndLabels",
                               "didn't find terminating ;");
                  END;
                END;
                EVAL res.put(rev, state);
              END;
            END;
          END;
        END;
      END;
    EXCEPT 
      Rd.EndOfFile => (* skip *)
    | Rd.Failure => 
      WITH msg = "reader failure" DO
        MsgX.Error2(self.msgif, "CVS.RevisionsAndLabels", msg);
        RAISE E(msg);
      END;
    | Thread.Alerted => 
      WITH msg = "interrupted" DO
        MsgX.Error2(self.msgif, "CVS.RevisionsAndLabels", msg);
        RAISE E(msg);
      END;
    END;
    RETURN res ;
  END RevisionsAndLabels;

(*---------------------------------------------------------------------------*)
PROCEDURE GetLabel(self : T; fn : APN.T; rev : TEXT) : TEXT 
  RAISES {E} =
  VAR 
    outRd   :  Rd.T;
    pid     :  Process.T;
    args    :  ARRAY [1..3] OF TEXT;
    revis   := "";
    t       :  TEXT;
    line    :  TEXT;
    state   :  TEXT := "???";
    found   := FALSE;
    i, j    :  CARDINAL;
    wd      :  APN.T;
    errfn   := ErrlogFilename(self);
  BEGIN
    LOCK self.mu DO
    TRY
      TRY
        args[1] := "log";
        args[2] := "-r" & rev;
        args[3] := APN.Last(fn).denotation();
        wd := APN.Prefix(fn);
        CVS_Call(self, args, pid, outRd, wd.denotation(), errfn);
        WHILE NOT noAction AND NOT Rd.EOF(outRd) AND NOT found DO
          TRY
            line := Rd.GetLine(outRd);
            IF Text.Equal("----", Text.Sub(line, 0, 4)) THEN
              line := Rd.GetLine(outRd);
              i := 0;
              IF TextEx.FindSub(line, "revision ", i) AND (i = 0) THEN
                revis := TextUtils.Compress(Text.Sub(line, 9));
                line := Rd.GetLine(outRd);
                i := 0;
                IF TextEx.FindSub(line, "date: ", i) AND (i = 0) THEN
                  IF TextEx.FindSub(line, "state: ", i) THEN
                    INC(i, 7);
                    j := i;
		    IF TextEx.FindChar(line, ';', i) THEN
		      state := TextUtils.Compress(Text.Sub(line, j, i - j));
		    ELSE
		      state := TextUtils.Compress(Text.Sub(line, j, 
							   LAST(CARDINAL)));
		      TRY 
			t := RdExtras.GetText(outRd,
                                              terminate := ASCII.Set{';'});
			state := state & t;
		      EXCEPT ELSE
                        WITH msg = "didn't find terminating ;" DO
                          MsgX.Error2(self.msgif, "CVS.GetLabel",
                                      msg);
                          RAISE E(msg);
                        END;
		      END;
		    END;
                  END;
                END;
              END;
            END;
          EXCEPT ELSE
          END;
        END;
      EXCEPT
        E(m) => RAISE E(m);
      ELSE
        RAISE E("couldn't get state label");
      END;
    FINALLY
      IF noAction THEN RETURN "unknown" END;
      TRY
        IF outRd # NIL THEN
          Rd.Close(outRd);
          EVAL Process.Wait(pid);
          CvsUnlock();
        END;
      EXCEPT ELSE END;
      RemoveErrlogFile(self, errfn);
    END;
    RETURN state;
    END (* lock *)
  END GetLabel;

(*---------------------------------------------------------------------------*)
PROCEDURE Tags(self : T; fn : APN.T; prefix : TEXT; 
               local := FALSE) : TextSeq.T RAISES {E} =
  VAR
    table   :  TextTextTbl.T;
    res     :  TextSeq.T;
    add     := FALSE;
    line    :  TEXT;
    i, j    :  CARDINAL;
    label   ,
    rev     :  TEXT;
    log     :  Rd.T;
    fnp     := fn.denotation();
    key     := "Tags_" & fn.denotation() & "_" & prefix;
    errmsg  :  TEXT := NIL;
  BEGIN
    MsgX.D(self.msgif, "CVS.Tags(" & fn.denotation() & ")");
    IF NOT FileObj.Exists(fn) THEN RETURN NIL END;
    LOCK self.mu DO
      IF self.cache.contains(key, fnp) THEN
        RETURN self.cache.getSeq(key, fnp);
      END;
    END;
    log := TextRd.New(LogFile(self, fn, heedErrors := FALSE,
                              headerOnly := TRUE, local := local));
    table := NEW(TextTextTbl.Default).init();
    res := NEW(TextSeq.T).init();
    IF noAction THEN RETURN res END; <* NOWARN *>
    TRY
      WHILE NOT Rd.EOF(log) DO
        line := Rd.GetLine(log);
        i := 0; j := 0; 
        IF TextEx.FindSub(line, "symbolic names:", i) AND (i = 0) THEN
          add := TRUE;
        ELSIF TextEx.FindSub(line, "keyword substitution:", j) AND 
          (j = 0) THEN
          add := FALSE;
        ELSIF add THEN
          i := 0;
          line := TextUtils.Compress(line);
          IF Text.Empty(prefix) OR
            TextEx.FindSub(line, prefix, i) AND (i = 0) THEN
            WITH pos = Text.FindChar(line, ':', 0) DO
              IF pos >= 0 THEN
                label := Text.Sub(line, 0, pos);
                rev   := Text.Sub(line, pos + 1);
                MsgX.D(self.msgif, "found tag " & label, level := 3);
                IF NOT table.get(label, rev) THEN
                  EVAL table.put(label, rev);
                  res.addhi(label);
                END;
              END;
            END;
          END;
        END;
      END;
    EXCEPT 
      Rd.EndOfFile => (* skip *)
    | Rd.Failure   => 
      errmsg := "reader failure";
      MsgX.Error2(self.msgif, "Tags", errmsg);
(* xxx
    | OSError.E    => 
      errmsg := "execution of " & self.mcvs.denotation() & " failed";
      MsgX.Error2(self.msgif, "Tags", errmsg);
      closeRd := FALSE;
*)
    | Thread.Alerted => 
      errmsg := "exception while reading log";
      MsgX.Error2(self.msgif, "Tags", errmsg);
    ELSE
      errmsg := "unexpected exception";
      MsgX.Error2(self.msgif, "Tags", errmsg);
    END;
    IF noAction THEN RETURN res END;
    IF errmsg # NIL THEN
      RAISE E(errmsg);
    END;
    LOCK self.mu DO
      self.cache.putSeq(key, fnp, res);
    END;
    RETURN res;
  END Tags;

(*---------------------------------------------------------------------------*)
PROCEDURE TagsAndSnaps(self : T; fn : APN.T; prefix : TEXT; 
                       local := FALSE; pkgName : TEXT := NIL) : TextSeq.T
  RAISES {E} =
  VAR
    pattern: TEXT;
    res: TextSeq.T;
  BEGIN
    pattern := prefix;
    IF prefix = NIL OR Text.Length(prefix) = 0 THEN
      pattern := "*";
    ELSIF Text.GetChar(pattern, Text.Length(pattern) -1) # '*' THEN
      pattern := pattern & "*";
    END;
    IF pkgName # NIL THEN
      pattern := pattern & "*_" & pkgName & "_*";
    END;
    res := SnapList(self, pattern);
    res := TextSeq.Cat(res, Tags(self, fn, prefix, local));
    RETURN res;
  END TagsAndSnaps;

(*---------------------------------------------------------------------------*)
PROCEDURE TaggedRevisions(self : T; fn : APN.T; 
                          local := FALSE) : TextTextTbl.T RAISES {E} =
  VAR
    log   : TextRd.T;
    table : TextTextTbl.T;
    nil   : TextTextSeqTbl.T := NIL;
  BEGIN
    MsgX.D(self.msgif, "CVS.TaggedRevisions(" & fn.denotation() & ")");
    IF NOT FileObj.Exists(fn) THEN RETURN NIL END;
    table := NEW(TextTextTbl.Default).init(50);
    log := TextRd.New(LogFile(self, fn, heedErrors := FALSE,
                              headerOnly := TRUE, local := local));
    ExtractTagsAndRevisions(self, log, table, nil);
    RETURN table;
  END TaggedRevisions;

(*---------------------------------------------------------------------------*)
PROCEDURE RevisionTags(self : T; fn : APN.T; 
                       local := FALSE) : TextTextSeqTbl.T RAISES {E} =
  VAR
    log   : TextRd.T;
    table : TextTextSeqTbl.T;
    nil   : TextTextTbl.T := NIL;
  BEGIN
    MsgX.D(self.msgif, "CVS.RevisionTags(" & fn.denotation() & ")");
    IF NOT FileObj.Exists(fn) THEN RETURN NIL END;
    table := NEW(TextTextSeqTbl.Default).init(50);
    log := TextRd.New(LogFile(self, fn, heedErrors := FALSE,
                              headerOnly := TRUE, local := local));
    ExtractTagsAndRevisions(self, log, nil, table);
    RETURN table;
  END RevisionTags;

(*---------------------------------------------------------------------------*)
PROCEDURE ExtractTagsAndRevisions(
    self      : T; 
    log       : TextRd.T;
    VAR trtab : TextTextTbl.T;
    VAR rttab : TextTextSeqTbl.T;
    ) RAISES {E} =
  VAR
    add   := FALSE;
    line  :  TEXT;
    i, j  :  CARDINAL;
    tag   ,
    rev   :  TEXT;
  BEGIN
    TRY
      WHILE NOT Rd.EOF(log) DO
        line := Rd.GetLine(log);
        i := 0; j := 0; 
        IF TextEx.FindSub(line, "symbolic names:", i) AND (i = 0) THEN
          add := TRUE;
        ELSIF TextEx.FindSub(line, "keyword substitution:", j) AND 
          (j = 0) THEN
          add := FALSE;
        ELSIF add THEN
          i := 0;
          line := TextUtils.Compress(line);
          WITH pos = Text.FindChar(line, ':', 0) DO
            IF pos >= 0 THEN
              tag := Text.Sub(line, 0, pos);
              rev := TextUtils.Compress(Text.Sub(line, pos + 1));
              MsgX.D(self.msgif, "found tag " & tag & " on revision " & 
                rev, level := 3);
              IF trtab # NIL THEN
                EVAL trtab.put(tag, rev);
              END;
              IF rttab # NIL THEN
                VAR tags : TextSeq.T; BEGIN
                  IF NOT rttab.get(rev, tags) THEN
                    tags := NEW(TextSeq.T).init();
                  END;
                  tags.addhi(tag);
                  EVAL rttab.put(rev, tags);
                END;
              END;
            END;
          END;
        END;
      END;
    EXCEPT
      Rd.EndOfFile => (* skip *)
    | Rd.Failure   => 
      WITH msg = "reader failure" DO
        MsgX.Error2(self.msgif, "ExtractTagsAndRevisions", msg);
        RAISE E(msg)
      END;
    ELSE
      WITH msg = "unexpected exception" DO
        MsgX.Error2(self.msgif, "ExtractTagsAndRevisions", msg);
        RAISE E(msg);
      END;
    END;
  END ExtractTagsAndRevisions;

(*---------------------------------------------------------------------------*)
PROCEDURE StateList(self : T; fn : APN.T) : TextSeq.T RAISES {E} =
  VAR
    outRd    :  Rd.T;
    pid      :  Process.T;
    args     :  ARRAY [1..3] OF TEXT;
    res      := NEW(TextSeq.T).init(50);
    restext  :  TEXT := "";
    ret      :  Process.ExitCode;
    line     :  TEXT;
    wd       :  APN.T;
    fnp      := fn.denotation();
    wait     := TRUE;
    errfn    := ErrlogFilename(self);
    closeRd  := TRUE;
    errmsg   :  TEXT := NIL;
  BEGIN
    MsgX.D(self.msgif, "CVS.StateList(" & fn.denotation() & ")");
    IF NOT FileObj.Exists(fn) THEN 
      MsgX.D(self.msgif, "file not found");
      RETURN res;
    END;
    LOCK self.mu DO
    IF self.cache.contains("StateList", fnp) THEN
      RETURN self.cache.getSeq("StateList", fnp);
    END;
    args[1] := "-nq";
    args[2] := "update";
    args[3] := APN.Last(fn).denotation();
    wd := APN.Prefix(fn);
    TRY
      CVS_Call(self, args, pid, outRd, wd.denotation(), errfn);
      WHILE outRd # NIL AND NOT Rd.EOF(outRd) DO
        line := Rd.GetLine(outRd);
        IF Text.Length(line) > 1 AND Text.GetChar(line, 1) = ' ' THEN
          res.addhi(line);
          MsgX.D(self.msgif, "  useful line " & line, level := 3);
        ELSE
          MsgX.D(self.msgif, "  garbage line " & line, level := 3);
        END;
      END;
      self.cache.putSeq("StateList", fnp, res);
    EXCEPT 
      Rd.EndOfFile   => self.cache.putSeq("StateList", fnp, res);
    | Rd.Failure(e)  => 
      errmsg := "reader failure: " & System.AtomListToText(e);
      MsgX.Error2(self.msgif, "StateList", errmsg);
    | OSError.E(e)   => 
      errmsg := "execution of " & self.mcvs.denotation() & " failed: " &
                    System.AtomListToText(e);
      MsgX.Error2(self.msgif, "StateList", errmsg);
      closeRd := FALSE;
    | Thread.Alerted => 
      errmsg := "exception while reading";
      MsgX.Error2(self.msgif, "StateList", errmsg);
    END;
    IF noAction THEN RETURN res END;
    TRY
      IF closeRd AND outRd # NIL THEN
        Rd.Close(outRd);
        IF wait THEN
          ret := Process.Wait(pid);
          CvsUnlock();
        END;
        MsgX.D(self.msgif, self.cvst & " exited with " & Fmt.Int(ret));
        (* BEWARE: cvs exits with 1 (256) in case of conflicts, but 0 in
           all other cases it seems. So we cannot rely on the exit code 
           at all.  *)
        (*
        IF ret # 0 THEN
          errmsg := self.cvst & " exited with " & Fmt.Int(ret);
        END;
        *)
      END;
    EXCEPT ELSE
      MsgX.Error2(self.msgif, "StateList", "exception while finalizing");
    END;
    restext := TextUtils.TextSeqToText(res, "\n");
    IF errmsg # NIL THEN
      RemoveErrlogFile(self, errfn);
      RAISE E(errmsg);
    END;
    IF NOT EvalCVSErrors(self.msgif, errfn, restext) THEN
      RemoveErrlogFile(self, errfn);
      RAISE E(restext);
    END;
    RemoveErrlogFile(self, errfn);
    RETURN res;
    END (* lock *);
  END StateList;

(*---------------------------------------------------------------------------*)
PROCEDURE GetCommitMessage(self : T; desc := ""; pkg := "") : TEXT =
  BEGIN (* GetCommitMessage *)
    RETURN PkgVCUtils.GetCommitMessage(self.editor, self.msgif, desc, pkg);
  END GetCommitMessage;

(*---------------------------------------------------------------------------*)
PROCEDURE Checkout(self : T; fn : APN.T; 
                   tag : TEXT := NIL; VAR res : TEXT) : BOOLEAN =
  VAR
    outRd   :  Rd.T;
    pid     :  Process.T;
    args    :  REF ARRAY OF TEXT;
    ret     :  Process.ExitCode;
    errfn   := ErrlogFilename(self);
    okay    := TRUE;
    closeRd := TRUE;
    isSnap  := FALSE;
  BEGIN
    res := "no error specification";
    IF tag = NIL THEN
      args := NEW(REF ARRAY OF TEXT, 2);
      args^[1] := fn.denotation();
    ELSIF Text.Equal(tag, "head") OR Text.Equal(tag, "-A") THEN
      args := NEW(REF ARRAY OF TEXT, 3);
      args^[1] := "-A";
      args^[2] := fn.denotation();
    ELSIF SnapExists(self, tag, FALSE) THEN <* NOWARN *>
      args := NEW(REF ARRAY OF TEXT, 4);
      args^[1] := "-S";
      args^[2] := tag;
      args^[3] := fn.denotation();
      isSnap := TRUE;
    ELSE
      args := NEW(REF ARRAY OF TEXT, 4);
      args^[1] := "-r";
      args^[2] := tag;
      args^[3] := fn.denotation();
    END;
    args^[0] := "checkout";
    LOCK self.mu DO
    TRY
      CVS_Call(self, args^, pid, outRd, ".", errfn);
      IF outRd # NIL THEN
        res := Rd.GetText(outRd, LAST(CARDINAL));
      END;
      IF isSnap THEN
        SaveStickySnapFile(fn, tag);
      ELSE
        RemoveStickySnapFile(fn);
      END;
    EXCEPT
      Rd.EndOfFile => (* skip *)
    | OSError.E    =>
      MsgX.Error2(self.msgif, "Checkout", "execution of " &
        self.mcvs.denotation() & " failed");
      closeRd := FALSE;
      okay := FALSE;
    ELSE 
      MsgX.Error2(self.msgif, "Checkout", "exception while reading");
      okay := FALSE;
    END;
    IF noAction THEN RETURN TRUE END;
    TRY
      IF closeRd THEN
        ret := Process.Wait(pid);
        CvsUnlock();
        IF outRd # NIL THEN
          Rd.Close(outRd);
        END;
      END;
      MsgX.D(self.msgif, self.cvst & " exited with " & Fmt.Int(ret));
    EXCEPT 
      Rd.EndOfFile => (* skip *)
    ELSE 
      MsgX.Error2(self.msgif, "Checkout", "exception while finalizing");
      IF ret = 0 THEN
	ret := 1;
      END;
    END;
    okay := okay AND EvalCVSErrors(self.msgif, errfn, res, terminate := FALSE);
    RemoveErrlogFile(self, errfn);
    RETURN okay AND (ret = 0);
    END;
  END Checkout;

(*---------------------------------------------------------------------------*)
PROCEDURE Commit(self : T; fn : APN.T; 
                 msg : TEXT := NIL; msgFile : TEXT := NIL;
                 force := FALSE; desc : TEXT := ""; pkg := "";
                 changeSetName : TEXT := NIL) : BOOLEAN 
  RAISES {E} =
  VAR
    outRd   :  Rd.T;
    pid     :  Process.T;
    args    :  REF ARRAY OF TEXT;
    ret     :  Process.ExitCode;
    wd      :  Pathname.T;
    rout    :  TEXT;
    errfn   := ErrlogFilename(self);
    okay    := TRUE;
    nargs   :  CARDINAL;
    noff    :  CARDINAL;
    closeRd := TRUE;
  BEGIN
    IF NOT FileObj.Exists(fn) THEN RETURN FALSE END;
    IF force THEN 
      nargs := 5;
      noff  := 1;
    ELSE
      nargs := 4;
      noff  := 0;
    END;
    IF changeSetName # NIL AND self.dcvsRoot # NIL THEN
      INC(nargs, 2);
    END;
    args := NEW(REF ARRAY OF TEXT, nargs);
    args^[0] := "commit";
    IF force THEN 
      args^[1] := "-f";
    END;
    IF changeSetName # NIL AND self.dcvsRoot # NIL THEN
      args^[1 + noff] := "-g";
      args^[2 + noff] := changeSetName;
      INC(noff, 2);
    END;
    LOCK self.mu DO
    IF msg # NIL OR msgFile # NIL THEN
      args^[3 + noff] := APN.Last(fn).denotation();
      IF msg # NIL THEN
        args^[1 + noff] := "-m";
        args^[2 + noff] := OSSpecials.QuotedProcessArgument(msg);
      ELSE
        args^[1 + noff] := "-F";
        args^[2 + noff] := msgFile;
      END;
    ELSE
      msg := GetCommitMessage(self, desc, pkg);
      IF msg = NIL THEN
        (* We won't continue without a valid commit message for our logs *)
        MsgX.Error2(self.msgif, "Commit", "couldn't get commit message");
        RAISE E("editor session failed");
      END;
      args^[1 + noff] := "-m";
      args^[2 + noff] := OSSpecials.QuotedProcessArgument(msg);
      args^[3 + noff] := APN.Last(fn).denotation();
    END;
    wd := APN.Prefix(fn).denotation();
    IF Text.Empty(wd) THEN
      wd := NIL;
    END;
    TRY
      CVS_Call(self, args^, pid, outRd, wd, errfn);
      IF outRd # NIL THEN
        rout := Rd.GetText(outRd, LAST(CARDINAL));
      END;
    EXCEPT 
      Rd.Failure   =>
      MsgX.Error2(self.msgif, "Commit", "reader failure while reading");
      okay := FALSE;
    | OSError.E    => MsgX.Error2(self.msgif, "Commit", "execution of " &
                                 self.mcvs.denotation() & " failed");
                      closeRd := FALSE;
                      okay := FALSE;
    | Thread.Alerted =>
      MsgX.Error2(self.msgif, "Commit", "exception while reading");
      okay := FALSE;
    END;
    IF noAction THEN RETURN TRUE END;
    TRY
      IF closeRd THEN
        ret := Process.Wait(pid);
        CvsUnlock();
        IF outRd # NIL THEN
          Rd.Close(outRd);
        END;
      END;
      MsgX.D(self.msgif, self.cvst & " exited with " & Fmt.Int(ret));
    EXCEPT 
      Rd.EndOfFile => (* skip *)
    ELSE 
      MsgX.Error2(self.msgif, "Commit", "exception while finalizing");
      IF ret = 0 THEN
	ret := 1;
      END;
    END;
    IF NOT EvalCVSErrors(self.msgif, errfn, rout, terminate := FALSE) THEN
      RAISE E(rout);
    END;
    RemoveErrlogFile(self, errfn);
    RETURN okay AND (ret = 0);
    END (* lock *);
  END Commit;

(*---------------------------------------------------------------------------*)
PROCEDURE Update(self : T; fn : APN.T; rev : TEXT; VAR res : TEXT;
                 createDirs := TRUE; pruneDirs := TRUE) : BOOLEAN =
  VAR
    outRd   :  Rd.T;
    pid     :  Process.T;
    args    :  REF ARRAY OF TEXT;
    ret     :  Process.ExitCode;
    wd      :  Pathname.T;
    errfn   := ErrlogFilename(self);
    okay    := TRUE;
    closeRd := TRUE;
    isSnap  := FALSE;
  BEGIN
    IF NOT FileObj.Exists(fn) THEN 
      res := fn.denotation() & " does not exist";
      RETURN FALSE;
    END;
    res := "no error specification";
    IF Text.Empty(rev) THEN
      args := NEW(REF ARRAY OF TEXT, 3);
      args^[2] := APN.Last(fn).denotation();
    ELSIF Text.Equal(rev, "head") OR 
          Text.Equal(rev, "top") OR 
          Text.Equal(rev, "-A") THEN
      args := NEW(REF ARRAY OF TEXT, 4);
      args^[2] := "-A";
      args^[3] := APN.Last(fn).denotation();
    ELSIF Text.Equal(rev, "headofbranch") OR 
          Text.Equal(rev, "tip") THEN 
      args := NEW(REF ARRAY OF TEXT, 3);
      (* works only if checked out on a branch *)
      args^[2] := APN.Last(fn).denotation();
    ELSIF SnapExists(self, rev, FALSE) THEN <* NOWARN *>
      isSnap := TRUE;
      args := NEW(REF ARRAY OF TEXT, 5);
      args^[2] := "-S";
      args^[3] := rev;
      args^[4] := APN.Last(fn).denotation();
    ELSE
      args := NEW(REF ARRAY OF TEXT, 5);
      args^[2] := "-r";
      args^[3] := rev;
      args^[4] := APN.Last(fn).denotation();
    END;
    args^[0] := "update";
    IF createDirs AND pruneDirs THEN
      args^[1] := "-dP";
    ELSIF createDirs THEN
      args^[1] := "-d";
    ELSIF pruneDirs THEN
      args^[1] := "-P";
    ELSE
      args^[1] := "-R"; (* dummy, this is the default anyway *)
    END;
    wd := APN.Prefix(fn).denotation();
    IF Text.Empty(wd) THEN
      wd := NIL;
    END;
    LOCK self.mu DO
    TRY
      CVS_Call(self, args^, pid, outRd, wd, errfn);
      IF outRd # NIL THEN
        res := Rd.GetText(outRd, LAST(CARDINAL));
      END;
      IF isSnap THEN
        SaveStickySnapFile(fn, rev);
      ELSE
        RemoveStickySnapFile(fn);
      END;
    EXCEPT
    | Rd.Failure => 
      res := "Update: reader failure";
      okay := FALSE;
    | OSError.E =>
      res := "Update: execution of " & self.mcvs.denotation() & " failed";
      closeRd := FALSE;
      okay := FALSE;
    | E(m) =>
      res := "Update: snaphot file error: " & m;
      okay := FALSE;
    | Thread.Alerted =>
      MsgX.Error2(self.msgif, "Update", "interrupted while reading");
      okay := FALSE;
    END;
    IF noAction THEN RETURN TRUE END;
    TRY
      IF closeRd THEN
        IF outRd # NIL THEN
          Rd.Close(outRd);
        END;
        ret := Process.Wait(pid);
        CvsUnlock();
      END;
      MsgX.D(self.msgif, self.cvst & " exited with " & Fmt.Int(ret));
    EXCEPT 
      Rd.EndOfFile => (* skip *)
    ELSE 
      MsgX.Error2(self.msgif, "Update", "exception while finalizing");
      IF ret = 0 THEN
	ret := 1;
      END;
    END;
    okay := okay AND EvalCVSErrors(self.msgif, errfn, res, terminate := FALSE);
    RemoveErrlogFile(self, errfn);
    RETURN okay AND (ret = 0);
    END (* lock *)
  END Update;

(*---------------------------------------------------------------------------*)
PROCEDURE Merge(self : T; fn : APN.T; rev : TEXT; rev2 : TEXT := NIL;
		with_d_option : BOOLEAN := FALSE;
                VAR res : TEXT) : BOOLEAN =
  VAR
    outRd   :  Rd.T;
    pid     :  Process.T;
    args    :  REF ARRAY OF TEXT;
    ret     :  Process.ExitCode;
    wd      :  Pathname.T;
    errfn   := ErrlogFilename(self);
    okay    :  BOOLEAN := TRUE;
    closeRd := TRUE;
    nargs   : INTEGER;
  BEGIN
    IF NOT FileObj.Exists(fn) THEN 
      res := "Cannot find " & fn.denotation();
      RETURN FALSE;
    END;
    res := "no error specification";
    nargs := 1;
    IF with_d_option = TRUE THEN
      nargs := nargs + 1;
    END;
    IF Text.Empty(rev) THEN
      res := "Empty source revision for merge.";
      RETURN FALSE;
    ELSIF Text.Equal(rev, "head") OR Text.Equal(rev, "-A") THEN
      res := "Cannot use `head' as merge source, sorry.";
      RETURN FALSE;
    ELSIF rev2 = NIL THEN
      args := NEW(REF ARRAY OF TEXT, nargs + 3);
      IF SnapExists(self, rev, FALSE) THEN <* NOWARN *>
        args^[nargs] := "-J";
      ELSE
        args^[nargs] := "-j";
      END;
      args^[nargs+1] := rev;
      args^[nargs+2] := APN.Last(fn).denotation();
    ELSIF Text.Equal(rev2, "head") OR Text.Equal(rev2, "-A") THEN
      RETURN FALSE;
    ELSE (* rev2 # NIL and not `head' *)
      args := NEW(REF ARRAY OF TEXT, nargs + 5);
      IF SnapExists(self, rev, FALSE) THEN <* NOWARN *>
        args^[nargs] := "-J";
      ELSE
        args^[nargs] := "-j";
      END;
      args^[nargs+1] := rev;
      IF SnapExists(self, rev2, FALSE) THEN <* NOWARN *>
        args^[nargs+2] := "-J";
      ELSE
        args^[nargs+2] := "-j";
      END;
      args^[nargs+3] := rev2;
      args^[nargs+4] := APN.Last(fn).denotation();
    END;
    args^[0] := "update";
    IF with_d_option = TRUE THEN
      args^[1] := "-d";
    END;
    wd := APN.Prefix(fn).denotation();
    IF Text.Empty(wd) THEN
      wd := NIL;
    END;
    LOCK self.mu DO
    TRY
      CVS_Call(self, args^, pid, outRd, wd, errfn);
      IF outRd # NIL THEN
        res := Rd.GetText(outRd, LAST(CARDINAL));
      END;
    EXCEPT
      Rd.EndOfFile => (* skip *)
    | Rd.Failure   => res := "Merge: reader failure"; 
                      okay := FALSE;
    | OSError.E    => res := "Merge: execution of " &
                                 self.mcvs.denotation() & " failed";
                      closeRd := FALSE;
                      okay := FALSE;
    ELSE
      MsgX.Error2(self.msgif, "Merge", "exception while reading");
      okay := FALSE;
    END;
    IF noAction THEN RETURN TRUE END;
    TRY
      IF closeRd THEN
        IF outRd # NIL THEN
          Rd.Close(outRd);
        END;
        ret := Process.Wait(pid);
        CvsUnlock();
      END;
      MsgX.D(self.msgif, "self.msgif, cvs exited with " & Fmt.Int(ret));
    EXCEPT 
      Rd.EndOfFile => (* skip *)
    ELSE 
      MsgX.Error2(self.msgif, "Merge", "exception while finalizing");
      IF ret = 0 THEN
	ret := 1;
      END;
    END;
    okay := okay AND EvalCVSErrors(self.msgif, errfn, res, terminate := FALSE);
    RemoveErrlogFile(self, errfn);
    RETURN okay AND (ret = 0);
    END (* lock *);
  END Merge; 

(*---------------------------------------------------------------------------*)
PROCEDURE Diff(self : T; fn : APN.T; from : TEXT := NIL; to : TEXT := NIL;
               udiff := FALSE; cdiff := FALSE; 
               flist : APNSeq.T := NIL; VAR res : TEXT) : BOOLEAN =
  VAR
    outRd   :  Rd.T;
    pid     :  Process.T;
    args    :  REF ARRAY OF TEXT;
    argc    := 3;
    next    :  INTEGER;
    ret     :  Process.ExitCode;
    wd      :  Pathname.T;
    errfn   := ErrlogFilename(self);
    okay    := TRUE;
    closeRd := TRUE;
  BEGIN
    IF NOT FileObj.Exists(fn) THEN
      res := fn.denotation() & " does not exist";
      RETURN FALSE;
    END;
    res := "no error specification";
    IF flist # NIL THEN INC(argc, flist.size() -1) END;
    IF from # NIL THEN INC(argc, 2) END;
    IF to   # NIL THEN INC(argc, 2) END;
    IF udiff OR cdiff THEN INC(argc, 1) END;
    args := NEW(REF ARRAY OF TEXT, argc);
    args^[0] := "diff";
    args^[1] := "-N";
    next := 2;
    IF udiff THEN 
      args^[2] := "-u"; 
      next := 3;
    ELSIF cdiff THEN
      args^[2] := "-c"; 
      next := 3;
    END;
    IF from # NIL THEN
      IF SnapExists(self, from, FALSE) THEN <* NOWARN *>
        args^[next] := "-S";
      ELSE
        args^[next] := "-r";
      END;
      INC(next);
      IF Text.Equal(from, "head") THEN
        from := "HEAD";
        (* This doesn't seem to work with current CVS versions. *)
      END;
      args^[next] := from; 
      INC(next);
    END;
    IF to # NIL THEN
      IF SnapExists(self, to, FALSE) THEN <* NOWARN *>
        args^[next] := "-S";
      ELSE
        args^[next] := "-r";
      END;
      INC(next);
      IF Text.Equal(to, "head") THEN
        to := "HEAD";
        (* This doesn't seem to work with current CVS versions. *)
      END;
      args^[next] := to; 
      INC(next);
    END;
    IF flist = NIL THEN
      wd := APN.Prefix(fn).denotation();
      args^[next] := APN.Last(fn).denotation();
    ELSE
      wd := fn.denotation();
      FOR i := 0 TO flist.size() - 1 DO
        WITH pn = flist.get(i) DO
          args^[next] := pn.denotation();
          INC(next);
        END;
      END;
    END;
    IF Text.Empty(wd) THEN
      wd := NIL;
    END;
    LOCK self.mu DO
    TRY
      CVS_Call(self, args^, pid, outRd, wd, errfn);
      IF outRd # NIL THEN
        res := Rd.GetText(outRd, LAST(CARDINAL));
      END;
    EXCEPT
      Rd.EndOfFile => (* skip *)
    | Rd.Failure   => res := "Diff: reader failure";
                      okay := FALSE;
    | OSError.E    => res := "Diff: execution of " &
                                 self.mcvs.denotation() & " failed";
                      closeRd := FALSE;
                      okay := FALSE;
    ELSE
      MsgX.Error2(self.msgif, "Diff", "exception while reading");
      okay := FALSE;
    END;
    IF noAction THEN RETURN TRUE END;
    TRY
      IF closeRd THEN
        IF outRd # NIL THEN
          Rd.Close(outRd);
        END;
        ret := Process.Wait(pid);
        CvsUnlock();
      END;
      MsgX.D(self.msgif, self.cvst & " exited with " & Fmt.Int(ret));
    EXCEPT 
      Rd.EndOfFile => (* skip *)
    ELSE 
      MsgX.Error2(self.msgif, "Diff", "exception while finalizing");
    END;
    okay := okay AND EvalCVSErrors(self.msgif, errfn, res, terminate := FALSE);
    RemoveErrlogFile(self, errfn);
    RETURN okay;
    END (* lock *);
  END Diff;

(*---------------------------------------------------------------------------*)
PROCEDURE Annotate(self : T; fn : APN.T; flist : APNSeq.T := NIL) : TEXT
  RAISES {E} =
  VAR
    outRd   :  Rd.T;
    pid     :  Process.T;
    args    :  REF ARRAY OF TEXT;
    argc    := 2;
    next    :  INTEGER;
    ret     :  Process.ExitCode;
    wd      :  Pathname.T;
    errfn   := ErrlogFilename(self);
    res     := "no error specification";
    okay    := TRUE;
    closeRd := TRUE;
  BEGIN
    IF NOT FileObj.Exists(fn) THEN
      RAISE E(fn.denotation() & " does not exist");
    END;
    IF flist # NIL THEN INC(argc, flist.size() -1) END;
    args := NEW(REF ARRAY OF TEXT, argc);
    args^[0] := "annotate";
    next := 1;
    IF flist = NIL THEN
      wd := APN.Prefix(fn).denotation();
      args^[next] := APN.Last(fn).denotation();
    ELSE
      wd := fn.denotation();
      FOR i := 0 TO flist.size() - 1 DO
        WITH pn = flist.get(i) DO
          args^[next] := pn.denotation();
          INC(next);
        END;
      END;
    END;
    IF Text.Empty(wd) THEN
      wd := NIL;
    END;
    LOCK self.mu DO
      TRY
        CVS_Call(self, args^, pid, outRd, wd, errfn);
        IF outRd # NIL THEN
          res := Rd.GetText(outRd, LAST(CARDINAL));
        END;
      EXCEPT
      | Rd.Failure   => res := "Annotate: reader failure";
                        okay := FALSE;
      | OSError.E    => res := "Annotate: execution of " &
                                   self.mcvs.denotation() & " failed";
                        closeRd := FALSE;
                        okay := FALSE;
      | Thread.Alerted =>
        res := "exception while reading";
        MsgX.Error2(self.msgif, "Annotate", res);
        okay := FALSE;
      END;
      IF noAction THEN RETURN "no action, no result" END;
      TRY
        IF closeRd THEN
          IF outRd # NIL THEN
            Rd.Close(outRd);
          END;
          ret := Process.Wait(pid);
          CvsUnlock();
        END;
        MsgX.D(self.msgif, self.cvst & " exited with " & Fmt.Int(ret));
      EXCEPT 
        Rd.EndOfFile => (* skip *)
      ELSE 
        MsgX.Error2(self.msgif, "Annotate", "exception while finalizing");
      END;
      TRY
        IF NOT okay THEN
          RAISE E(res);
        END;
        IF NOT EvalCVSErrors(self.msgif, errfn, res, terminate := FALSE) THEN
          RAISE E(res);
        END;
      FINALLY
        RemoveErrlogFile(self, errfn);
      END;
      RETURN res;
    END (* lock *);
  END Annotate;

(*---------------------------------------------------------------------------*)
PROCEDURE Tag(self : T; fn : APN.T; tag : TEXT; branch := FALSE; 
              force := FALSE) : BOOLEAN =
  VAR
    outRd   :  Rd.T;
    pid     :  Process.T;
    args    :  REF ARRAY OF TEXT;
    ret     :  Process.ExitCode;
    wd      :  Pathname.T;
    errfn   := ErrlogFilename(self);
    okay    := TRUE;
    closeRd := TRUE;
    res     := "";
  BEGIN
    IF NOT FileObj.Exists(fn) THEN RETURN FALSE END;
    IF branch AND force THEN
      args := NEW(REF ARRAY OF TEXT, 5);
      args^[1] := "-F";
      args^[2] := "-b";
      args^[3] := tag;
      args^[4] := APN.Last(fn).denotation();
    ELSIF branch THEN
      args := NEW(REF ARRAY OF TEXT, 4);
      args^[1] := "-b";
      args^[2] := tag;
      args^[3] := APN.Last(fn).denotation();
    ELSIF force THEN
      args := NEW(REF ARRAY OF TEXT, 4);
      args^[1] := "-F";
      args^[2] := tag;
      args^[3] := APN.Last(fn).denotation();
    ELSE (* NOT branch AND NOT force *)
      args := NEW(REF ARRAY OF TEXT, 3);
      args^[1] := tag;
      args^[2] := APN.Last(fn).denotation();
    END;
    args^[0] := "tag";
    wd := APN.Prefix(fn).denotation();
    IF Text.Empty(wd) THEN
      wd := NIL;
    END;
    LOCK self.mu DO
    TRY
      CVS_Call(self, args^, pid, outRd, wd, errfn);
      IF outRd # NIL THEN
        EVAL Rd.GetText(outRd, LAST(CARDINAL)); 
      END;
    EXCEPT
    | OSError.E    => MsgX.Error2(self.msgif, "Tag", "execution of " &
                                  self.mcvs.denotation() & " failed");
                      closeRd := FALSE;
                      okay := FALSE;
    | Rd.Failure   => MsgX.Error2(self.msgif, "Tag", "reader failure");
                      okay := FALSE;
    | Thread.Alerted =>
      MsgX.Error2(self.msgif, "Tag", "interrupted while reading");
      okay := FALSE;
    END;
    IF noAction THEN
      MsgX.D(self.msgif, "early return");
      RETURN TRUE; <* NOWARN *>
    END;
    TRY
      IF closeRd THEN
        IF outRd # NIL THEN
          Rd.Close(outRd);
        END;
        ret := Process.Wait(pid);
        CvsUnlock();
      END;
      MsgX.D(self.msgif, self.cvst & " exited with " & Fmt.Int(ret));
    EXCEPT ELSE
      MsgX.Error2(self.msgif, "Tag", "exception while finalizing");
    END;
    IF NOT EvalCVSErrors(self.msgif, errfn, res, terminate := FALSE) THEN
      MsgX.Error(self.msgif, res);
      okay := FALSE;
    END;
    RemoveErrlogFile(self, errfn);
    RETURN okay AND (ret = 0);
    END (* lock *);
  END Tag; 

(*---------------------------------------------------------------------------*)
PROCEDURE TagAgain(self : T; fn : APN.T; oldtag, newtag : TEXT; 
                   branch := FALSE; force := FALSE) : BOOLEAN =
  VAR
    outRd   :  Rd.T;
    pid     :  Process.T;
    args    :  REF ARRAY OF TEXT;
    ret     :  Process.ExitCode;
    wd      :  Pathname.T;
    errfn   := ErrlogFilename(self);
    okay    := TRUE;
    closeRd := TRUE;
    res     := "";
  BEGIN
    IF NOT FileObj.Exists(fn) THEN RETURN FALSE END;
    IF branch AND force THEN
      args := NEW(REF ARRAY OF TEXT, 7);
      args^[1] := "-F";
      args^[2] := "-b";
      args^[3] := "-r";
      args^[4] := oldtag;
      args^[5] := newtag;
      args^[6] := APN.Last(fn).denotation();
    ELSIF branch THEN
      args := NEW(REF ARRAY OF TEXT, 6);
      args^[1] := "-b";
      args^[2] := "-r";
      args^[3] := oldtag;
      args^[4] := newtag;
      args^[5] := APN.Last(fn).denotation();
    ELSIF force THEN
      args := NEW(REF ARRAY OF TEXT, 6);
      args^[1] := "-F";
      args^[2] := "-r";
      args^[3] := oldtag;
      args^[4] := newtag;
      args^[5] := APN.Last(fn).denotation();
    ELSE (* NOT branch AND NOT force *)
      args := NEW(REF ARRAY OF TEXT, 5);
      args^[1] := "-r";
      args^[2] := oldtag;
      args^[3] := newtag;
      args^[4] := APN.Last(fn).denotation();
    END;
    args^[0] := "tag";
    wd := APN.Prefix(fn).denotation();
    IF Text.Empty(wd) THEN
      wd := NIL;
    END;
    LOCK self.mu DO
    TRY
      CVS_Call(self, args^, pid, outRd, wd, errfn);
      IF outRd # NIL THEN
        EVAL Rd.GetText(outRd, LAST(CARDINAL));
      END;
    EXCEPT
      Rd.EndOfFile => (* skip *)
    | Rd.Failure   => MsgX.Error2(self.msgif, "TagAgain", "reader failure");
                      okay := FALSE;
    | OSError.E    => MsgX.Error2(self.msgif, "TagAgain", "execution of " &
                                 self.mcvs.denotation() & " failed");
                      closeRd := FALSE;
                      okay := FALSE;
    ELSE
      MsgX.Error2(self.msgif, "TagAgain", "exception while reading");
      okay := FALSE;
    END;
    IF noAction THEN RETURN TRUE END;
    TRY
      IF closeRd THEN
        IF outRd # NIL THEN
          Rd.Close(outRd);
        END;
        ret := Process.Wait(pid);
        CvsUnlock();
      END;
      MsgX.D(self.msgif, self.cvst & " exited with " & Fmt.Int(ret));
    EXCEPT ELSE
      MsgX.Error2(self.msgif, "TagAgain", "exception while finalizing");
    END;
    IF NOT EvalCVSErrors(self.msgif, errfn, res, terminate := FALSE) THEN
      MsgX.Error(self.msgif, res);
      okay := FALSE;
    END;
    RemoveErrlogFile(self, errfn);
    RETURN okay AND (ret = 0);
    END (* lock *);
  END TagAgain; 

(*---------------------------------------------------------------------------*)
PROCEDURE Snapshot(self : T; fn : APN.T; tag : TEXT) : BOOLEAN =
  VAR
    outRd   :  Rd.T;
    pid     :  Process.T;
    args    :  REF ARRAY OF TEXT;
    ret     :  Process.ExitCode;
    wd      :  Pathname.T;
    errfn   := ErrlogFilename(self);
    okay    := TRUE;
    closeRd := TRUE;
    res     := "";
  BEGIN
    IF NOT FileObj.Exists(fn) THEN RETURN FALSE END;
    args := NEW(REF ARRAY OF TEXT, 4);
    args^[0] := "mksnap";
    args^[1] := "-f";
    args^[2] := tag;
    args^[3] := APN.Last(fn).denotation();
    wd := APN.Prefix(fn).denotation();
    IF Text.Empty(wd) THEN
      wd := NIL;
    END;
    LOCK self.mu DO
    TRY
      CVS_Call(self, args^, pid, outRd, wd, errfn);
      IF outRd # NIL THEN
        EVAL Rd.GetText(outRd, LAST(CARDINAL)); 
      END;
    EXCEPT
    | OSError.E    => MsgX.Error2(self.msgif, "Snapshot", "execution of " &
                                  self.mcvs.denotation() & " failed");
                      closeRd := FALSE;
                      okay := FALSE;
    | Rd.Failure   => MsgX.Error2(self.msgif, "Snapshot", "reader failure");
                      okay := FALSE;
    | Thread.Alerted =>
      MsgX.Error2(self.msgif, "Snapshot", "interrupted while reading");
      okay := FALSE;
    END;
    IF noAction THEN
      MsgX.D(self.msgif, "early return");
      RETURN TRUE; <* NOWARN *>
    END;
    TRY
      IF closeRd THEN
        IF outRd # NIL THEN
          Rd.Close(outRd);
        END;
        ret := Process.Wait(pid);
        CvsUnlock();
      END;
      MsgX.D(self.msgif, self.cvst & " exited with " & Fmt.Int(ret));
    EXCEPT ELSE
      MsgX.Error2(self.msgif, "Snapshot", "exception while finalizing");
    END;
    IF NOT EvalCVSErrors(self.msgif, errfn, res, terminate := FALSE) THEN
      MsgX.Error(self.msgif, res);
      okay := FALSE;
    END;
    RemoveErrlogFile(self, errfn);
    RETURN okay AND (ret = 0);
    END (* lock *);
  END Snapshot; 

(*---------------------------------------------------------------------------*)
PROCEDURE StateCharToText(c : CHAR) : TEXT =
  BEGIN
    CASE c OF
      'M' => RETURN "locally modified"
    | 'C' => RETURN "conflicts with repository"
    | 'U' => RETURN "needs update"
    | 'A' => RETURN "locally added"
    | 'R' => RETURN "locally removed"
    | '?' => RETURN "unknown to CVS"
    ELSE
      RETURN "unknown file state"
    END;
  END StateCharToText;

(*---------------------------------------------------------------------------*)
PROCEDURE ChangeDesc(self : T; fn : APN.T) : TEXT RAISES {E} =
  VAR
    list : TextSeq.T;
    resA : TEXT := "";
    resR : TEXT := "";
    resM : TEXT := "";
    resU : TEXT := "";
    resC : TEXT := "";
    resX : TEXT := "";
    res  : TEXT := "";

  (*-------------------------------------------------------------------------*)
  PROCEDURE Append(VAR res : TEXT; fn : TEXT; prefix := "") =
    BEGIN
      IF Text.Empty(res) THEN
        res := prefix & " " & fn & OSSpecials.LineBreak;
      ELSE
        res := res & prefix & " " & fn & OSSpecials.LineBreak;
      END;
    END Append;

  BEGIN (* ChangeDesc *)
    IF NOT FileObj.Exists(fn) THEN RETURN "" END;
    list := StateList(self, fn);
    FOR i := 0 TO list.size() - 1 DO
      WITH line = list.get(i) DO
        VAR
          state := Text.GetChar(line, 0);
          name  := Text.Sub(line, 2); 
        BEGIN
          IF    state = 'A' THEN
            Append(resA, name, "added:");
          ELSIF state = 'R' THEN
            Append(resR, name, "removed:");
          ELSIF state = 'M' THEN
            Append(resM, name, "modified:");
          ELSIF state = 'U' THEN
            Append(resU, name, "out-of-date:");
          ELSIF state = 'C' THEN
            Append(resC, name, "conflicts:");
          ELSIF state = '?' THEN
            Append(resX, name, "PKG: unknown:");
          END;
        END;
      END;
    END;
    IF NOT Text.Empty(resA) THEN
      res := res & resA;
    END;
    IF NOT Text.Empty(resR) THEN
      res := res & resR;
    END;
    IF NOT Text.Empty(resM) THEN
      res := res & resM;
    END;
    IF NOT Text.Empty(resU) THEN
      res := res & resU;
    END;
    IF NOT Text.Empty(resC) THEN
      res := res & resC;
    END;
    IF NOT Text.Empty(resX) THEN
      res := res & OSSpecials.LineBreak & resX;
    END;
    RETURN res;
  END ChangeDesc;

(*---------------------------------------------------------------------------*)
PROCEDURE Modified(self : T; fn : APN.T; VAR res : TEXT) : BOOLEAN RAISES {E} =
  VAR
    list : TextSeq.T;
    modified := FALSE;
    prefix : TEXT := NIL;
  BEGIN
    res  := "";
    IF NOT FileObj.Exists(fn) THEN RETURN FALSE END;
    IF FileObj.IsDir(fn) THEN prefix := APN.Last(fn).denotation() END;
    list := StateList(self, fn);
    FOR i := 0 TO list.size() - 1 DO
      WITH line = list.get(i) DO
        VAR
          state := Text.GetChar(line, 0);
          name  := Text.Sub(line, 2); 
          k     :  CARDINAL;
        BEGIN
          IF prefix # NIL AND TextEx.FindSub(name, prefix, k)
             AND k = 0 THEN
            name := Text.Sub(name, Text.Length(prefix) + 1, LAST(CARDINAL));
          END;
          IF state = 'M' OR state = 'C' OR
             state = 'A' OR state = 'R' THEN
            modified := TRUE;
          END;
          res := res & Fmt.Pad(name & " ", 51, '.', Fmt.Align.Left) & 
                     " " & StateCharToText(state) & "\n";
        END;
      END;
    END;
    RETURN modified;
  END Modified; 

(*---------------------------------------------------------------------------*)
PROCEDURE UpToDate(self : T; fn : APN.T; VAR res : TEXT) : BOOLEAN RAISES {E} =
  VAR
    list : TextSeq.T;
    okay := TRUE;
    prefix : TEXT := NIL;
  BEGIN
    res  := "";
    IF NOT FileObj.Exists(fn) THEN RETURN FALSE END;
    IF FileObj.IsDir(fn) THEN prefix := APN.Last(fn).denotation() END;
    list := StateList(self, fn);
    FOR i := 0 TO list.size() - 1 DO
      WITH line = list.get(i) DO
        VAR
          state := Text.GetChar(line, 0);
          name  := Text.Sub(line, 2); 
          k     :  CARDINAL;
        BEGIN
          IF prefix # NIL AND TextEx.FindSub(name, prefix, k)
             AND k = 0 THEN
            name := Text.Sub(name, Text.Length(prefix) + 1, LAST(CARDINAL));
          END;
          IF state = 'U' OR state = 'C' THEN
            okay := FALSE;
          END;
          res := res & Fmt.Pad(name & " ", 51, '.', Fmt.Align.Left) & 
                     " " & StateCharToText(state) & "\n";
        END;
      END;
    END;
    RETURN okay;
  END UpToDate; 

(*---------------------------------------------------------------------------*)
PROCEDURE Conflicts(self : T; fn : APN.T; VAR res : TEXT) : BOOLEAN 
  RAISES {E} =
  VAR
    list : TextSeq.T;
    conflicts := FALSE;
    prefix : TEXT := NIL;
  BEGIN
    res  := "";
    IF NOT FileObj.Exists(fn) THEN RETURN FALSE END;
    IF FileObj.IsDir(fn) THEN prefix := APN.Last(fn).denotation() END;
    list := StateList(self, fn);
    FOR i := 0 TO list.size() - 1 DO
      WITH line = list.get(i) DO
        VAR
          state := Text.GetChar(line, 0);
          name  := Text.Sub(line, 2); 
          k     :  CARDINAL;
        BEGIN
          IF prefix # NIL AND TextEx.FindSub(name, prefix, k)
             AND k = 0 THEN
            name := Text.Sub(name, Text.Length(prefix) + 1, LAST(CARDINAL));
          END;
          IF state = 'C' THEN
            conflicts := TRUE;
          END;
          res := res & Fmt.Pad(name & " ", 51, '.', Fmt.Align.Left) & 
                     " " & StateCharToText(state) & "\n";
        END;
      END;
    END;
    RETURN conflicts;
  END Conflicts;

(*---------------------------------------------------------------------------*)
PROCEDURE Import(self       : T; 
                 fn         : APN.T; 
                 vtag, rtag : TEXT; 
                 VAR   res  : TEXT;
                 msg        : TEXT := NIL; 
                 msgFile    : TEXT := NIL;
                 desc       : TEXT := ""; 
                 pkg        : TEXT := ""
                ) : BOOLEAN RAISES {E} =
  VAR
    outRd :  Rd.T;
    pid   :  Process.T;
    args  :  REF ARRAY OF TEXT;
    ret   :  Process.ExitCode;
    errfn := ErrlogFilename(self);
    okay  :  BOOLEAN;
  BEGIN
    res := "no error specification";
    args := NEW(REF ARRAY OF TEXT, 6);
    args^[0] := "import";
    LOCK self.mu DO
    IF msg # NIL OR msgFile # NIL THEN
      IF msg # NIL THEN
        args^[1] := "-m";
        args^[2] := OSSpecials.QuotedProcessArgument(msg);
      ELSE
        args^[1] := "-F";
        args^[2] := msgFile;
      END;
    ELSE
      msg := GetCommitMessage(self, desc, pkg);
      IF msg = NIL THEN
        (* We won't continue without a valid commit message for our logs *)
        MsgX.Error2(self.msgif, "Commit", "couldn't get commit message");
        RAISE E("editor session failed");
      END;
      args^[1] := "-m";
      args^[2] := OSSpecials.QuotedProcessArgument(msg);
    END;
    args^[3] := fn.denotation();
    args^[4] := vtag;
    args^[5] := rtag;
    TRY
      CVS_Call(self, args^, pid, outRd, fn.denotation(), errfn);
      IF outRd # NIL THEN
        res := Rd.GetText(outRd, LAST(CARDINAL));
      END;
    EXCEPT 
      Rd.EndOfFile => (* skip *)
    ELSE 
      MsgX.Error2(self.msgif, "Import", "exception while reading");
    END;
    IF noAction THEN RETURN TRUE END;
    TRY
      ret := Process.Wait(pid);
      CvsUnlock();
      IF outRd # NIL THEN
        Rd.Close(outRd);
      END;
      MsgX.D(self.msgif, self.cvst & " exited with " & Fmt.Int(ret));
    EXCEPT 
      Rd.EndOfFile => (* skip *)
    ELSE 
      MsgX.Error2(self.msgif, "Import", "exception while finalizing");
      IF ret = 0 THEN
	ret := 1;
      END;
    END;
    okay := EvalCVSErrors(self.msgif, errfn, res, terminate := FALSE);
    RemoveErrlogFile(self, errfn);
    RETURN okay AND (ret = 0);
    END (* lock *);
  END Import;

(*---------------------------------------------------------------------------*)
PROCEDURE Add(self : T; fn : APN.T; binary := FALSE) : BOOLEAN RAISES {E} =

  (*-------------------------------------------------------------------------*)
  PROCEDURE AddFile(fn : APN.T) : BOOLEAN RAISES {E} =
    VAR
      outRd   :  Rd.T;
      pid     :  Process.T;
      args    :  REF ARRAY OF TEXT;
      ret     :  Process.ExitCode;
      wd      :  Pathname.T;
      errfn   := ErrlogFilename(self);
      res     := "";
      errmsg  :  TEXT := NIL;
    BEGIN
      IF binary THEN
        args := NEW(REF ARRAY OF TEXT, 3);
        args^[1] := "-kb";
        args^[2] := APN.Last(fn).denotation();
      ELSE
        args := NEW(REF ARRAY OF TEXT, 2);
        args^[1] := APN.Last(fn).denotation();
      END;
      args^[0] := "add";
      wd := APN.Prefix(fn).denotation();
      IF Text.Empty(wd) THEN
        wd := NIL;
      END;
      TRY
	CVS_Call(self, args^, pid, outRd, wd, errfn);
        IF outRd # NIL THEN
          EVAL Rd.GetText(outRd, LAST(CARDINAL));
        END;
      EXCEPT
        Rd.EndOfFile => (* skip *)
      | Rd.Failure   => 
        errmsg := "reader failure";
        MsgX.Error2(self.msgif, "AddFile", errmsg);
      ELSE
        errmsg := "exception while reading";
        MsgX.Error2(self.msgif, "AddFile", errmsg);
      END;
      IF noAction THEN RETURN TRUE END;
      TRY
	ret := Process.Wait(pid);
        CvsUnlock();
        MsgX.D(self.msgif, self.cvst & " exited with " & Fmt.Int(ret));
        IF outRd # NIL THEN
          Rd.Close(outRd);
        END;
      EXCEPT ELSE
        MsgX.Error2(self.msgif, "AddFile", "exception while finalizing");
      END;
      IF errmsg # NIL THEN
        RAISE E(errmsg);
      END;
      IF NOT EvalCVSErrors(self.msgif, errfn, res, terminate := FALSE) THEN
        MsgX.Error(self.msgif, res);
        RAISE E(res);
      END;
      RETURN ret = 0;
    END AddFile;

  (*-------------------------------------------------------------------------*)
  VAR (* Add *)
    res    : BOOLEAN;
  BEGIN (* Add *)
    IF NOT FileObj.Exists(fn) THEN 
      RETURN FALSE;
    END;
    LOCK self.mu DO
      res := AddFile(fn);
      RETURN res;
    END;
  END Add; 

(*---------------------------------------------------------------------------*)
PROCEDURE Remove(self : T; fn : APN.T) : BOOLEAN RAISES {E} =

  (*-------------------------------------------------------------------------*)
  PROCEDURE RemoveFile(fn : APN.T) : BOOLEAN RAISES {E} =
    VAR
      outRd   :  Rd.T;
      pid     :  Process.T;
      args    :  ARRAY [0..2] OF TEXT;
      ret     :  Process.ExitCode;
      wd      :  Pathname.T;
      errfn   := ErrlogFilename(self);
      res     := "";
      errmsg  :  TEXT := NIL;
    BEGIN
      args[0] := "remove";
      args[1] := "-l";
      args[2] := APN.Last(fn).denotation();
      wd := APN.Prefix(fn).denotation();
      IF Text.Empty(wd) THEN
        wd := NIL;
      END;
      TRY
	CVS_Call(self, args, pid, outRd, wd, errfn);
        IF outRd # NIL THEN
          EVAL Rd.GetText(outRd, LAST(CARDINAL));
        END;
      EXCEPT
        Rd.EndOfFile => (* skip *)
      | Rd.Failure   => 
        errmsg := "reader failure";
        MsgX.Error2(self.msgif, "RemoveFile", errmsg);
      ELSE
        errmsg := "exception while reading";
        MsgX.Error2(self.msgif, "RemoveFile", errmsg);
      END;
      IF noAction THEN RETURN TRUE END;
      TRY
	ret := Process.Wait(pid);
        CvsUnlock();
        MsgX.D(self.msgif, self.cvst & " exited with " & Fmt.Int(ret));
        IF outRd # NIL THEN
          Rd.Close(outRd);
        END;
      EXCEPT ELSE
        MsgX.Error2(self.msgif, "RemoveFile", "exception while finalizing");
      END;
      IF errmsg # NIL THEN
        RAISE E(errmsg);
      END;
      IF NOT EvalCVSErrors(self.msgif, errfn, res, terminate := FALSE) THEN
        MsgX.Error(self.msgif, res);
        RAISE E(res);
      END;
      RETURN ret = 0;
    END RemoveFile;

  (*-------------------------------------------------------------------------*)
  VAR (* Remove *)
    res    : BOOLEAN;
  BEGIN (* Remove *)
    LOCK self.mu DO
      res := RemoveFile(fn);
      RETURN res;
    END;
  END Remove; 

(*---------------------------------------------------------------------------*)
PROCEDURE CurrentStickyTags(self : T; fn : APN.T) : TextSeq.T RAISES {E} =
  VAR
    res      := NEW(TextSeq.T).init(5);
    restext  :  TEXT := "";
    table    :  TextTextTbl.T;
    it       :  TextTextTbl.Iterator;
    outRd    :  Rd.T;
    resseq   := NEW(TextSeq.T).init();
    pid      :  Process.T;
    args     :  ARRAY [1..3] OF TEXT;
    line     :  TEXT;
    tag      :  TEXT;
    i        :  CARDINAL;
    wd       :  Pathname.T;
    fnp      := fn.denotation();
    errfn    := ErrlogFilename(self);
    wait     := TRUE;
    closeRd  := TRUE;
    errmsg   :  TEXT := NIL;
    ssnap    :  TEXT;
  BEGIN
    IF NOT FileObj.Exists(fn) THEN RETURN res END;
    LOCK self.mu DO
    ssnap := ReadStickySnapFile(self, fn);
    IF ssnap # NIL THEN
      res.addhi(ssnap);
      RETURN res;
    END;
    IF self.cache.contains("StickyTags", fnp) THEN
      RETURN self.cache.getSeq("StickyTags", fnp);
    END;
    table := NEW(TextTextTbl.Default).init(5);
    args[1] := "-q";
    args[2] := "stat";
    args[3] := APN.Last(fn).denotation();
    wd := APN.Prefix(fn).denotation();
    IF Text.Empty(wd) THEN
      wd := NIL;
    END;
    TRY
      CVS_Call(self, args, pid, outRd, wd, errfn);
      IF NOT noAction THEN
        WHILE NOT Rd.EOF(outRd) DO
          line := Rd.GetLine(outRd);
          i := 0;
          IF NOT Text.Empty(line) AND ' ' = Text.GetChar(line, 0) THEN
            IF TextEx.FindSub(line, "Sticky Tag:", i) THEN
              VAR rd := TextRd.New(line); BEGIN
                tag := GetToken(rd);
                tag := GetToken(rd);
                tag := GetToken(rd);
                IF NOT Text.Equal(tag, "none") AND 
                  NOT Text.Equal(tag, "(none)") THEN
                  IF NOT TextUtils.MemberOfTextSeq(resseq, tag) THEN
                    resseq.addhi(tag);
                  END;
                  EVAL table.put(tag, line);
                END;
              END;
            END;
          END;
        END;
      END;
    EXCEPT
      Rd.EndOfFile => (* skip *)
    | Rd.Failure   => 
      errmsg := "reader failure";
      MsgX.Error2(self.msgif, "CurrentStickyTags", errmsg);
    | OSError.E    => 
      errmsg := "execution of " & self.mcvs.denotation() & " failed";
      MsgX.Error2(self.msgif, "CurrentStickyTags", errmsg);
      closeRd := FALSE;
    ELSE
      errmsg := "exception while reading";
      MsgX.Error2(self.msgif, "CurrentStickyTags", errmsg);
    END;
    IF noAction THEN RETURN res END;
    TRY
      IF closeRd THEN
        IF outRd # NIL THEN
          Rd.Close(outRd);
        END;
        IF wait THEN
          EVAL Process.Wait(pid);
          CvsUnlock();
        END;
      END;
    EXCEPT ELSE
      MsgX.Error2(self.msgif, "CurrentStickyTags",
                  "exception while finalizing");
    END;
    TRY
      IF errmsg # NIL THEN
        RAISE E(errmsg);
      END;
      restext := TextUtils.TextSeqToText(resseq);
      self.cache.putSeq("StickyTags", fnp, resseq);
      IF NOT EvalCVSErrors(self.msgif, errfn, restext) THEN
        RAISE E(restext);
      END;
    FINALLY
      RemoveErrlogFile(self, errfn);
    END;
    it := table.iterate();
    WHILE it.next(tag, line) DO
      res.addhi(tag);
    END;
    RETURN res;
    END (* lock *);
  END CurrentStickyTags; 

(*---------------------------------------------------------------------------*)
PROCEDURE FlushCache(self : T; ) =
  BEGIN
    LOCK self.mu DO
      EVAL self.cache.init();
    END;
  END FlushCache;

(*---------------------------------------------------------------------------*)
PROCEDURE Cleanup() = 
  VAR
    ref       : REFANY;
    stdinFile : File.T;
    stdinFN   : TEXT;
    text      : TEXT;
    iter      : TextRefTbl.Iterator;
  BEGIN
    IF ftab # NIL THEN
      iter := ftab.iterate();
      WHILE iter.next(text, ref) DO
        IF ISTYPE(ref, File.T) THEN
          stdinFile := NARROW(ref, File.T);
          IF stdinFile # NIL THEN
            TRY
              stdinFile.close();
            EXCEPT ELSE END;
          END;
          stdinFN := StdinFilename(text);
          TRY
            FSUtils.Rm(stdinFN);
          EXCEPT ELSE END;
        END;
      END;
    END;
  END Cleanup;

(*---------------------------------------------------------------------------*)
PROCEDURE Watch(self : T; fn : APN.T; 
                cmd : TEXT := "on"; recursive := TRUE) RAISES {E} =
  VAR
    proc    := "Watch";
    outRd   :  Rd.T;
    pid     :  Process.T;
    args    :  REF ARRAY OF TEXT;
    wd      :  APN.T;
    errfn   := ErrlogFilename(self);
    closeRd := TRUE;
    ret     :  INTEGER;
    errmsg  :  TEXT := NIL;
  BEGIN
    LOCK self.mu DO
      wd := APN.Prefix(fn);
      args := NEW(REF ARRAY OF TEXT, 4);
      args^[0] := "watch";
      args^[1] := cmd;
      args^[2] := "-R";
      args^[3] := APN.Last(fn).denotation();
      IF NOT recursive THEN
        args^[2] := "-l";
      END;
      TRY
        CVS_Call(self, args^, pid, outRd, wd.denotation(), errfn);
        IF outRd # NIL THEN
          EVAL Rd.GetText(outRd, LAST(CARDINAL));
        END;
      EXCEPT 
      | Rd.Failure     => 
        errmsg := "reader failure";
        MsgX.Error2(self.msgif, proc, errmsg);
      | OSError.E      => 
        errmsg := "execution of " & self.mcvs.denotation() & " failed";
        MsgX.Error2(self.msgif, proc, errmsg);
        closeRd := FALSE;
      | Thread.Alerted => 
        errmsg := "exception while reading cvs output";
        MsgX.Error2(self.msgif, proc, errmsg);
      END;
      IF noAction THEN RETURN END;
      TRY
        IF closeRd AND outRd # NIL THEN
          Rd.Close(outRd);
          ret := Process.Wait(pid);
          CvsUnlock();
        END;
        MsgX.D(self.msgif, self.cvst & " exited with " & Fmt.Int(ret));
        (* does not work reliably 
           IF ret # 0 THEN
           errmsg := self.cvst & " exited with " & Fmt.Int(ret);
           END;
        *)
      EXCEPT ELSE 
        MsgX.Error2(self.msgif, proc, "exception in finalization");
      END;
      TRY
        IF errmsg # NIL THEN
          RAISE E(errmsg);
        END;
        IF NOT EvalCVSErrors(self.msgif, errfn, errmsg) THEN
          RemoveErrlogFile(self, errfn);
          RAISE E(errmsg);
        END;
      FINALLY
        RemoveErrlogFile(self, errfn);
      END;
    END (* lock *)
  END Watch;

(*---------------------------------------------------------------------------*)
PROCEDURE Editors(self : T; fn : APN.T; recursive := TRUE) : TextSeq.T
  RAISES {E} =
  VAR
    lockInfo := EditorInfo(self, fn, recursive);
    liseq    :  CVSLockInfoSeq.T;
    iter     := lockInfo.iterate();
    fnact    :  TEXT;
    li       :  CVSLockInfo.T;
    res      := NEW(TextSeq.T).init();
  BEGIN
    WHILE iter.next(fnact, liseq) DO
      FOR i := 0 TO liseq.size() - 1 DO
        li := liseq.get(i);
        IF NOT TextUtils.MemberOfTextSeq(res, li.user) THEN
          res.addhi(li.user);
        END;
      END;
    END;
    RETURN res;
  END Editors;

(*---------------------------------------------------------------------------*)
PROCEDURE EditorInfo(self : T; fn : APN.T; recursive := TRUE)
  : TextLockInfoTbl.T RAISES {E} =
  VAR 
    outRd   :  Rd.T;
    pid     :  Process.T;
    args    :  REF ARRAY OF TEXT;
    tokens  :  TextSeq.T;
    res     :  TextLockInfoTbl.T;
    liseq   :  CVSLockInfoSeq.T;
    wd      :  APN.T;
    errfn   := ErrlogFilename(self);
    closeRd := TRUE;
    ret     :  INTEGER;
    errmsg  :  TEXT := NIL;
    line    :  TEXT;
    proc    := "Editors";
    actfn   := "[unknown file]";
  BEGIN
    LOCK self.mu DO
      wd := APN.Prefix(fn);
      args := NEW(REF ARRAY OF TEXT, 3);
      args^[0] := "editors";
      args^[1] := "-R";
      args^[2] := APN.Last(fn).denotation();
      IF NOT recursive THEN
        args^[1] := "-l";
      END;
      TRY
        CVS_Call(self, args^, pid, outRd, wd.denotation(), errfn);
        res := NEW(TextLockInfoTbl.Default).init();
        WHILE outRd # NIL AND NOT Rd.EOF(outRd) DO
          line := Rd.GetLine(outRd);
          tokens := TextUtils.Tokenize(line, sepchars := ASCII.Set{ASCII.HT},
                                       squeeze := FALSE);
          (* format is
|              ? filename
|            or
|              filename\tuser\tdate\thost\tfile
|            or (continuation line)
|                      \tuser\tdate\thost\tfile
           *)
          WITH li = NEW(CVSLockInfo.T) DO
            IF tokens.size() > 4 THEN
              li.fn := tokens.get(0);
              IF Text.Empty(li.fn) THEN
                li.fn := actfn;
              ELSE
                actfn := li.fn;
              END;
              li.user := tokens.get(1);
              li.date := tokens.get(2);
              li.host := tokens.get(3);
              li.dir := tokens.get(4);
              IF NOT res.get(li.fn, liseq) THEN
                liseq := NEW(CVSLockInfoSeq.T).init();
              END;
              liseq.addhi(li);
              EVAL res.put(li.fn, liseq);
            ELSE
              IF TextUtils.Pos(line, "? ") # 0 THEN
                MsgX.Error2(self.msgif, proc, 
                            "unexpected cvs output: " & line);
              END;
            END;
          END;
        END;
      EXCEPT 
      | Rd.EndOfFile   => (* skip *)
      | Rd.Failure     => 
        errmsg := "reader failure";
        MsgX.Error2(self.msgif, proc, errmsg);
      | OSError.E      => 
        errmsg := "execution of " & self.mcvs.denotation() & " failed";
        MsgX.Error2(self.msgif, proc, errmsg);
        closeRd := FALSE;
      | Thread.Alerted => 
        errmsg := "exception while reading cvs output";
        MsgX.Error2(self.msgif, proc, errmsg);
      END;
      IF noAction THEN RETURN res END;
      TRY
        IF closeRd AND outRd # NIL THEN
          Rd.Close(outRd);
          ret := Process.Wait(pid);
          CvsUnlock();
        END;
        MsgX.D(self.msgif, self.cvst & " exited with " & Fmt.Int(ret));
        (* does not work reliably 
           IF ret # 0 THEN
           errmsg := self.cvst & " exited with " & Fmt.Int(ret);
           END;
        *)
      EXCEPT ELSE 
        MsgX.Error2(self.msgif, proc, "exception in finalization");
      END;
      TRY
        IF errmsg # NIL THEN
          RAISE E(errmsg);
        END;
        IF NOT EvalCVSErrors(self.msgif, errfn, errmsg) THEN
          RemoveErrlogFile(self, errfn);
          RAISE E(errmsg);
        END;
      FINALLY
        RemoveErrlogFile(self, errfn);
      END;
      RETURN res;
    END (* lock *)
  END EditorInfo;

(*---------------------------------------------------------------------------*)
PROCEDURE Edit(self : T; fn : APN.T; recursive := TRUE) RAISES {E} =
  VAR
    proc    := "Edit";
    outRd   :  Rd.T;
    pid     :  Process.T;
    args    :  REF ARRAY OF TEXT;
    wd      :  APN.T;
    errfn   := ErrlogFilename(self);
    closeRd := TRUE;
    ret     :  INTEGER;
    errmsg  :  TEXT := NIL;
  BEGIN
    LOCK self.mu DO
      wd := APN.Prefix(fn);
      args := NEW(REF ARRAY OF TEXT, 3);
      args^[0] := "edit";
      args^[1] := "-R";
      args^[2] := APN.Last(fn).denotation();
      IF NOT recursive THEN
        args^[1] := "-l";
      END;
      TRY
        CVS_Call(self, args^, pid, outRd, wd.denotation(), errfn);
        IF outRd # NIL THEN
          EVAL Rd.GetText(outRd, LAST(CARDINAL));
        END;
      EXCEPT 
      | Rd.Failure     => 
        errmsg := "reader failure";
        MsgX.Error2(self.msgif, proc, errmsg);
      | OSError.E      => 
        errmsg := "execution of " & self.mcvs.denotation() & " failed";
        MsgX.Error2(self.msgif, proc, errmsg);
        closeRd := FALSE;
      | Thread.Alerted => 
        errmsg := "exception while reading cvs output";
        MsgX.Error2(self.msgif, proc, errmsg);
      END;
      IF noAction THEN RETURN END;
      TRY
        IF closeRd AND outRd # NIL THEN
          Rd.Close(outRd);
          ret := Process.Wait(pid);
          CvsUnlock();
        END;
        MsgX.D(self.msgif, self.cvst & " exited with " & Fmt.Int(ret));
        (* does not work reliably 
           IF ret # 0 THEN
           errmsg := self.cvst & " exited with " & Fmt.Int(ret);
           END;
        *)
      EXCEPT ELSE 
        MsgX.Error2(self.msgif, proc, "exception in finalization");
      END;
      TRY
        IF errmsg # NIL THEN
          RAISE E(errmsg);
        END;
        IF NOT EvalCVSErrors(self.msgif, errfn, errmsg) THEN
          RemoveErrlogFile(self, errfn);
          RAISE E(errmsg);
        END;
      FINALLY
        RemoveErrlogFile(self, errfn);
      END;
    END (* lock *)
  END Edit;

(*---------------------------------------------------------------------------*)
PROCEDURE Unedit(self : T; fn : APN.T; recursive := TRUE) RAISES {E} =
  VAR
    proc    := "Unedit";
    outRd   :  Rd.T;
    pid     :  Process.T;
    args    :  REF ARRAY OF TEXT;
    wd      :  APN.T;
    errfn   := ErrlogFilename(self);
    closeRd := TRUE;
    ret     :  INTEGER;
    errmsg  :  TEXT := NIL;
  BEGIN
    LOCK self.mu DO
      wd := APN.Prefix(fn);
      args := NEW(REF ARRAY OF TEXT, 3);
      args^[0] := "unedit";
      args^[1] := "-R";
      args^[2] := APN.Last(fn).denotation();
      IF NOT recursive THEN
        args^[1] := "-l";
      END;
      TRY
        CVS_Call(self, args^, pid, outRd, wd.denotation(), errfn,
                 stdInText := "y", stdInCount := 1024);
        IF outRd # NIL THEN
          EVAL Rd.GetText(outRd, LAST(CARDINAL));
        END;
      EXCEPT 
      | Rd.Failure     => 
        errmsg := "reader failure";
        MsgX.Error2(self.msgif, proc, errmsg);
      | OSError.E      => 
        errmsg := "execution of " & self.mcvs.denotation() & " failed";
        MsgX.Error2(self.msgif, proc, errmsg);
        closeRd := FALSE;
      | Thread.Alerted => 
        errmsg := "exception while reading cvs output";
        MsgX.Error2(self.msgif, proc, errmsg);
      END;
      IF noAction THEN RETURN END;
      TRY
        IF closeRd AND outRd # NIL THEN
          Rd.Close(outRd);
          ret := Process.Wait(pid);
          CvsUnlock();
        END;
        MsgX.D(self.msgif, self.cvst & " exited with " & Fmt.Int(ret));
        (* does not work reliably 
           IF ret # 0 THEN
           errmsg := self.cvst & " exited with " & Fmt.Int(ret);
           END;
        *)
      EXCEPT ELSE 
        MsgX.Error2(self.msgif, proc, "exception in finalization");
      END;
      TRY
        IF errmsg # NIL THEN
          RAISE E(errmsg);
        END;
        IF NOT EvalCVSErrors(self.msgif, errfn, errmsg) THEN
          RemoveErrlogFile(self, errfn);
          RAISE E(errmsg);
        END;
      FINALLY
        RemoveErrlogFile(self, errfn);
      END;
    END (* lock *)
  END Unedit;

(*---------------------------------------------------------------------------*)
PROCEDURE NumParts(r: TEXT): CARDINAL =
  VAR
    np := 1;
    pos := Text.FindChar(r, '.');
  BEGIN
    WHILE pos >= 0 DO
      INC(np);
      pos := Text.FindChar(r, '.', pos+1);
    END;
    RETURN np;
  END NumParts;

(*---------------------------------------------------------------------------*)
PROCEDURE Prefix(r: TEXT): TEXT =
  BEGIN
    WITH pos = Text.FindCharR(r, '.') DO
      IF pos = -1 THEN
        RETURN "";
      ELSE
        RETURN Text.Sub(r, 0, pos);
      END;
    END;
  END Prefix;

(*---------------------------------------------------------------------------*)
PROCEDURE Last(r: TEXT): TEXT =
  BEGIN
    WITH pos = Text.FindCharR(r, '.') DO
      IF pos = -1 THEN
        RETURN r;
      ELSE
        RETURN Text.Sub(r, pos+1);
      END;
    END;
  END Last;

(*---------------------------------------------------------------------------*)
PROCEDURE IsCVSBranch(r: TEXT): BOOLEAN =
  (* <=> n > 2 AND n MOD 2 = 1 OR 
         n > 2 AND n MOD 2 = 0 AND Last(Prefix(r)) = 0
         for n = NumParts(r) *)
  VAR n := NumParts(r);
  BEGIN
    RETURN n > 2 AND n MOD 2 = 1 OR
           n > 2 AND n MOD 2 = 0 AND Text.Equal(Last(Prefix(r)), "0");
  END IsCVSBranch;

(*---------------------------------------------------------------------------*)
CONST StickySnapFN = "StickySnap";

(*---------------------------------------------------------------------------*)
PROCEDURE SaveStickySnapFile(fn: APN.T; text: TEXT) RAISES {E} =
  VAR sfn : TEXT;
  BEGIN
    IF NOT FSUtils.IsDir(fn.denotation()) THEN
      RAISE E("directory " & fn.denotation() & " does not exist");
    END;
    sfn := Pathname.Join(fn.denotation(), StickySnapFN);
    TRY
      FSUtils.PutFile(sfn, text);
    EXCEPT
      FSUtils.E(m) => RAISE E(m);
    END;
  END SaveStickySnapFile;

(*---------------------------------------------------------------------------*)
PROCEDURE ReadStickySnapFile(<*UNUSED*>self: T; fn: APN.T): TEXT RAISES {E} =
  VAR sfn, res: TEXT;
  BEGIN
    IF NOT FSUtils.IsDir(fn.denotation()) THEN
      RAISE E("directory " & fn.denotation() & " does not exist");
    END;
    sfn := Pathname.Join(fn.denotation(), StickySnapFN);
    res := NIL;
    TRY
      IF FSUtils.IsReadable(sfn) THEN
        res := FSUtils.FileContents(sfn);
      END;
    EXCEPT
      FSUtils.E(m) => RAISE E(m);
    END;
    RETURN res;
  END ReadStickySnapFile;

(*---------------------------------------------------------------------------*)
PROCEDURE RemoveStickySnapFile(fn: APN.T) RAISES {E} =
  VAR sfn: TEXT;
  BEGIN
    IF NOT FSUtils.IsDir(fn.denotation()) THEN
      RAISE E("directory " & fn.denotation() & " does not exist");
    END;
    sfn := Pathname.Join(fn.denotation(), StickySnapFN);
    TRY
      IF FSUtils.Exists(sfn) THEN
        FSUtils.Rm(sfn);
      END;
    EXCEPT
      FSUtils.E(m) => RAISE E(m);
    END;
  END RemoveStickySnapFile; 

(*---------------------------------------------------------------------------*)
VAR 
  cvsLock      := NEW(MUTEX);
  stdinLock    := NEW(MUTEX);
  random       := NEW(Random.Default).init();
  ftab         := NEW(TextRefTbl.Default).init(); (* pseudo stdin files *)
  fileSuffixC  := 'a';
  nullDev      :  File.T;
  nullDevName  :  TEXT;
  win32        := MxConfig.HOST_OS_TYPE() = MxConfig.OS_TYPE.WIN32;
  cvsDisableVM := win32;
  tmp          :  TEXT := NIL;
  debugFindCVSFiles := FALSE;
BEGIN
  IF MxConfig.HOST_OS_TYPE() = MxConfig.OS_TYPE.POSIX THEN
    nullDevName := "/dev/null";
  ELSE
    nullDevName := "nul";
  END;
  TRY
    nullDev := FS.OpenFile(nullDevName, truncate := FALSE);
  EXCEPT
    OSError.E(a) => MsgX.Error(NIL, "os error opening file " & nullDevName &
      " (" & Atom.ToText(a.head) & ")"); 
  ELSE
    MsgX.Fatal(NIL, "unexpected exception opening file " & nullDevName); 
  END;
  Process.RegisterExitor(Cleanup);
END CVS_st.
