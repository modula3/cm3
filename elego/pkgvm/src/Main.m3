(*--------------------------------------------------------------------------*)
MODULE Main;

IMPORT Stdio, Text, TextWr, Wr, Rsrc, ParseParams, Process, Pathname,
       TextSeq, Env, TextTextTbl, Time, RTCollectorSRC, Fmt, Rd, Thread;
IMPORT PkgVMBundle, Tag, TagSeq, PkgVC, FileInfo, DirTree, Creation,
       FSUtils, CompactRC, Copyright, Release,
       RsrcUtils, SimpleScanner, ScanToken, FindExpr, VCUtils, Confirmation,
       ProcessEnv, System, SimpleValueEnv, CVSLockInfo, CVSLockInfoSeq,
       TextLockInfoTbl;
IMPORT APN AS APN, APNSeq AS APNSeq, PathRepr,
       SMsg AS Msg;

(*--------------------------------------------------------------------------*)
REVEAL
  SimpleScanner.Token = 
    ScanToken.T BRANDED "ScanToken pkgvm 0.0" OBJECT
    METHODS
    END;

(*--------------------------------------------------------------------------*)
TYPE
  Action = {update, commit, release, commitToChangeBranch,
            add, remove, up_to_date, modified,
            lockForEdit, revert, showEditors, 
            setStrictLocking, setNoLocking,
            merge, diff, cdiff, udiff, log, cvslog, annotate,
            vclist, vccheck, list,
            conflicts, tags, tag, isRelease, isReleaseBranch, nextReleaseTag,
            checkout, newCollection, newPackage, currentTag,
            nextDevelTag, lastReleaseBranch, currentStickyTags,
            currentReleaseTag, currentDevelTag, status, shortstatus,
            listLabels, getLabel, setLabel};

  Debug  = {d1, d2, d3, d4, d5, d6, d7, d8};
  DSet   = SET OF Debug;

(*--------------------------------------------------------------------------*)
CONST
  IgnDir = "tmp or temp or CVS";
  IgnFile = "\"PkgCDT\" or \"PkgCRT\" or \"PkgCT\" or \"*~\" or " &
    "\"*.bak\" or \"*.tmp\" or \"*.temp\" or \"PkgErr\"";

(*--------------------------------------------------------------------------*)
VAR (* pkgvm MAIN *)
  targets	 : TextSeq.T;
  nTargets       : CARDINAL;
  mainAction     : Action := Action.modified;
  rsrcPath       : Rsrc.Path;
  homeDir        : Pathname.T;
  packageDir     : Pathname.T;
  pkgName        : TEXT;
  user           : TEXT;
  debug          : DSet := DSet{};
  collection     : TEXT;
  tagArg1        : TEXT := NIL;
  tagArg2        : TEXT := NIL;
  locking        : PkgVC.LockType;

  recursive      : BOOLEAN := FALSE;
  interactive    : BOOLEAN := FALSE;
  branch         : BOOLEAN := FALSE;
  binary         : BOOLEAN := FALSE;
  noAction       : BOOLEAN := FALSE;
  longList       : BOOLEAN := FALSE;
  commitMsg      : TEXT := NIL;
  commitFile     : TEXT := NIL;
  commitType     : PkgVC.CommitType;
  changeName     : TEXT := NIL;
  changeType     : Tag.Kind := Tag.Kind.Change;
  newBranch      : BOOLEAN := FALSE;
  env            : TextTextTbl.T;
  val            : TEXT;
  ignoreDirExpr  := FindExpr.New(IgnDir); <* NOWARN *>
  ignoreFileExpr := FindExpr.New(IgnFile); <* NOWARN *>
  displayTags    := TRUE;
  displayRevs    := TRUE;
  displayLog     := TRUE;
  force          := FALSE;
  callEditor     := FALSE;

  pageit         : BOOLEAN;
  pagerCmd       := Env.Get("PAGER");

  (* merge options *)
  allowUnaryMerge : BOOLEAN := FALSE;
  ignoreMissingDirs : BOOLEAN := FALSE;

(*---------------------------------------------------------------------------*)
PROCEDURE M(msg : TEXT; unconditionalNewLine := TRUE) =
  BEGIN
    TRY
      IF unconditionalNewLine OR NOT Text.Empty(msg) THEN
        Wr.PutText(Stdio.stdout, msg & "\n");
        Wr.Flush(Stdio.stdout);
      END;
    EXCEPT ELSE
      Msg.Fatal("cannot write to stdout", 1000);
    END;
  END M;

(*---------------------------------------------------------------------------*)
PROCEDURE Page(data : TEXT; unconditionalNewLine := TRUE) =
  VAR
    pid  : Process.T;
    wr   : Wr.T;
  BEGIN
    TRY
      pid := System.PipeTo(pagerCmd, wr);
      IF unconditionalNewLine AND NOT Text.Empty(data) THEN
        Wr.PutText(wr, data & "\n");
      ELSE
        Wr.PutText(wr, data);
      END;
      Wr.Close(wr);
      EVAL Process.Wait(pid);
    EXCEPT
    | Wr.Failure     => Msg.Fatal("cannot pipe " & pagerCmd);
    | Thread.Alerted => Msg.Fatal("interrupted piping to " & pagerCmd);
    | System.ExecuteError(e) => Msg.Fatal("execution failed: " & e);
    END;
  END Page;

(*--------------------------------------------------------------------------*)
PROCEDURE PreProcessParameters() =
  BEGIN
    WITH pp = NEW(ParseParams.T).init(Stdio.stderr) DO
      (* some message and trace options *)
      IF pp.keywordPresent("-v") THEN
        Msg.vFlag := TRUE;
      END;
      IF pp.keywordPresent("-d") THEN
        Msg.dFlag := TRUE;
      END;
      Msg.tFlag := NOT pp.keywordPresent("-q");
      noAction := pp.keywordPresent("-n");
    END;
  END PreProcessParameters;

(*--------------------------------------------------------------------------*)
PROCEDURE ProcessParameters() =
  BEGIN
    WITH pp = NEW(ParseParams.T).init(Stdio.stderr) DO
      TRY
        pageit := NOT pp.keywordPresent("-nopager") AND 
                  NOT pp.keywordPresent("-pipe");
	(* help option *)
	IF pp.keywordPresent("-h") OR pp.keywordPresent("-help") THEN
          TRY
            RsrcUtils.PageResource(rsrcPath, "pkgvm.help", pageit);
          EXCEPT
            RsrcUtils.Error(e) =>
            M("error listing description: " & e);
          END;
          Process.Exit(0);
	END;
        (* man option *)
        IF pp.keywordPresent("-man") OR pp.keywordPresent("-desc") THEN
          TRY
            RsrcUtils.PageResource(rsrcPath, "pkgvm.desc", pageit);
          EXCEPT
            RsrcUtils.Error(e) =>
            M("error listing description: " & e);
          END;
          Process.Exit(0);
        END;
	(* copyright option *)
	IF pp.keywordPresent("-cr") OR pp.keywordPresent("-copyright") THEN
          Copyright.Show(Copyright.T.All);
	  Process.Exit(0);
	END;
	(* release version option *)
	IF pp.keywordPresent("-version") THEN
          Release.Show();
	  Process.Exit(0);
	END;
        (* creation date option *)
	IF pp.keywordPresent("-created") THEN
          M(Creation.Date & " on " & Creation.System);
	  Process.Exit(0);
	END;
        (* long / verbose list option *)
	longList := pp.keywordPresent("-l");
	(* some message and trace options *)
	IF    pp.keywordPresent("-v") THEN
	  Msg.vFlag := TRUE;
	END;
	IF    pp.keywordPresent("-d") THEN
	  Msg.dFlag := TRUE;
	END;
	Msg.tFlag := NOT pp.keywordPresent("-q");
        noAction := pp.keywordPresent("-n");

        (* tag arguments *)
        IF pp.keywordPresent("-t") THEN
          tagArg1 := pp.getNext();
          IF pp.keywordPresent("-t") THEN
            tagArg2 := pp.getNext();
          END;
        END;
        (* log options *)
	IF pp.keywordPresent("-TRL") THEN
          displayTags := TRUE;
          displayRevs := TRUE;
          displayLog  := TRUE;
	ELSIF pp.keywordPresent("-TR") THEN
          displayTags := TRUE;
          displayRevs := TRUE;
          displayLog  := FALSE;
	ELSIF pp.keywordPresent("-TL") THEN
          displayTags := TRUE;
          displayRevs := FALSE;
          displayLog  := TRUE;
	ELSIF pp.keywordPresent("-RL") THEN
          displayTags := FALSE;
          displayRevs := TRUE;
          displayLog  := TRUE;
	ELSIF pp.keywordPresent("-T") THEN
          displayTags := TRUE;
          displayRevs := FALSE;
          displayLog  := FALSE;
	ELSIF pp.keywordPresent("-R") THEN
          displayTags := FALSE;
          displayRevs := TRUE;
          displayLog  := FALSE;
	ELSIF pp.keywordPresent("-L") THEN
          displayTags := FALSE;
          displayRevs := FALSE;
          displayLog  := TRUE;
	END;
	(* debug options *)
	WITH d = debug DO
	  IF    pp.keywordPresent("-d1") THEN
	    d := d + DSet{Debug.d1};
	  END;
	  IF    pp.keywordPresent("-d2") THEN
	    d := d + DSet{Debug.d2};
	  END;
	  IF    pp.keywordPresent("-d3") THEN
	    d := d + DSet{Debug.d3};
	  END;
	  IF    pp.keywordPresent("-d4") THEN
	    d := d + DSet{Debug.d4};
	  END;
	  IF    pp.keywordPresent("-d5") THEN
	    d := d + DSet{Debug.d5};
	  END;
	  IF    pp.keywordPresent("-d6") THEN
	    d := d + DSet{Debug.d6};
	  END;
	  IF    pp.keywordPresent("-d7") THEN
	    d := d + DSet{Debug.d7};
	  END;
	  IF    pp.keywordPresent("-d8") THEN
	    d := d + DSet{Debug.d8};
	  END;
	END;
	(* main options: build, depend, genmake, makefile *)
	IF    pp.keywordPresent("-update") OR pp.keywordPresent("-up") THEN
	  mainAction := Action.update;
	ELSIF pp.keywordPresent("-checkout") OR pp.keywordPresent("-co") THEN
	  mainAction := Action.checkout;
	ELSIF pp.keywordPresent("-newcollection") OR 
          pp.keywordPresent("-newcol") OR
          pp.keywordPresent("-newc") THEN
	  mainAction := Action.newCollection;
	ELSIF pp.keywordPresent("-newpackage") OR 
          pp.keywordPresent("-newpkg") OR
          pp.keywordPresent("-newp") THEN
	  mainAction := Action.newPackage;
	ELSIF pp.keywordPresent("-commit") OR pp.keywordPresent("-ci") THEN
	  mainAction := Action.commit;
          commitType := CommitTypeFromText(pp.getNext());
	ELSIF pp.keywordPresent("-release") OR pp.keywordPresent("-rel") THEN
	  mainAction := Action.release;
          commitType := CommitTypeFromText(pp.getNext());
	ELSIF pp.keywordPresent("-changebranchcommit") OR
          pp.keywordPresent("-commitchange") OR
          pp.keywordPresent("-cichange") OR
          pp.keywordPresent("-cic") THEN
	  mainAction := Action.commitToChangeBranch;
          commitType := CommitTypeFromText(pp.getNext());
          changeType := Tag.Kind.Change;
          changeName := pp.getNext();
          newBranch := FALSE;
	ELSIF pp.keywordPresent("-fixbranchcommit") OR
          pp.keywordPresent("-commitfix") OR
          pp.keywordPresent("-cifix") OR
          pp.keywordPresent("-ciF") THEN
	  mainAction := Action.commitToChangeBranch;
          commitType := CommitTypeFromText(pp.getNext());
          changeType := Tag.Kind.Fix;
          changeName := pp.getNext();
          newBranch := FALSE;
	ELSIF pp.keywordPresent("-featurebranchcommit") OR
          pp.keywordPresent("-commitfeature") OR
          pp.keywordPresent("-cifeature") OR
          pp.keywordPresent("-cif") THEN
	  mainAction := Action.commitToChangeBranch;
          commitType := CommitTypeFromText(pp.getNext());
          changeType := Tag.Kind.Feature;
          (* changeName := pp.getNext(); *)
          newBranch := FALSE;
	ELSIF pp.keywordPresent("-newchangebranch") OR
          pp.keywordPresent("-newchange") OR
          pp.keywordPresent("-cinc") THEN
	  mainAction := Action.commitToChangeBranch;
          commitType := PkgVC.CommitType.Major;
          changeType := Tag.Kind.Change;
          changeName := pp.getNext();
          newBranch := TRUE;
	ELSIF pp.keywordPresent("-newfixbranch") OR
          pp.keywordPresent("-newfix") OR
          pp.keywordPresent("-cinF") THEN
	  mainAction := Action.commitToChangeBranch;
          commitType := PkgVC.CommitType.Major;
          changeType := Tag.Kind.Fix;
          changeName := pp.getNext();
          newBranch := TRUE;
	ELSIF pp.keywordPresent("-newfeaturebranch") OR
          pp.keywordPresent("-newfeature") OR
          pp.keywordPresent("-cinf") THEN
	  mainAction := Action.commitToChangeBranch;
          commitType := PkgVC.CommitType.Major;
          changeType := Tag.Kind.Feature;
          changeName := pp.getNext();
          newBranch := TRUE;
	ELSIF pp.keywordPresent("-addfile") OR pp.keywordPresent("-add") THEN
	  mainAction := Action.add;
	ELSIF pp.keywordPresent("-remove") OR pp.keywordPresent("-rm") THEN
	  mainAction := Action.remove;
	ELSIF pp.keywordPresent("-lock") THEN
	  mainAction := Action.lockForEdit;
	ELSIF pp.keywordPresent("-edit") THEN
	  mainAction := Action.lockForEdit;
          callEditor := TRUE;
	ELSIF pp.keywordPresent("-unedit") OR pp.keywordPresent("-revert") THEN
	  mainAction := Action.revert;
	ELSIF pp.keywordPresent("-showeditors") OR 
              pp.keywordPresent("-editors") THEN
	  mainAction := Action.showEditors;
	ELSIF pp.keywordPresent("-strictlocking") OR 
              pp.keywordPresent("-strict") THEN
	  mainAction := Action.setStrictLocking;
	ELSIF pp.keywordPresent("-nostrictlocking") OR 
              pp.keywordPresent("-nostrict") THEN
	  mainAction := Action.setNoLocking;
	ELSIF pp.keywordPresent("-uptodate") OR pp.keywordPresent("-utd") THEN
	  mainAction := Action.up_to_date;
	ELSIF pp.keywordPresent("-modified") OR pp.keywordPresent("-mod") THEN
	  mainAction := Action.modified;
	ELSIF pp.keywordPresent("-conflicts") OR pp.keywordPresent("-con") THEN
	  mainAction := Action.conflicts;
	ELSIF pp.keywordPresent("-showtags") OR pp.keywordPresent("-tags") THEN
	  mainAction := Action.tags;
	ELSIF pp.keywordPresent("-vclist") OR pp.keywordPresent("-vcl") THEN
	  mainAction := Action.vclist;
	ELSIF pp.keywordPresent("-vccheck") OR pp.keywordPresent("-vcc") THEN
	  mainAction := Action.vccheck;
	ELSIF pp.keywordPresent("-list") OR pp.keywordPresent("-ls") OR
              pp.keywordPresent("-tree") THEN
	  mainAction := Action.list;
	ELSIF pp.keywordPresent("-tag") THEN
	  mainAction := Action.tag;
	ELSIF pp.keywordPresent("-merge") THEN
	  mainAction := Action.merge;
	  IF pp.keywordPresent("-allow-unary") OR 
	       pp.keywordPresent("-one") THEN
	     allowUnaryMerge := TRUE;
	  END;
	  IF pp.keywordPresent("-ignore-missing-dirs") OR 
	       pp.keywordPresent("-nod") THEN
	     ignoreMissingDirs := TRUE;
	  END;
	ELSIF pp.keywordPresent("-diff") THEN
	  mainAction := Action.diff;
	ELSIF pp.keywordPresent("-annotate") OR pp.keywordPresent("-ann") THEN
	  mainAction := Action.annotate;
	ELSIF pp.keywordPresent("-cdiff") THEN
	  mainAction := Action.cdiff;
	ELSIF pp.keywordPresent("-udiff") THEN
	  mainAction := Action.udiff;
	ELSIF pp.keywordPresent("-log") THEN
	  mainAction := Action.log;
	ELSIF pp.keywordPresent("-cvslog") THEN
	  mainAction := Action.cvslog;
	ELSIF pp.keywordPresent("-isrelease") OR 
              pp.keywordPresent("-isrel") THEN
	  mainAction := Action.isRelease;
	ELSIF pp.keywordPresent("-isreleasebranch") OR 
              pp.keywordPresent("-isrelb") OR
              pp.keywordPresent("-irb") THEN
	  mainAction := Action.isReleaseBranch;
	ELSIF pp.keywordPresent("-currentstickytags") OR 
              pp.keywordPresent("-cst") THEN
	  mainAction := Action.currentStickyTags;
	ELSIF pp.keywordPresent("-currentreleasetag") OR 
              pp.keywordPresent("-crt") THEN
	  mainAction := Action.currentReleaseTag;
	ELSIF pp.keywordPresent("-currentdeveltag") OR 
              pp.keywordPresent("-lastdeveltag") OR 
              pp.keywordPresent("-ldt") OR 
              pp.keywordPresent("-cdt") THEN
	  mainAction := Action.currentDevelTag;
	ELSIF pp.keywordPresent("-status") OR 
              pp.keywordPresent("-stat") THEN
	  mainAction := Action.status;
	ELSIF pp.keywordPresent("-shortstatus") OR 
              pp.keywordPresent("-sstat") THEN
	  mainAction := Action.shortstatus;
	ELSIF pp.keywordPresent("-nextreleasetag") OR 
              pp.keywordPresent("-nrt") THEN
	  mainAction := Action.nextReleaseTag;
          commitType := CommitTypeFromText(pp.getNext());
	ELSIF pp.keywordPresent("-nextdeveltag") OR 
              pp.keywordPresent("-ndt") THEN
	  mainAction := Action.nextDevelTag;
          commitType := CommitTypeFromText(pp.getNext());
	ELSIF pp.keywordPresent("-lastreleasebranch") OR 
              pp.keywordPresent("-lastreleasetag") OR 
              pp.keywordPresent("-lrt") OR 
              pp.keywordPresent("-lrb") THEN
	  mainAction := Action.lastReleaseBranch;
	ELSIF pp.keywordPresent("-currenttag") OR 
              pp.keywordPresent("-ct") THEN
	  mainAction := Action.currentTag;
	ELSIF pp.keywordPresent("-listlabels") OR 
              pp.keywordPresent("-labels") THEN
	  mainAction := Action.listLabels;
	ELSIF pp.keywordPresent("-listlabel") OR 
              pp.keywordPresent("-getlabel") THEN
	  mainAction := Action.getLabel;
	ELSIF pp.keywordPresent("-setlabel") OR 
              pp.keywordPresent("-label") THEN
	  mainAction := Action.setLabel;
	ELSE
	  mainAction := Action.modified; (* default *)
	END;

        force := pp.keywordPresent("-F") OR pp.keywordPresent("-force");
	IF pp.keywordPresent("-recursive") OR pp.keywordPresent("-r") THEN
          recursive := TRUE;
        END;
	IF pp.keywordPresent("-interactive") OR pp.keywordPresent("-i") THEN
          interactive := TRUE;
        END;
	IF pp.keywordPresent("-branch") OR pp.keywordPresent("-b") THEN
          branch := TRUE;
        END;
	IF pp.keywordPresent("-binary") OR pp.keywordPresent("-bin") THEN
          binary := TRUE;
        END;
	IF pp.keywordPresent("-message") OR pp.keywordPresent("-msg") OR 
          pp.keywordPresent("-m") THEN
          commitMsg := pp.getNext();
        END;
	IF pp.keywordPresent("-file") OR pp.keywordPresent("-f") THEN
          commitFile := pp.getNext();
        END;
        IF pp.keywordPresent("-nostdin") OR
           pp.keywordPresent("-usegui") THEN
          PkgVC.confirmation := NEW(Confirmation.ExternalClosure, 
                                    cmd := "confirm");
        END;
        (* IF mainAction = Action.checkout THEN *)
	  IF pp.keywordPresent("-collection") OR pp.keywordPresent("-c") THEN
	    collection := pp.getNext();
	  END;
        (* END; *)

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
PROCEDURE DetectPackageRootDir() =
  (* post: packageDir, pkgName, pkgKind defined *)

  BEGIN
    VCUtils.DetectPackageRootDir(packageDir, pkgName);
    IF Text.Equal(pkgName, "LonelyPackage") THEN
      IF mainAction # Action.checkout AND 
         mainAction # Action.newCollection THEN
        Msg.Warning("cannot find the package root");
      END;
    END;
  END DetectPackageRootDir;

(*--------------------------------------------------------------------------*)
PROCEDURE CommitTypeFromText(t : TEXT) : PkgVC.CommitType =
  BEGIN
    TRY
      RETURN VCUtils.CommitTypeFromText(t);
    EXCEPT
      PkgVC.E(t) => Msg.Fatal(t); RETURN PkgVC.CommitType.Patch;
    END;
  END CommitTypeFromText;

(*--------------------------------------------------------------------------*)
PROCEDURE CheckoutDir(name : TEXT; tag : Tag.T) : BOOLEAN =
  BEGIN
    TRY
      IF FSUtils.Exists(name) THEN
        IF NOT FSUtils.IsDir(name) THEN
          Msg.Error(name & " exists, but is no directory");
          RETURN FALSE;
        ELSE
          TRY
            PkgVC.VC.setPackageRoot(APN.New(FSUtils.CanonicalPathname(name)));
          EXCEPT 
            FSUtils.E(t) => Msg.Error(t); RETURN FALSE;
          | PkgVC.E(t) => Msg.Error(t); RETURN FALSE; 
          END;
          PkgVC.VC.update(tag);
          Msg.T(PkgVC.VC.lastVCMsg, FALSE);
          TRY
            PkgVC.VC.setPackageRoot(APN.New(packageDir));
          EXCEPT 
            PkgVC.E(t) => Msg.Error(t); RETURN FALSE;
          END;
        END;
      ELSE
        PkgVC.VC.checkout(name, tag);
        Msg.T(PkgVC.VC.lastVCMsg, FALSE);
      END;
      RETURN TRUE
    EXCEPT 
      PkgVC.E(t) => Msg.T(PkgVC.VC.lastVCMsg, FALSE);
                    Msg.Error(t);
                    RETURN FALSE;
    END;
  END CheckoutDir;

(*--------------------------------------------------------------------------*)
PROCEDURE CheckoutPackage(name : TEXT; tag : Tag.T) : BOOLEAN =
  BEGIN
    Msg.V("checking out package " & name);
    RETURN CheckoutDir(name, tag);
  END CheckoutPackage;

(*--------------------------------------------------------------------------*)
PROCEDURE CheckoutCollection(name : TEXT; tag : Tag.T) : BOOLEAN =
  BEGIN
    Msg.V("checking out collection " & name);
    (* FIXME: This might be inappropriate for other systems than CVS *)
    RETURN CheckoutDir(name, tag);
  END CheckoutCollection; 

(*--------------------------------------------------------------------------*)
PROCEDURE CheckoutDirect() : BOOLEAN =
  VAR
    okay := TRUE;
    tag  :  Tag.T;
    prjRoot := Env.Get("PRJ_ROOT");
  BEGIN
    IF nTargets < 1 OR
       nTargets = 1 AND collection = NIL THEN
      Msg.Fatal("please specify exactly one tag and " & 
        "one collection or package");
    ELSIF Text.Equal(targets.get(0), "head") THEN
      tag := Tag.Head;
    ELSE
      tag := Tag.New(targets.get(0));
    END;
    VAR prefix, name : TEXT; 
    BEGIN
      IF collection # NIL THEN
        IF nTargets > 1 THEN
          prefix := collection;
        ELSE
          (* no package, checkout complete colletcion *)
          RETURN CheckoutCollection(
                     APN.New(collection).denotation(APN.Type.Posix), tag);
        END;
      ELSIF prjRoot # NIL THEN
        prefix := prjRoot;
      ELSE
	prefix := NIL;
      END;
      (* checkout all packages given on the command line *)
      FOR i := 1 TO nTargets - 1 DO
	name := PathRepr.Native(targets.get(i));
        IF Pathname.Absolute(name) THEN
          Msg.Error("ignoring absolute package path " & name);
        ELSE
          IF prefix # NIL THEN
            name := Pathname.Join(prefix, name, NIL);
          END;
          okay := okay AND 
            CheckoutPackage(APN.New(name).denotation(APN.Type.Posix), tag);
        END;
      END;
    END;
    RETURN okay;
  END CheckoutDirect;

(*--------------------------------------------------------------------------*)
TYPE
  PkgTreeLayout = DirTree.SimpleTextLayout OBJECT
    known : APNSeq.T;
    added : APNSeq.T;
    removed : APNSeq.T;
    modified : APNSeq.T;
    conflicts : APNSeq.T;
    needingUpdate : APNSeq.T;
    unknown : APNSeq.T;
  METHODS
    init(known, added, removed, modified, conflicts, needingUpdate, 
         unknown : APNSeq.T) : PkgTreeLayout := PkgTreeLayoutInit;
    (*
    fmtDir(pn : APN.T; time : Time.T; size : INTEGER) : TEXT := 
        PkgTreeFmtDir;
    fmtFile(pn : APN.T; time : Time.T; size : INTEGER) : TEXT := 
        PkgTreeFmtFile;
    *)
  OVERRIDES
    fmtDir := PkgTreeFmtDir;
    fmtFile := PkgTreeFmtFile;
  END;

(*---------------------------------------------------------------------------*)
(*
REVEAL DirTree.LayoutClosure = DirTree.PublicLayoutClosure;
*)

(*---------------------------------------------------------------------------*)
PROCEDURE MemberOfAPNSeq(tl : APNSeq.T; elem : APN.T) : BOOLEAN =
  BEGIN
    FOR i := 0 TO tl.size() - 1 DO
      WITH act = tl.get(i) DO
        IF APN.Equal(act, elem) THEN
          RETURN TRUE;
        END;
      END;
    END;
    RETURN FALSE;
  END MemberOfAPNSeq;

(*--------------------------------------------------------------------------*)
PROCEDURE PkgTreeLayoutInit(self : PkgTreeLayout;
                            known : APNSeq.T;
                            added : APNSeq.T;
                            removed : APNSeq.T;
                            modified : APNSeq.T;
                            conflicts : APNSeq.T;
                            needingUpdate : APNSeq.T;
                            unknown : APNSeq.T) : PkgTreeLayout =
  BEGIN
    self.known := known;
    self.added := added;
    self.removed := removed;
    self.modified := modified;
    self.conflicts := conflicts;
    self.needingUpdate := needingUpdate;
    self.unknown := unknown;
    RETURN self;
  END PkgTreeLayoutInit;

(*--------------------------------------------------------------------------*)
PROCEDURE PkgTreeFmtDir(self : PkgTreeLayout; 
                        pn : APN.T;
                        <*UNUSED*> time : Time.T;
                        <*UNUSED*> size : LONGINT) : TEXT =
  VAR prefix := "<dir>  ";
  BEGIN
    IF MemberOfAPNSeq(self.unknown, pn) THEN
      prefix := "<???>  ";
    END;
    RETURN prefix & APN.Last(pn).denotation();
  END PkgTreeFmtDir;

(*--------------------------------------------------------------------------*)
PROCEDURE PkgTreeFmtFile(self : PkgTreeLayout; 
                         pn : APN.T;
                         <*UNUSED*> time : Time.T;
                         <*UNUSED*> size : LONGINT) : TEXT =
  VAR prefix := "<ok >  ";
  BEGIN
    IF NOT MemberOfAPNSeq(self.known, pn) THEN
      prefix := "<ign>  ";
    ELSIF MemberOfAPNSeq(self.unknown, pn) THEN
      prefix := "<???>  "
    ELSIF MemberOfAPNSeq(self.added, pn) THEN
      prefix := "<+++>  "
    ELSIF MemberOfAPNSeq(self.removed, pn) THEN
      prefix := "<--->  "
    ELSIF MemberOfAPNSeq(self.modified, pn) THEN
      prefix := "<mod>  "
    ELSIF MemberOfAPNSeq(self.conflicts, pn) THEN
      prefix := "<cfl> "
    ELSIF MemberOfAPNSeq(self.needingUpdate, pn) THEN
      prefix := "<old>  "
    END;
    RETURN prefix & APN.Last(pn).denotation();
  END PkgTreeFmtFile;

(*--------------------------------------------------------------------------*)
PROCEDURE NewTreeLayouter() : PkgTreeLayout =
  VAR
    known : APNSeq.T;
    added : APNSeq.T;
    removed : APNSeq.T;
    modified : APNSeq.T;
    conflicts : APNSeq.T;
    needingUpdate : APNSeq.T;
    unknown : APNSeq.T;
  BEGIN
    TRY
      known := PkgVC.VC.versionControlledFiles(includePkgName := TRUE);
      PkgVC.VC.getFileStatus(added, removed, modified, conflicts, 
                             needingUpdate, unknown, includePkgName := TRUE);
    EXCEPT PkgVC.E(t) => Msg.T(PkgVC.VC.lastVCMsg, FALSE); 
                         Msg.Fatal("Cannot get file status: " & t);
    END;
    RETURN NEW(PkgTreeLayout).init(known, added, removed, modified,
                                   conflicts, needingUpdate, unknown);
  END NewTreeLayouter; 

(*--------------------------------------------------------------------------*)
PROCEDURE TagExists(t : Tag.T) : BOOLEAN RAISES {PkgVC.E} =
  BEGIN
    RETURN VCUtils.TagExists(PkgVC.VC, t);
  END TagExists;

(*--------------------------------------------------------------------------*)
PROCEDURE LockAndEdit(fn : TEXT) RAISES {PkgVC.E} =
  VAR
    afn := APN.New(fn);
  BEGIN
    PkgVC.VC.lockForEdit(afn, recursive);
    IF callEditor THEN
      TRY
        VAR
          rd      : Rd.T;
          penv    := ProcessEnv.Current();
          process :  Process.T;
          ret     :  INTEGER;
          senv    := NEW(SimpleValueEnv.T).init().setFromTextTextTbl(env);
        BEGIN
          fn := CompactRC.GetEditorCmd(senv, fn);
          ProcessEnv.Add(penv, env);
          process := System.RdExecute(fn, rd, NIL, NIL, NIL);
          ret:= Process.Wait(process);
          TRY Rd.Close(rd) EXCEPT ELSE END;
          Msg.V("process " & Fmt.Int(Process.GetID(process)) &
            " exited with code " & Fmt.Int(ret));
        END;
      EXCEPT
        Thread.Alerted => Msg.Error("interrupted");
        Process.Exit(2);
      | System.ExecuteError(t) => Msg.Error(t);
        Process.Exit(2);
      END;
    END;
  END LockAndEdit;

(*--------------------------------------------------------------------------*)
PROCEDURE ShowLockInfo(lockInfo : TextLockInfoTbl.T) =
  VAR
    liseq :  CVSLockInfoSeq.T;
    li    :  CVSLockInfo.T;
    iter  := lockInfo.iterate();
    fn    :  TEXT;
  BEGIN
    WHILE iter.next(fn, liseq) DO
      FOR i := 0 TO liseq.size() - 1 DO
        li := liseq.get(i);
        IF longList THEN
          M(li.fn & "\n\tedited by " & li.user & " from " & li.host &
            "\n\tsince " & li.date);
        ELSE
          M(Fmt.Pad(li.fn & " ", 61, '.', Fmt.Align.Left) & " " & li.user);
        END;
      END;
    END;
  END ShowLockInfo;

(*--------------------------------------------------------------------------*)
PROCEDURE PerformVersionControl() =
  VAR
    pre   :  TEXT;
    tags  :  TagSeq.T;
    tag   :  Tag.T;
    file  :  APN.T := NIL;
    fn    :  TEXT;
    label :  TEXT;
  BEGIN
    Msg.V("performing version control action...");
    IF noAction THEN
      PkgVC.VC.setMode(PkgVC.Mode.Test);
    END;
    CASE mainAction OF
      Action.checkout =>  
      IF recursive THEN
        Msg.Fatal("recursive checkout not yet implemented, sorry...");
      ELSIF NOT CheckoutDirect() THEN 
        Process.Exit(2);
      END;
    | Action.newCollection =>  
      IF commitFile # NIL THEN
        file := APN.New(commitFile);
      END;
      IF nTargets # 1 THEN
        Msg.Fatal("please specify exactly one collection name");
      END;
      fn := targets.get(0);
      IF FSUtils.Exists(fn) THEN
        Msg.Fatal("directory " & fn & " already exists");
      END;
      TRY 
        PkgVC.VC.newCollection(APN.New(fn), commitMsg, file);
        Msg.T("new collection " & fn & 
              " created and put under " & "version control");
        Msg.T(PkgVC.VC.lastVCMsg, FALSE);
      EXCEPT PkgVC.E(t) => Msg.T(PkgVC.VC.lastVCMsg, FALSE); 
                           Msg.Fatal(t & " New collection failed.");
      END;
    | Action.newPackage =>  
      IF nTargets < 1 THEN
        Msg.Fatal("please specify at least one name");
      END;
      TRY 
        FOR i := 0 TO nTargets - 1 DO
          fn := targets.get(i);
          FSUtils.Mkdir(fn);
          (* FIXME: this may be a problem since PkgVC.VC.pkgRoot is
             not defined correctly *)
          IF NOT PkgVC.VC.add(APN.New(fn), FALSE, FALSE) THEN
            Msg.Error(" Failed to put " & fn & " under version control.");
          END;
          WITH sdir = Pathname.Join(fn, "src", NIL) DO
            FSUtils.Mkdir(sdir);
            IF NOT PkgVC.VC.add(APN.New(sdir), FALSE, FALSE) THEN
              Msg.Error(" Failed to put " & sdir & " under version control.");
            END;
          END;
          WITH ptags = Pathname.Join(fn, "PkgTags", NIL) DO
            FSUtils.Touch(ptags);
            IF NOT PkgVC.VC.add(APN.New(ptags), FALSE, FALSE) THEN
              Msg.Error(" Failed to put " & ptags & " under version control.");
            END;
          END;
        END;
      EXCEPT 
        FSUtils.E(t) => Msg.T(" Cannot create " & fn);
                        Msg.Fatal(t & " New collection failed.");
      | PkgVC.E(t)   => Msg.T(" Version control backend failed: " & t);
                        Msg.Fatal(" New collection failed.");
      END;
    | Action.update =>  
      IF nTargets # 1 THEN
        Msg.Fatal("please specify exactly one tag");
      ELSE
        tag := Tag.New(targets.get(0));
      END;
      TRY
        (* Do Not validate tag names. Rely on (D)CVS to check tag names. *)
        PkgVC.VC.update(tag);
        Msg.T(PkgVC.VC.lastVCMsg, FALSE);
      EXCEPT PkgVC.E(t) => Msg.T(PkgVC.VC.lastVCMsg, FALSE); 
                           Msg.Error(t); Process.Exit(2);
      END;
    | Action.merge =>  
      VAR
        tag1 : Tag.T := NIL;
        tag2 : Tag.T := NIL;
      BEGIN
        IF nTargets = 0 THEN
          IF tagArg1 # NIL THEN
            tag1 := Tag.New(tagArg1);
          END;
          IF tagArg2 # NIL THEN
            tag2 := Tag.New(tagArg2);
          END;
        END;
        IF nTargets > 0 THEN
          WITH p = targets.get(0) DO
            tag1 := Tag.New(p);
          END;
        END;
        IF nTargets > 1 THEN
          WITH p = targets.get(1) DO
            tag2 := Tag.New(p);
          END;
        END;
        IF nTargets > 2 THEN
          Msg.Error("superfluous parameters ignored");
        END;
        IF tag1 = NIL THEN
          Msg.Fatal("need at least one tag for merge");
        END;
	IF tag2 = NIL
           AND NOT (Tag.Attribute.Branch IN tag1.t_attr)
           AND NOT allowUnaryMerge
        THEN
	  Msg.Fatal("Disallowing merge with a single static tag!\n" &
                  " Using a single static tag for a merge does not always work as\n" &
		  " expected. To make sure that removed files are merged correctly, you have\n" &
		  " to supply both a start- and an end-tag marking the changes to be merged,\n" &
                  " or, alternatively, a single *dynamic* branch tag (the branch head).\n" &
		  " (use option `-one' or `-allow-unary' to override)");
        END;
        TRY
          IF NOT TagExists(tag1) THEN
            Msg.Fatal("tag does not exist: " & tag1.originalText());
          END;
          IF tag2 # NIL AND NOT TagExists(tag2) THEN
            Msg.Fatal("tag does not exist: " & tag2.originalText());
          END;
          PkgVC.VC.merge(tag1, tag2, NOT ignoreMissingDirs);
          Msg.T(PkgVC.VC.lastVCMsg, FALSE);
        EXCEPT PkgVC.E(t) => Msg.T(PkgVC.VC.lastVCMsg, FALSE); 
                             Msg.Error(t); Process.Exit(2);
        END;
      END;
    | Action.commit =>  
      IF commitFile # NIL THEN
        file := APN.New(commitFile);
      END;
      TRY
        PkgVC.VC.commitChanges(commitType, commitMsg, file);
      EXCEPT PkgVC.E(t) => Msg.Error(t); Process.Exit(2);
      END;
    | Action.release =>  
      IF commitFile # NIL THEN
        file := APN.New(commitFile);
      END;
      TRY
        PkgVC.VC.commitRelease(commitType, commitMsg, file);
      EXCEPT PkgVC.E(t) => Msg.Error(t); Process.Exit(2);
      END;
    | Action.commitToChangeBranch =>  
      IF commitFile # NIL THEN
        file := APN.New(commitFile);
      END;
      TRY
        PkgVC.VC.commitToChangeBranch(changeName, changeType, commitType,
                                      newBranch, commitMsg, file);
      EXCEPT PkgVC.E(t) => Msg.Error(t); Process.Exit(2);
      END;
    | Action.tag =>  
      IF nTargets # 1 THEN
        Msg.Fatal("please specify exactly one tag");
      END;
      tag := Tag.New(targets.get(0));
      IF NOT tag.okay() AND NOT force THEN
        Msg.Warning("The format of the tag is not approved.");
        IF NOT PkgVC.confirmation.okay("Continue with tagging") THEN
          Msg.Fatal("tagging wisely aborted");
        END;
      END;
      TRY
        PkgVC.VC.tagAll(tag, branch, force);
      EXCEPT PkgVC.E(t) => Msg.Error(t); Process.Exit(2);
      END;
    | Action.add =>  
      VAR okay := TRUE;
      BEGIN
        IF nTargets < 1 THEN
          Msg.Fatal("please specify at least one argument");
        END;
        FOR i := 0 TO nTargets - 1 DO
          WITH arg = targets.get(i) DO
            TRY
              okay := PkgVC.VC.add(APN.New(arg), 
                                   recursive, interactive, binary) AND okay;
            EXCEPT
            | PkgVC.E(t) => Msg.Fatal(" Version control backend failed: " & t);
            END;
          END;
        END;
        IF NOT okay THEN
          Process.Exit(2);
        END;
      END;
    | Action.remove =>  
      VAR okay := TRUE;
      BEGIN
        IF nTargets < 1 THEN
          Msg.Fatal("please specify at least one argument");
        END;
        FOR i := 0 TO nTargets - 1 DO
          WITH arg = targets.get(i) DO
            TRY
              okay := PkgVC.VC.remove(APN.New(arg), 
                                      recursive, interactive) AND okay;
            EXCEPT
            | PkgVC.E(t) => Msg.Fatal(" Version control backend failed: " & t);
            END;
          END;
        END;
        IF NOT okay THEN
          Process.Exit(2);
        END;
      END;
    | Action.lockForEdit =>
      TRY
        IF nTargets = 0 THEN
          PkgVC.VC.lockForEdit(NIL, recursive);
        ELSE
          FOR i := 0 TO nTargets - 1 DO
            WITH arg = targets.get(i) DO
              LockAndEdit(arg);
            END;
          END;
        END;
      EXCEPT
      | PkgVC.E(t) => Msg.Fatal(" Version control backend failed: " & t);
      END;
    | Action.revert =>
      TRY
        IF nTargets = 0 THEN
          PkgVC.VC.revert(NIL, recursive);
        ELSE
          FOR i := 0 TO nTargets - 1 DO
            WITH arg = targets.get(i) DO
              PkgVC.VC.revert(APN.New(arg), recursive);
            END;
          END;
        END;
      EXCEPT
      | PkgVC.E(t) => Msg.Fatal(" Version control backend failed: " & t);
      END;
    | Action.showEditors =>
      TRY
        IF nTargets = 0 THEN
          WITH lockInfo = PkgVC.VC.editorInfo(NIL, recursive) DO
            ShowLockInfo(lockInfo);
          END;
        ELSE
          FOR i := 0 TO nTargets - 1 DO
            WITH arg = targets.get(i) DO
              WITH lockInfo = PkgVC.VC.editorInfo(APN.New(arg), recursive) DO
                ShowLockInfo(lockInfo);
              END;
            END;
          END;
        END;
      EXCEPT
      | PkgVC.E(t) => Msg.Fatal(" Version control backend failed: " & t);
      END;
    | Action.setStrictLocking =>
      TRY
        IF nTargets = 0 THEN
          PkgVC.VC.setLocking(NIL, "on", recursive);
        ELSE
          FOR i := 0 TO nTargets - 1 DO
            WITH arg = targets.get(i) DO
              PkgVC.VC.setLocking(APN.New(arg), "on", recursive);
            END;
          END;
        END;
      EXCEPT
      | PkgVC.E(t) => Msg.Fatal(" Version control backend failed: " & t);
      END;
    | Action.setNoLocking =>
      TRY
        IF nTargets = 0 THEN
          PkgVC.VC.setLocking(NIL, "off", recursive);
        ELSE
          FOR i := 0 TO nTargets - 1 DO
            WITH arg = targets.get(i) DO
              PkgVC.VC.setLocking(APN.New(arg), "off", recursive);
            END;
          END;
        END;
      EXCEPT
      | PkgVC.E(t) => Msg.Fatal(" Version control backend failed: " & t);
      END;
    | Action.up_to_date =>  
      IF nTargets > 0 THEN
        Msg.Error("You may not specify any parameter with this command");
      END;
      TRY
        WITH flag = PkgVC.VC.upToDate() DO
          Msg.T(PkgVC.VC.lastVCMsg, FALSE);
          IF NOT flag THEN
            Process.Exit(2);
          END;
        END;
      EXCEPT
        PkgVC.E(t) => Msg.Fatal(" Version control backend failed: " & t);
      END;
    | Action.modified => 
      IF nTargets > 0 THEN
        Msg.Error("You may not specify any parameter with this command");
      END;
      TRY
        WITH flag = PkgVC.VC.modified() DO
          Msg.T(PkgVC.VC.lastVCMsg, FALSE);
          IF NOT flag THEN
            Process.Exit(2);
          END;
        END;
      EXCEPT
        PkgVC.E(t) => Msg.Fatal(" Version control backend failed: " & t);
      END;
    | Action.conflicts =>  
      IF nTargets > 0 THEN
        Msg.Error("You may not specify any parameter with this command");
      END;
      TRY
        WITH flag = PkgVC.VC.conflicts() DO
          Msg.T(PkgVC.VC.lastVCMsg, FALSE);
          IF NOT flag THEN
            Process.Exit(2);
          END;
        END;
      EXCEPT
        PkgVC.E(t) => Msg.Fatal(" Version control backend failed: " & t);
      END;
    | Action.vclist =>  
      TRY
	WITH res = TextWr.New(), fnlist = PkgVC.VC.versionControlledFiles() DO
	  FOR i := 0 TO fnlist.size() - 1 DO
	    WITH fn = fnlist.get(i).denotation(APN.Type.Posix) DO
              TRY
                Wr.PutText(res, fn & "\n");
              EXCEPT ELSE
                Msg.Fatal("cannot write to text buffer");
              END;
	    END;
	  END;
          Page(TextWr.ToText(res), FALSE);
	END;
      EXCEPT PkgVC.E(t) => Msg.Error(t); Process.Exit(2);
      END;
    | Action.vccheck => Msg.Error("Sorry, not yet implemented");
    | Action.tags =>  
      IF nTargets = 0 THEN
        pre := ""; 
      ELSIF nTargets = 1 THEN
        pre := targets.get(0);
      ELSE
        Msg.Fatal("Please specifiy at most one argument");
      END;
      TRY
        tags := PkgVC.VC.tags(pre);
      EXCEPT
        PkgVC.E(t) => Msg.Fatal(" Version control backend failed: " & t);
      END;
      WITH res = TextWr.New() DO
        FOR i := 0 TO tags.size() - 1 DO
          TRY
            Wr.PutText(res, tags.get(i).denotation() & "\n");
          EXCEPT ELSE
            Msg.Fatal("cannot write to text buffer");
          END;
        END;
        Page(TextWr.ToText(res), FALSE);
      END;
    | Action.list => 
      VAR
        base  := APN.New(Pathname.Prefix(packageDir));
        root  := APN.New(Pathname.Last(packageDir));
        cache := NEW(FileInfo.T).init(100, base);
      BEGIN
        Page(DirTree.Layout(
                  cache,
                  root,
                  lfuns := NewTreeLayouter(),
                  ignDir := ignoreDirExpr,
                  ignFile := ignoreFileExpr,
                  sc := DirTree.SortCriterion.FilesFirst,
                  update := TRUE
        ));
      END;
    | Action.log => 
      TRY
        IF nTargets > 0 THEN
          WITH res = TextWr.New() DO
            FOR i := 0 TO nTargets -1 DO
              TRY
                Wr.PutText(res, PkgVC.VC.niceLog(APN.New(targets.get(i)),
                                                 displayTags, displayRevs,
                                                 displayLog));
              EXCEPT ELSE
                Msg.Fatal("cannot write to text buffer");
              END;
            END;
            Page(TextWr.ToText(res), FALSE);
          END;
        ELSE
          Page(PkgVC.VC.niceLog(NIL, displayTags, displayRevs, displayLog));
        END;
      EXCEPT PkgVC.E(t) => Msg.Error(t); Process.Exit(2);
      END;
    | Action.cvslog => 
      TRY
        IF nTargets > 0 THEN
          WITH res = TextWr.New() DO
            FOR i := 0 TO nTargets -1 DO
              TRY
                Wr.PutText(res, PkgVC.VC.log(APN.New(targets.get(i))));
              EXCEPT ELSE
                Msg.Fatal("cannot write to text buffer");
              END;
            END;
            Page(TextWr.ToText(res), FALSE);
          END;
        ELSE
          Page(PkgVC.VC.log());
        END;
      EXCEPT PkgVC.E(t) => Msg.Error(t); Process.Exit(2);
      END;
    | Action.diff =>  
      VAR
        from : Tag.T := NIL;
        to   : Tag.T := NIL;
        fns  : APNSeq.T := NIL;
      BEGIN
        TRY
          IF tagArg1 # NIL THEN
            from := Tag.New(tagArg1);
            IF NOT TagExists(from) THEN
              Msg.Fatal("tag does not exist: " & from.originalText());
            END;
          END;
          IF tagArg2 # NIL THEN
            to := Tag.New(tagArg2);
            IF NOT TagExists(to) THEN
              Msg.Fatal("tag does not exist: " & to.originalText());
            END;
          END;
          IF nTargets > 0 THEN
            fns := NEW(APNSeq.T).init(targets.size());
            FOR i := 0 TO nTargets -1 DO
              fns.addhi(APN.New(targets.get(i)));
            END;
          END;
          Page(PkgVC.VC.diff(from, to, FALSE, FALSE, fns), FALSE);
        EXCEPT PkgVC.E(t) => Msg.Error(t); Process.Exit(2);
        END;
      END;
    | Action.cdiff =>  
      VAR
        from : Tag.T := NIL;
        to   : Tag.T := NIL;
        fns  : APNSeq.T := NIL;
      BEGIN
        TRY
          IF tagArg1 # NIL THEN
            from := Tag.New(tagArg1);
            IF NOT TagExists(from) THEN
              Msg.Fatal("tag does not exist: " & from.originalText());
            END;
          END;
          IF tagArg2 # NIL THEN
            to := Tag.New(tagArg2);
            IF NOT TagExists(to) THEN
              Msg.Fatal("tag does not exist: " & to.originalText());
            END;
          END;
          IF nTargets > 0 THEN
            fns := NEW(APNSeq.T).init(targets.size());
            FOR i := 0 TO nTargets -1 DO
              fns.addhi(APN.New(targets.get(i)));
            END;
          END;
          Page(PkgVC.VC.diff(from, to, FALSE, TRUE, fns), FALSE);
        EXCEPT PkgVC.E(t) => Msg.Error(t); Process.Exit(2);
        END;
      END;
    | Action.udiff =>  
      VAR
        from : Tag.T := NIL;
        to   : Tag.T := NIL;
        fns  : APNSeq.T := NIL;
      BEGIN
        TRY
          IF tagArg1 # NIL THEN
            from := Tag.New(tagArg1);
            IF NOT TagExists(from) THEN
              Msg.Fatal("tag does not exist: " & from.originalText());
            END;
          END;
          IF tagArg2 # NIL THEN
            to := Tag.New(tagArg2);
            IF NOT TagExists(to) THEN
              Msg.Fatal("tag does not exist: " & to.originalText());
            END;
          END;
          IF nTargets > 0 THEN
            fns := NEW(APNSeq.T).init(targets.size());
            FOR i := 0 TO nTargets -1 DO
              fns.addhi(APN.New(targets.get(i)));
            END;
          END;
          Page(PkgVC.VC.diff(from, to, TRUE, FALSE, fns), FALSE);
        EXCEPT PkgVC.E(t) => Msg.Error(t); Process.Exit(2);
        END;
      END;
    | Action.annotate =>  
      VAR
        fns  : APNSeq.T := NIL;
      BEGIN
        IF nTargets > 0 THEN
          fns := NEW(APNSeq.T).init(targets.size());
          FOR i := 0 TO nTargets -1 DO
            fns.addhi(APN.New(targets.get(i)));
          END;
        END;
        TRY
          Page(PkgVC.VC.annotate(fns));
        EXCEPT PkgVC.E(t) => Msg.Error(t); Process.Exit(2);
        END;
      END;
    | Action.isRelease =>  
      TRY
        IF PkgVC.VC.isRelease(tag) THEN
          Msg.T(tag.denotation()); 
        ELSE
          Msg.T("no complete release found"); 
          Process.Exit(2);
        END;
      EXCEPT
        PkgVC.E(t) => Msg.Fatal(" Version control backend failed: " & t);
      END;
    | Action.isReleaseBranch =>  
      TRY
        IF PkgVC.VC.isReleaseBranch(tag) THEN
          Msg.T(tag.denotation()); 
        ELSE
          Msg.T("no complete release branch found"); 
          Process.Exit(2);
        END;
      EXCEPT
        PkgVC.E(t) => Msg.Fatal(" Version control backend failed: " & t);
      END;
    | Action.currentReleaseTag =>  
      TRY
        tag := PkgVC.VC.currentReleaseTag();
        M(tag.denotation());
      EXCEPT
        PkgVC.E(t) => Msg.Fatal(" Version control backend failed: " & t);
      END;
    | Action.currentDevelTag =>  
      TRY
        tag := PkgVC.VC.currentDevelopmentTag();
        M(tag.denotation());
      EXCEPT
        PkgVC.E(t) => Msg.Fatal(" Version control backend failed: " & t);
      END;
    | Action.nextReleaseTag => 
      TRY
        tag := PkgVC.VC.nextReleaseTag(commitType);
        M(tag.denotation());
      EXCEPT
        PkgVC.E(t) => Msg.Fatal(" Version control backend failed: " & t);
      END;
    | Action.nextDevelTag =>  
      TRY
        tag := PkgVC.VC.nextDevelopmentTag(commitType);
        M(tag.denotation());
      EXCEPT
        PkgVC.E(t) => Msg.Fatal(" Version control backend failed: " & t);
      END;
    | Action.lastReleaseBranch =>       
      TRY
        IF PkgVC.VC.latestReleaseBranch(tag) THEN
          M(tag.denotation());
        ELSE
          M("no release found");
          Process.Exit(2);
        END;
      EXCEPT
        PkgVC.E(t) => Msg.Fatal(" Version control backend failed: " & t);
      END;
    | Action.currentStickyTags =>
      TRY
        IF PkgVC.VC.isSticky(tag)  THEN
          M(tag.denotation());
        ELSE
          M("This package is not completely tagged as `sticky'");
          Process.Exit(2);
        END;
      EXCEPT
        PkgVC.E(t) => Msg.Fatal(" Version control backend failed: " & t);
      END;
    | Action.status =>
      TRY
        VAR
          modified := PkgVC.VC.modified();
          upToDate := PkgVC.VC.upToDate();
          conflicts := PkgVC.VC.conflicts();
          relTag, latestRelTag, stickyTag : Tag.T;
          isSticky := PkgVC.VC.isSticky(stickyTag);
          isRelease := PkgVC.VC.isRelease(relTag);
          isReleaseBranch := PkgVC.VC.isReleaseBranch(relTag);
          latestRelease := PkgVC.VC.latestReleaseBranch(latestRelTag);
          curDevelTag := PkgVC.VC.currentDevelopmentTag();
        BEGIN
          M("");
          M("Version Control Status for Package " & pkgName);
          M("");
          IF modified THEN
            M("The package has been locally modified.");
          ELSE
            M("The package has not been locally modified.");
          END;
          IF upToDate THEN
            M("The package is up-to-date.");
          ELSE
            M("The package is not up-to-date.");
          END;
          IF conflicts THEN
            M("The package contains unresolved conflicts.");
          END;
          M("");
          IF isRelease THEN
            M("It is checked out as release " & relTag.denotation());
          ELSIF isReleaseBranch THEN
            M("It ISTYPE checked out on release branch " & 
              relTag.denotation());
          ELSIF isSticky THEN
            M("It is checked out with sticky tag " & 
              stickyTag.denotation());
          ELSE
            M("It is not checked out as release or with any " &
              "other coherent sticky tag.");
          END;
          IF latestRelease THEN
            M("The latest release has been tagged as " & 
              latestRelTag.denotation());
          ELSE
            M("There has never been a release.");
          END;
          M("The current development tag is " & curDevelTag.denotation());
          M("");
        END;
      EXCEPT
        PkgVC.E(t) => Msg.Fatal(" Version control backend failed: " & t);
      END;
    | Action.shortstatus =>
      TRY
        VAR
          modified := PkgVC.VC.modified();
          upToDate := PkgVC.VC.upToDate();
          conflicts := PkgVC.VC.conflicts();
          relTag, stickyTag : Tag.T;
          isSticky := PkgVC.VC.isSticky(stickyTag);
          isRelease := PkgVC.VC.isRelease(relTag);
          curtag := PkgVC.VC.currentLocalTag();
          res := "";
        BEGIN
          IF modified  THEN res := res & " modified" END;
          IF upToDate  THEN res := res & " up-to-date" END;
          IF conflicts THEN res := res & " conflicts" END;
          IF isSticky  THEN res := res & " sticky:" & 
                                       stickyTag.denotation() END;
          IF isRelease THEN res := res & " release:" & relTag.denotation() END;
          res := res & " current:" & curtag.denotation();
          M(pkgName & ":" & res);
        END;
      EXCEPT
        PkgVC.E(t) => Msg.Fatal(" Version control backend failed: " & t);
      END;
    | Action.currentTag =>
      TRY
        tag := PkgVC.VC.currentLocalTag();
        M(tag.denotation());
      EXCEPT
        PkgVC.E(t) => Msg.Fatal(" Version control backend failed: " & t);
      END;
    | Action.listLabels =>  
      TRY
	WITH res = TextWr.New(), tbl = PkgVC.VC.stateLabels() DO
	  VAR 
	    iter := tbl.iterate();
	    tag  : TEXT;
	  BEGIN
	    WHILE iter.next(tag, label) DO
              TRY
                Wr.PutText(res, tag & " --> " & label & "\n");
              EXCEPT ELSE
                Msg.Fatal("cannot write to text buffer");
              END;
	    END;
	  END;
          Page(TextWr.ToText(res), FALSE);
	END
      EXCEPT PkgVC.E(t) => Msg.Error(t); Process.Exit(2);
      END;
    | Action.setLabel =>  
      IF nTargets # 2 THEN
        Msg.Fatal("please specify exactly one tag and one label");
      END;
      tag := Tag.New(targets.get(0));
      label := targets.get(1);
      TRY
        PkgVC.VC.setLabel(tag, label, commitMsg);
      EXCEPT PkgVC.E(t) => Msg.Error(t); Process.Exit(2);
      END;
    | Action.getLabel =>  
      IF nTargets > 1 THEN
        Msg.Fatal("please specify exactly one tag");
      ELSIF nTargets = 1 THEN
        tag := Tag.New(targets.get(0));
      ELSE
        tag := NIL;
      END;
      TRY
        label := PkgVC.VC.stateLabel(tag);
        M(label);
      EXCEPT PkgVC.E(t) => Msg.Error(t); Process.Exit(2);
      END;
    ELSE
      Msg.Fatal("unknown version control action")
    END;
    Process.Exit(0);
  END PerformVersionControl;

(*--------------------------------------------------------------------------*)
BEGIN (* Main *)
  RTCollectorSRC.incremental := FALSE;
  (* RTCollectorSRC.StartBackgroundCollection(); *)
  debug := DSet{};
  IF pagerCmd = NIL THEN
    pagerCmd := "more";
  END;

  PreProcessParameters();
  env := CompactRC.Evaluate(CompactRC.Eval(PkgVMBundle.Get()));
  EVAL env.get("HOME", homeDir); 
  EVAL env.get("USER", user);
  (* user defined *)
  IF env.get("filecache-ignore-dirs", val) THEN
    ignoreDirExpr  := FindExpr.New(val); <* NOWARN *>
  END;
  IF env.get("filecache-ignore-files", val) THEN
    ignoreFileExpr  := FindExpr.New(val); <* NOWARN *>
  END;

  rsrcPath := 
      Rsrc.BuildPath(Pathname.Join(homeDir, "compact", NIL),
                     "/usr/local/lib/compact",
                     "/opt/compact",
                     PkgVMBundle.Get());
  (* rsrcPath defined *)

  ProcessParameters();
  (* options processed and globals set accordingly *)

  (* disabled in CM3
  IF NOT DemoCheck1.IsDemoVersion() THEN
    VAR
      pass: TEXT;
    BEGIN
      EVAL env.get(CompactEnvName.Passphrase, pass); 
      IF NOT Release.KeyCheck(pass) THEN
        Msg.Error("invalid passphrase");
        Process.Exit(1);
      END;
    END;
  END;
  *)

  DetectPackageRootDir();
  TRY
    PkgVC.VC.setPackageRoot(APN.New(packageDir));
  EXCEPT PkgVC.E(t) => Msg.Error(t); Process.Exit(2); END;
  PkgVC.VC.setEnvironment(env, CompactRC.GetMapping("repository-mapping"));
  locking := PkgVC.VC.lockingScheme();

  PerformVersionControl();
END Main.
