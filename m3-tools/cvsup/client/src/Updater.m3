(* Copyright 1996-2003 John D. Polstra.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgment:
 *      This product includes software developed by John D. Polstra.
 * 4. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * $Id: Updater.m3,v 1.1.1.1 2009-04-09 17:01:42 jkrell Exp $ *)

MODULE Updater;

IMPORT
  Attic, CVProto, CheckoutCreator, CheckoutUpdater, ErrMsg, File,
  FileAttr, FileID, FileStatus, FileUpdater, FileWr, Fixup, Fmt,
  FS, GzipRd, Logger, MD5, MD5Wr, OSError, OSErrorPosix, Pathname,
  Process, RCSDate, RCSError, RCSFile, RCSKeyword, RCSRevNum,
  RCSUpdater, Rd, Reaper, RefSeq, RegularCreator, RegularUpdater,
  RsyncUpdater, StatusFile, StreamRd, SupFileRec, SupFileRecSeq,
  SupMisc, SyncFixupQueue, TempFiles, Text, TextSeq, Thread, Time,
  TokScan, Uerror, Word, Wr;

EXCEPTION Error(TEXT);

TYPE
  ParsedExecute = REF RECORD
    name: Pathname.T;
    command: TEXT;
  END;

REVEAL
  T = Public BRANDED OBJECT
    proto: CVProto.T;
    wireRd: StreamRd.T;	(* Raw reader. *)
    rd: StreamRd.T;	(* Currently active reader. *)
    collections: SupFileRecSeq.T;
    reaper: Reaper.T;
    userDestDir: TEXT;  (* User-specified destination directory, or NIL. *)
    destDir: TEXT;      (* Set for each collection, never NIL. *)
    executes: RefSeq.T; (* ParsedExecute sequence for next file update. *)
    dirsCreated: TextSeq.T; (* A stack of directories that we've created. *)
    fixups: SyncFixupQueue.T;
    stats: Stats;
    trace: Logger.T;
    deleteLimit := -1;
    deleteCount := 0;
    statusFile: StatusFile.T;
    startingWireBytes: LONGREAL;
    startingCommBytes: LONGREAL;
  OVERRIDES
    apply := Apply;
    init := Init;
  END;

CONST
  DevInodeAttrs =
    FileAttr.AttrTypes{ FileAttr.AttrType.Dev, FileAttr.AttrType.Inode };

VAR  (*CONST*)
  EexistAtom := OSErrorPosix.ErrnoAtom(Uerror.EEXIST);
  EnoentAtom := OSErrorPosix.ErrnoAtom(Uerror.ENOENT);

PROCEDURE Apply(self: T): REFANY =
  BEGIN
    TRY
      IF self.stats # NIL THEN
	self.stats.start();
      END;
      TRY
	UpdateBatch(self, isFixups := FALSE);  (* Normal updates. *)
	self.fixups.close();
	UpdateBatch(self, isFixups := TRUE);  (* Fixups. *)
      FINALLY
	IF self.reaper # NIL THEN
	  Reaper.Dying(self.reaper);
	END;
	IF self.stats # NIL THEN
	  self.stats.finish();
	END;
      END;
      RETURN NEW(SupMisc.ThreadStatus,
	status := SupMisc.ExitCode.Success, message := NIL);
    EXCEPT
    | Error(msg) =>
	RETURN NEW(SupMisc.ThreadStatus,
	  status := SupMisc.ExitCode.Failure,
	  message := "Updater failed: " & msg);
    | Rd.EndOfFile =>
	RETURN NEW(SupMisc.ThreadStatus,
	  status := SupMisc.ExitCode.TransientFailure,
	  message := "Updater failed: Premature EOF from server");
    | Rd.Failure(list) =>
	RETURN NEW(SupMisc.ThreadStatus,
	  status := SupMisc.ExitCode.TransientFailure,
	  message := "Updater failed: Network read failure: "
	    & ErrMsg.StrError(list));
    | Thread.Alerted =>
	RETURN NEW(SupMisc.ThreadStatus,
	  status := SupMisc.ExitCode.Failure,
	  message := "Updater failed: Interrupted");
    | TokScan.Error(msg) =>
	RETURN NEW(SupMisc.ThreadStatus,
	  status := SupMisc.ExitCode.Failure,
	  message := "Updater failed: Protocol error: " & msg);
    END;
  END Apply;

PROCEDURE UpdateBatch(self: T;
                      isFixups: BOOLEAN)
  RAISES {Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted, TokScan.Error} =
(* Process all the collections or all the fixups. *)
  VAR
    ts: TokScan.T;
    collection, release: TEXT;
  BEGIN
    FOR i := 0 TO self.collections.size()-1 DO
      WITH sfr = self.collections.get(i) DO
	IF NOT SupFileRec.Option.Skip IN sfr.options THEN
	  ts := self.proto.getCmd(self.rd);
	  ts.getFolded("COLL");
	  collection := ts.getToken("collection");
	  release := ts.getToken("release");
	  SupFileRec.Check(sfr, collection, release);

	  IF self.userDestDir # NIL THEN
	    self.destDir := self.userDestDir;
	  ELSE  (* Construct a destDir that is a no-op. *)
	    self.destDir := "";
	  END;

	  UpdateCollection(self, sfr, isFixups);
	ELSE
	  IF NOT isFixups THEN
	    Trace(self, "Skipping collection " & sfr.collection
		  & "/" & sfr.release);
	  END;
	END;
      END;
    END;
    ts := self.proto.getCmd(self.rd);
    ts.getLiteral(".");
  END UpdateBatch;

PROCEDURE UpdateCollection(self: T;
                           sfr: SupFileRec.T;
			   isFixups: BOOLEAN)
  RAISES {Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted, TokScan.Error} =
  VAR
    ts: TokScan.T;
    cmd: TEXT;
    name: TEXT;
    cmdCh: CHAR;
    attr: FileAttr.T;
    linkTo: TEXT;
    fsType: FileStatus.Type;
    tag: TEXT;
    date: TEXT;
    revNum: TEXT;
    revDate: RCSDate.T;
    expand: RCSKeyword.ExpandMode;
    optStr: TEXT;
    cksum: TEXT;
    rcsOptions: RCSFile.Options;
    blockSize: CARDINAL;
    pos: CARDINAL;
    logLines: CARDINAL;
    fromAttic: BOOLEAN;
    needFixupTrace := isFixups;
    clearQueuedExecutes := TRUE;
  PROCEDURE AttrOrModTime(): FileAttr.T RAISES {TokScan.Error} =
    BEGIN
      IF self.proto.v.hasFileAttrs THEN
	RETURN DecodeAttr(self, ts.getToken("attributes"));
      ELSE
	RETURN NEW(FileAttr.T).init(FileAttr.FileType.File,
	  modTime := ts.getTime("modTime"));
      END;
    END AttrOrModTime;
  PROCEDURE AttrOrModTimeAndSize(): FileAttr.T RAISES {TokScan.Error} =
    BEGIN
      IF self.proto.v.hasFileAttrs THEN
	RETURN DecodeAttr(self, ts.getToken("attributes"));
      ELSE
	WITH modTime = ts.getTime("modTime"), size = ts.getInt("size") DO
	  RETURN NEW(FileAttr.T).init(FileAttr.FileType.File,
	    modTime := modTime,
	    size := size);
	END;
      END;
    END AttrOrModTimeAndSize;
  PROCEDURE DateToTime(date: RCSDate.T): Time.T RAISES {TokScan.Error} =
    BEGIN
      TRY
	RETURN RCSDate.ToTime(date);
      EXCEPT RCSError.E(msg) =>
	RAISE TokScan.Error("Invalid RCS date: " & msg);
      END;
    END DateToTime;
  BEGIN
    IF SupFileRec.Option.Compress IN sfr.options THEN
      TRY
	IF self.stats = NIL THEN
	  self.rd := NEW(GzipRd.T).init(self.wireRd, closeChild := FALSE);
	ELSE
	  (* Use reduced read sizes in order to reduce the granularity of
	     the statistics. *)
	  self.rd := NEW(GzipRd.T).init(self.wireRd, maxChildRead := 512,
	    closeChild := FALSE);
	END;
      EXCEPT OSError.E(list) =>
	RAISE Error("Cannot create Gzip reader: " & ErrMsg.StrError(list));
      END;
    END;
    TRY
      TRY
	self.statusFile := StatusFile.Open(sfr, sfr.scanTime, self.destDir,
	  readFromDestDir := isFixups);
	TRY
	  IF NOT isFixups THEN
	    Trace(self, "Updating collection " & sfr.collection
	      & "/" & sfr.release);
	  END;
	  self.dirsCreated := NEW(TextSeq.T).init(30);
	  LOOP
	    IF Thread.TestAlert() THEN
	      RAISE Thread.Alerted;
	    END;
	    self.startingWireBytes := StreamRd.ByteCount(self.wireRd);
	    self.startingCommBytes := StreamRd.ByteCount(self.rd);
	    ts := self.proto.getCmd(self.rd);
	    cmdCh := ts.getChar("command");
	    IF cmdCh = '.' THEN EXIT END;
	    IF needFixupTrace THEN
	      Trace(self, "Applying fixups for collection " & sfr.collection
		& "/" & sfr.release);
	      needFixupTrace := FALSE;
	    END;

	    (* Executes (shell commands to be executed on update
	       of certain files) arrive from the server immediately
	       before the associated file update commands.  We
	       clear the sequence of queued executes every time
	       through the command loop, except when the last
	       command was "E", which adds an execute to the
	       sequence.  Thus we can accumulate a sequence of
	       executes with successive "E" commands, and they will
	       be applied to the first subsequent non-"E" command. *)
	    IF clearQueuedExecutes THEN
	      FOR i := 1 TO self.executes.size() DO
		EVAL self.executes.remlo();
	      END;
	    END;
	    clearQueuedExecutes := TRUE;  (* Assumption for the next time. *)

	    cmd := Text.FromChar(cmdCh);
	    CASE cmdCh OF
	    | 'A', 'a' =>	(* Add file. *)
		name := ts.getToken("file name");
		IF self.proto.v.hasFileAttrs THEN
		  attr := DecodeAttr(self, ts.getToken("attributes"));
		ELSE
		  WITH modTime = ts.getTime("modTime"),
		       size = ts.getInt("size"),
		       mode = ts.getInt("mode")
		  DO
		    attr := NEW(FileAttr.T).init(FileAttr.FileType.File,
		      modTime := modTime,
		      size := size,
		      mode := mode);
		  END;
		END;
		ts.getEnd("end of \"" & cmd & "\" command");
		UpdateFile(self, sfr, name,
		  toAttic := cmdCh = 'a',
		  fup := NEW(RegularCreator.T).init(
		    attr := attr,
		    isFixup := FALSE));
	    | 'C' =>	(* Checkout file. *)
		name := ts.getToken("file name");
		tag := ts.getToken("tag");
		date := ts.getToken("date");
		revNum := ts.getToken("revision number");
		revDate := ts.getToken("revision date");
		attr := AttrOrModTime();
		ts.getEnd("end of \"" & cmd & "\" command");
		UpdateFile(self, sfr, name,
		  toAttic := FALSE,
		  fup := NEW(CheckoutCreator.T).init(
		    tag := tag,
		    date := date,
		    revNum := revNum,
		    revDate := revDate,
		    fileAttr := NEW(FileAttr.T).init(FileAttr.FileType.File,
		      modTime := DateToTime(revDate)),
		    rcsAttr := attr,
		    isFixup := FALSE));
	    | 'c' =>	(* Checkout dead file. *)
		name := ts.getToken("file name");
		tag := ts.getToken("tag");
		date := ts.getToken("date");
		attr := AttrOrModTime();
		ts.getEnd("end of \"" & cmd & "\" command");
		WITH coName = SupMisc.CheckoutName(name) DO
		  (* Theoretically, the file does not exist on the client.
		     Just to make sure, we'll delete it here, if it exists. *)
		  TRY
		    EVAL FS.Status(SupMisc.CatPath(sfr.clientPrefix, coName));
		    Delete(self, sfr, coName);
		  EXCEPT OSError.E =>
		    (* As expected, the file does not exist. *)
		  END;
		END;
		self.statusFile.put(NEW(FileStatus.T,
		  type := FileStatus.Type.CheckoutDead,
		  name := name,
		  tag := tag,
		  date := date,
		  serverAttr := attr));
	    | 'D' =>	(* Delete file. *)
		(* This will delete the live and/or dead versions of the
		   file -- whichever ones exist. *)
		name := ts.getToken("file name");
		ts.getEnd("end of \"" & cmd & "\" command");
		IF SupFileRec.Option.CheckoutMode IN sfr.options THEN
		  WITH coName = SupMisc.CheckoutName(name) DO
		    Delete(self, sfr, coName);
		  END;
		ELSE
		  Delete(self, sfr, name);
		END;
		self.statusFile.delete(name);
	    | 'E' =>  (* Save a shell command for after the next update. *)
		WITH pe = NEW(ParsedExecute) DO
		  pe.name := ts.getToken("file name");
		  pe.command := ts.getRest();
		  self.executes.addhi(pe);
		END;
		clearQueuedExecutes := FALSE;
	    | 'H', 'h' =>  (* Make hard link. *)
		name := ts.getToken("file name");
		linkTo := ts.getToken("hard link target");
		ts.getEnd("end of \"" & cmd & "\" command");
		HardLink(self, sfr, name, linkTo, toAttic := cmdCh = 'h');
	    | 'I' =>  (* Create directory. *)
		name := ts.getToken("directory name");
		ts.getEnd("End of \"" & cmd & "\" command");
		CreateDirectory(self, sfr, name);
	    | 'i' =>  (* Remove DirDown listfile entry. *)
		name := ts.getToken("directory name");
		ts.getEnd("End of \"" & cmd & "\" command");
		self.statusFile.delete(name);
	    | 'J' =>  (* Set directory attributes. *)
		name := ts.getToken("directory name");
		attr := AttrOrModTime();
		ts.getEnd("End of \"" & cmd & "\" command");
		SetDirectoryAttributes(self, sfr, name, attr);
	    | 'j' =>  (* Remove directory. *)
		name := ts.getToken("directory name");
		ts.getEnd("End of \"" & cmd & "\" command");
		RemoveDirectory(self, sfr, name);
	    | 'L', 'l' =>  (* Update recorded information for cvs file. *)
		name := ts.getToken("file name");
		attr := AttrOrModTime();
		ts.getEnd("end of \"" & cmd & "\" command");
		IF cmdCh = 'L' THEN
		  fsType := FileStatus.Type.FileLive;
		ELSE
		  fsType := FileStatus.Type.FileDead;
		END;
		self.statusFile.put(NEW(FileStatus.T,
		  type := fsType,
		  name := name,
		  clientAttr := attr,
		  serverAttr := attr));
	    | 'N', 'n' =>  (* Create a node. *)
		name := ts.getToken("file name");
		attr := AttrOrModTime();
		ts.getEnd("end of \"" & cmd & "\" command");
		UpdateNode(self, sfr, name, attr, toAttic := cmdCh = 'n');
	    | 'R' =>	(* Replace regular file. *)
		(* FIXME - Use 'A' for this, and use the 'R' command only
		   for legacy protocol support. *)
		name := ts.getToken("file name");
		attr := AttrOrModTimeAndSize();
		ts.getEnd("end of \"" & cmd & "\" command");
		UpdateFile(self, sfr, name,
		  toAttic := FALSE,
		  fup := NEW(RegularUpdater.T).init(
		    pos := 0,
		    attr := attr));
	    | 'r' =>	(* Update regular file with rsync algorithm. *)
		name := ts.getToken("file name");
		attr := AttrOrModTime();
		blockSize := ts.getInt("blockSize");
		cksum := ts.getToken("checksum");
		ts.getEnd("end of \"" & cmd & "\" command");
		UpdateFile(self, sfr, name,
		  toAttic := FALSE,
		  fup := NEW(RsyncUpdater.T).init(
		    blockSize := blockSize,
		    attr := attr,
		    wantSum := cksum));
	    | 'T' =>	(* Update recorded information for checked-out file. *)
		name := ts.getToken("file name");
		tag := ts.getToken("tag");
		date := ts.getToken("date");
		revNum := ts.getToken("revision number");
		IF self.proto.v.sendsRevDates THEN
		  revDate := ts.getToken("revision date");
		ELSE
		  revDate := ".";
		END;
		attr := AttrOrModTime();
		ts.getEnd("end of \"" & cmd & "\" command");
		UpdateCheckoutInfo(self, sfr, name,
		  tag := tag,
		  date := date,
		  revNum := revNum,
		  revDate := revDate,
		  rcsAttr := attr);
	    | 'U' =>	(* Update live checked-out file. *)
		name := ts.getToken("file name");
		tag := ts.getToken("tag");
		date := ts.getToken("date");
		revNum := ts.getToken("old revision number");
		fromAttic := NOT Text.Equal(ts.getToken("Attic flag"), "0");
		logLines := ts.getInt("log deletion count");
		WITH text = ts.getToken("expansion mode") DO
		  TRY
		    expand := RCSKeyword.DecodeExpand(text);
		  EXCEPT RCSError.E(msg) =>
		    RAISE TokScan.Error(msg);
		  END;
		END;
		attr := AttrOrModTime();
		cksum := ts.getToken("checksum");
		ts.getEnd("end of \"" & cmd & "\" command");
		UpdateFile(self, sfr, name,
		  toAttic := FALSE,
		  fup := NEW(CheckoutUpdater.T).init(
		    tag := tag,
		    date := date,
		    oldRevNum := revNum,
		    oldLogLines := logLines,
		    fromAttic := fromAttic,
		    expand := expand,
		    rcsAttr := attr,
		    wantSum := cksum));
	    | 'u' =>	(* Update dead checked-out file. *)
		name := ts.getToken("file name");
		tag := ts.getToken("tag");
		date := ts.getToken("date");
		attr := AttrOrModTime();
		ts.getEnd("end of \"" & cmd & "\" command");
		WITH coName = SupMisc.CheckoutName(name) DO
		  Delete(self, sfr, coName);
		END;
		self.statusFile.put(NEW(FileStatus.T,
		  type := FileStatus.Type.CheckoutDead,
		  name := name,
		  tag := tag,
		  date := date,
		  serverAttr := attr));
	    | 'V', 'v' =>	(* Edit RCS file. *)
		name := ts.getToken("file name");
		attr := AttrOrModTime();
		optStr := ts.getToken("RCS file options");
		cksum := ts.getToken("checksum");
		ts.getEnd("end of \"" & cmd & "\" command");
		TRY
		  rcsOptions := RCSFile.DecodeOptions(optStr);
		EXCEPT RCSError.E =>
		  RAISE TokScan.Error("Invalid RCS file options \"" &
		    optStr & "\"");
		END;
		UpdateFile(self, sfr, name,
		  toAttic := cmdCh = 'v',
		  fup := NEW(RCSUpdater.T).init(
		    attr := attr,
		    rcsOptions := rcsOptions,
		    wantSum := cksum));
	    | 'X', 'x' =>	(* Receive RCS file fixup. *)
		name := ts.getToken("file name");
		attr := AttrOrModTimeAndSize();
		ts.getEnd("end of \"" & cmd & "\" command");
		UpdateFile(self, sfr, name,
		  toAttic := cmdCh = 'x',
		  fup := NEW(RegularCreator.T).init(
		    attr := attr,
		    isFixup := TRUE),
		  isFixup := TRUE);
	    | 'Y' =>	(* Receive checkout-mode fixup. *)
		name := ts.getToken("file name");
		tag := ts.getToken("tag");
		date := ts.getToken("date");
		revNum := ts.getToken("revision number");
		revDate := ts.getToken("revision date");
		attr := AttrOrModTime();
		ts.getEnd("end of \"" & cmd & "\" command");
		UpdateFile(self, sfr, name,
		  toAttic := FALSE,
		  fup := NEW(CheckoutCreator.T).init(
		    tag := tag,
		    date := date,
		    revNum := revNum,
		    revDate := revDate,
		    fileAttr := NEW(FileAttr.T).init(FileAttr.FileType.File,
		      modTime := DateToTime(revDate)),
		    rcsAttr := attr,
		    isFixup := TRUE),
		  isFixup := TRUE);
	    | 'Z' =>	(* Append to regular file. *)
		name := ts.getToken("file name");
		IF self.proto.v.hasFileAttrs THEN
		  attr := DecodeAttr(self, ts.getToken("attributes"));
		  pos := ts.getInt("pos");
		ELSE
		  WITH modTime = ts.getTime("RCS file modTime"),
		       nbytes = ts.getInt("nbytes")
		  DO
		    pos := ts.getInt("pos");
		    attr := NEW(FileAttr.T).init(FileAttr.FileType.File,
		      modTime := modTime,
		      size := pos + nbytes);
		  END;
		END;
		ts.getEnd("end of \"" & cmd & "\" command");
		UpdateFile(self, sfr, name,
		  toAttic := FALSE,
		  fup := NEW(RegularUpdater.T).init(
		    pos := pos,
		    attr := attr));
	    | '!' =>	(* Warning from server. *)
		Warn(self, "Server warning: " & ts.getRest());
	    ELSE
	      RAISE TokScan.Error("Invalid command \"" & cmd & "\"");
	    END;
	  END;
	FINALLY
	  self.statusFile.close();
	END;
      EXCEPT FileStatus.Error(msg) =>
	RAISE Error(msg);
      END;
      IF SupFileRec.Option.Compress IN sfr.options THEN
	IF NOT Rd.EOF(self.rd) THEN
	  RAISE TokScan.Error(
	    "Expected EOF from compressed stream, didn't get it");
	END;
	Rd.Close(self.rd);
      END;
    FINALLY
      IF SupFileRec.Option.Compress IN sfr.options THEN
	GzipRd.Cleanup(self.rd);
	self.rd := self.wireRd;
      END;
    END;
  END UpdateCollection;

PROCEDURE DecodeAttr(self: T; t: TEXT): FileAttr.T
  RAISES {TokScan.Error} =
  BEGIN
    LOOP
      TRY
	RETURN FileAttr.Decode(t);
      EXCEPT
      | FileAttr.UnknownGroup(name) =>
	  Warn(self, "Unknown group name \"" & name
	    & "\" received from server");
      | FileAttr.UnknownOwner(name) =>
	  Warn(self, "Unknown user name \"" & name
	    & "\" received from server");
      END;
    END;
  END DecodeAttr;

PROCEDURE UpdateFile(self: T;
                     sfr: SupFileRec.T;
		     name: Pathname.T;
		     toAttic: BOOLEAN;
		     fup: FileUpdater.T;
		     isFixup := FALSE)
  RAISES {Error, FileStatus.Error, Rd.EndOfFile, Rd.Failure,
	  Thread.Alerted, TokScan.Error} =
  VAR
    destName := name;
    destPath, atticPath, finalPath, tempPath: Pathname.T;
    md5: MD5.T;
    wr: MD5Wr.T;
    size: CARDINAL;
    status: FileUpdater.Status;
  BEGIN
    IF SupFileRec.Option.CheckoutMode IN sfr.options THEN
      destName := SupMisc.CheckoutName(name);
    END;
    destPath := SupMisc.CatPath(self.destDir,
      SupMisc.CatPath(sfr.clientPrefix, destName));
    atticPath := SupMisc.AtticName(destPath);
    finalPath := destPath;
    IF toAttic THEN finalPath := atticPath END;

    tempPath := SupMisc.TempName(destPath);
    MakeDirectories(tempPath, sfr.umask);

    IF fup.isRCS()
    AND NOT SupFileRec.Option.StrictCheckRCS IN sfr.options
    AND self.proto.v.hasLooseRCSCheck THEN
      md5 := MD5.NewRCS();
    ELSE
      md5 := MD5.New();
    END;

    TRY
      TRY
	wr := NEW(MD5Wr.T).init(
	  wr := NEW(FileWr.T).init(FS.OpenFile(tempPath,
	    access := FS.AccessOption.OnlyOwnerCanRead)),
	  md5 := md5);
	TRY
	  TempFiles.Note(tempPath);
	  fup.update(
	    sfr := sfr,
	    name := name,
	    toAttic := toAttic,
	    proto := self.proto,
	    trace := self.trace,
	    protoRd := self.rd,
	    wr := wr,
	    status := status);
	  size := Wr.Index(wr);
	FINALLY
	  Wr.Close(wr);
	END;
      EXCEPT
      | FileUpdater.Error(msg) =>
	  RAISE Error(destName & ": " & msg);
      | OSError.E(l) =>
	  RAISE Error(tempPath & ": Cannot create: " & ErrMsg.StrError(l));
      | Wr.Failure(l) =>
	  RAISE Error(tempPath & ": Write failure: " & ErrMsg.StrError(l));
      END;

      IF status.wantSum # NIL
      AND NOT Text.Equal(wr.getSignature(), status.wantSum) THEN
	RAISE FileUpdater.FixupNeeded("Checksum mismatch");
      END;

      IF status.fromAttic THEN
	MakeDirectories(atticPath, sfr.umask);
	destPath := atticPath;
      ELSIF toAttic THEN
	MakeDirectories(atticPath, sfr.umask);
      END;

      TempFiles.Forget(tempPath);
      status.fs.clientAttr := FileAttr.Umask(status.fs.clientAttr, sfr.umask);
      Install(status.fs.clientAttr,
	from := tempPath, via := destPath, to := finalPath);

      DoQueuedExecutes(self, sfr);

      (* We weren't necessarily able to set all the file attributes to the
	 desired values, and any executes may have altered the attributes.
	 To make sure we record the actual attribute values, we fetch
	 them from the file.
       
	 However, we preserve the link count as received from the
	 server.  This is important for preserving hard links in mirror
	 mode. *)
      TRY
        status.fs.clientAttr := FileAttr.Override(
          FileAttr.FromPathname(finalPath, follow := FALSE),
          status.fs.clientAttr,
          FileAttr.AttrTypes{FileAttr.AttrType.LinkCount});
      EXCEPT OSError.E(l) =>
	RAISE Error("Cannot stat \"" & finalPath & "\": " & ErrMsg.StrError(l));
	(* FIXME - This could happen if the execute deleted the file.  Maybe
	   we should simply delete its entry from the status file. *)
      END;

      (* To save space, don't write out the device and inode unless
	 the link count is greater than 1.  These attributes are used
	 only for detecting hard links.  If the link count is 1 then we
	 know there aren't any hard links. *)
      IF NOT FileAttr.AttrType.LinkCount IN
        FileAttr.GetMask(status.fs.clientAttr)
      OR FileAttr.GetLinkCount(status.fs.clientAttr) <= 1 THEN
	status.fs.clientAttr :=
	  FileAttr.MaskOut(status.fs.clientAttr, DevInodeAttrs);
      END;

      (* In checkout mode, limit the attributes that we keep around. *)
      IF SupFileRec.Option.CheckoutMode IN sfr.options THEN
	status.fs.clientAttr :=
	  FileAttr.MaskOut(status.fs.clientAttr, FileAttr.CheckoutIgnore);
      END;

      self.statusFile.put(status.fs);

      IF status.modified THEN
	StatsUpdate(self, status.updateType, size);
      ELSE
	StatsUpdate(self, FileUpdater.UpdateType.Touch, size);
      END;
    EXCEPT FileUpdater.FixupNeeded(msg) =>
      IF isFixup THEN
	Warn(self, destName & ": " & msg & " -- file not updated");
      ELSE
	Warn(self, destName & ": " & msg & " -- will transfer entire file");
	self.fixups.put(NEW(Fixup.T, sfr := sfr, name := name));
      END;
      TempFiles.Forget(tempPath);
      IF SupFileRec.Option.KeepBadFiles IN sfr.options THEN
	Warn(self, "Bad version saved in " & tempPath);
      ELSE
	DeleteFile(self, tempPath);
      END;
    END;
  END UpdateFile;

PROCEDURE UpdateNode(self: T;
                     sfr: SupFileRec.T;
		     name: Pathname.T;
		     attr: FileAttr.T;
		     toAttic: BOOLEAN)
  RAISES {Error, FileStatus.Error, Thread.Alerted} =
  VAR
    srcPath := SupMisc.CatPath(sfr.clientPrefix, name);
    origSrcPath := srcPath;
    destPath, atticPath, finalPath, tempPath: Pathname.T;
    fs: FileStatus.T;
    what: TEXT;
  BEGIN
    (* We don't bother mentioning the Attic here. *)
    IF attr.fileType = FileAttr.FileType.SymLink THEN
      Trace(self, " Symlink " & name & " -> " & FileAttr.GetLinkTarget(attr));
      what := "symbolic link";
    ELSE
      Trace(self, " Mknod " & name);
      what := "node";
    END;

    destPath := SupMisc.CatPath(self.destDir,
      SupMisc.CatPath(sfr.clientPrefix, name));
    atticPath := SupMisc.AtticName(destPath);
    finalPath := destPath;
    IF toAttic THEN finalPath := atticPath END;

    tempPath := SupMisc.TempName(destPath);
    MakeDirectories(tempPath, sfr.umask);

    TRY
      FileAttr.MakeNode(attr, tempPath);
      TempFiles.Note(tempPath);
    EXCEPT OSError.E(l) =>
      RAISE Error(tempPath & ": Cannot make " & what
	& ": " & ErrMsg.StrError(l));
    END;

    IF srcPath # origSrcPath THEN  (* Coming from the Attic. *)
      MakeDirectories(atticPath, sfr.umask);
      destPath := atticPath;
    ELSIF toAttic THEN
      MakeDirectories(atticPath, sfr.umask);
    END;

    TempFiles.Forget(tempPath);
    attr := FileAttr.Umask(attr, sfr.umask);
    Install(attr,
      from := tempPath, via := destPath, to := finalPath);

    DoQueuedExecutes(self, sfr);

    (* We weren't necessarily able to set all the file attributes to the
       desired values, and any executes may have altered the attributes.
       To make sure we record the actual attribute values, we fetch
       them from the file.
       
       However, we preserve the link count as received from the
       server.  This is important for preserving hard links in mirror
       mode. *)
    TRY
      attr := FileAttr.Override(
        FileAttr.FromPathname(finalPath, follow := FALSE), attr,
        FileAttr.AttrTypes{FileAttr.AttrType.LinkCount});
    EXCEPT OSError.E(l) =>
      RAISE Error("Cannot stat \"" & finalPath & "\": " & ErrMsg.StrError(l));
      (* FIXME - This could happen if the execute deleted the file.  Maybe
	 we should simply delete its entry from the status file. *)
    END;

    (* To save space, don't write out the device and inode unless
       the link count is greater than 1.  These attributes are used
       only for detecting hard links.  If the link count is 1 then we
       know there aren't any hard links. *)
    IF NOT FileAttr.AttrType.LinkCount IN FileAttr.GetMask(attr)
    OR FileAttr.GetLinkCount(attr) <= 1 THEN
      attr := FileAttr.MaskOut(attr, DevInodeAttrs);
    END;

    fs := NEW(FileStatus.T,
      name := name,
      clientAttr := attr,
      serverAttr := attr);
    IF toAttic THEN
      fs.type := FileStatus.Type.FileDead;
    ELSE
      fs.type := FileStatus.Type.FileLive;
    END;
    self.statusFile.put(fs);

    StatsUpdate(self, FileUpdater.UpdateType.Other, 0);
  END UpdateNode;

PROCEDURE UpdateCheckoutInfo(self: T;
                             sfr: SupFileRec.T;
			     name: Pathname.T;
			     tag: TEXT;
			     date: RCSDate.T;
			     revNum: RCSRevNum.T;
			     revDate: RCSDate.T;
			     rcsAttr: FileAttr.T)
  RAISES {Error, FileStatus.Error, Thread.Alerted} =
  VAR
    path := SupMisc.CatPath(sfr.clientPrefix,
      SupMisc.CheckoutName(name));
    destPath := SupMisc.CatPath(self.destDir, path);
    fileAttr: FileAttr.T;
  BEGIN
    TRY
      fileAttr := FileAttr.FromPathname(path, follow := FALSE);
      fileAttr := FileAttr.Override(fileAttr,
	FileAttr.ForCheckout(rcsAttr, sfr.umask));
      TRY
	IF FileAttr.Install(fileAttr, destPath) THEN
	  Trace(self, " SetAttrs " & name);
	  fileAttr := FileAttr.FromPathname(destPath, follow := FALSE);
	END;
      EXCEPT OSError.E(l) =>
	Trace(self, " SetAttrs " & name);
	(* Ignore it if a different destination directory was specified
	   and the file doesn't exist. *)
	IF l.head # EnoentAtom OR Text.Equal(path, destPath) THEN
	  RAISE Error("Cannot set attributes for \"" & destPath & "\": " &
	    ErrMsg.StrError(l));
	END;
      END;
      fileAttr := FileAttr.MaskOut(fileAttr, FileAttr.CheckoutIgnore);
      self.statusFile.put(NEW(FileStatus.T,
	type := FileStatus.Type.CheckoutLive,
	name := name,
	tag := tag,
	date := date,
	serverAttr := rcsAttr,
	revNum := revNum,
	revDate := revDate,
	clientAttr := fileAttr));
    EXCEPT OSError.E => (* The file has vanished. *)
      self.statusFile.delete(name);
    END;
  END UpdateCheckoutInfo;

PROCEDURE HardLink(self: T;
                   sfr: SupFileRec.T;
		   name: Pathname.T;
		   linkTo: Pathname.T;
		   toAttic: BOOLEAN)
  RAISES {Error, FileStatus.Error, Thread.Alerted} =
  VAR
    srcPath := SupMisc.CatPath(sfr.clientPrefix, name);
    origSrcPath := srcPath;
    targetPath := SupMisc.CatPath(sfr.clientPrefix, linkTo);
    destPath, atticPath, finalPath, tempPath: Pathname.T;
    fromAttic := FALSE;
    idA, idB: FileID.T;
    attr: FileAttr.T;
    fs: FileStatus.T;
  BEGIN
    (* See if the link already exists in the right place. *)
    TRY
      idA :=
	FileID.FromAttr(Attic.FileAttrFromPathname(srcPath, follow := FALSE));
      idB :=
	FileID.FromAttr(FileAttr.FromPathname(targetPath, follow := FALSE));
      fromAttic := srcPath # origSrcPath;
      IF fromAttic = toAttic AND idA # NIL AND idB # NIL
      AND FileID.Equal(idA, idB) THEN
	RETURN;  (* Already linked. *)
      END;
    EXCEPT OSError.E => (* Ignore. *) END;

    (* We won't bother to mention the Attic here. *)
    Trace(self, " Hardlink " & name & " -> " & linkTo);

    destPath := SupMisc.CatPath(self.destDir,
      SupMisc.CatPath(sfr.clientPrefix, name));
    atticPath := SupMisc.AtticName(destPath);
    finalPath := destPath;
    IF toAttic THEN finalPath := atticPath END;

    tempPath := SupMisc.TempName(destPath);
    MakeDirectories(tempPath, sfr.umask);

    TRY
      attr := FileAttr.HardLink(path := tempPath, target := targetPath);
      TempFiles.Note(tempPath);
    EXCEPT OSError.E(l) =>
      RAISE Error(tempPath & ": Cannot make node: " & ErrMsg.StrError(l));
    END;

    IF fromAttic THEN
      MakeDirectories(atticPath, sfr.umask);
      destPath := atticPath;
    ELSIF toAttic THEN
      MakeDirectories(atticPath, sfr.umask);
    END;

    TempFiles.Forget(tempPath);
    attr := FileAttr.Umask(attr, sfr.umask);
    Install(attr,
      from := tempPath, via := destPath, to := finalPath);

    DoQueuedExecutes(self, sfr);

    (* We weren't necessarily able to set all the file attributes to the
       desired values, and any executes may have altered the attributes.
       To make sure we record the actual attribute values, we fetch
       them from the file.
       
       However, we preserve the link count as received from the
       server.  This is important for preserving hard links in mirror
       mode. *)
    TRY
      attr := FileAttr.Override(
        FileAttr.FromPathname(finalPath, follow := FALSE), attr,
        FileAttr.AttrTypes{FileAttr.AttrType.LinkCount});
    EXCEPT OSError.E(l) =>
      RAISE Error("Cannot stat \"" & finalPath & "\": " & ErrMsg.StrError(l));
      (* FIXME - This could happen if the execute deleted the file.  Maybe
	 we should simply delete its entry from the status file. *)
    END;

    fs := NEW(FileStatus.T,
      name := name,
      clientAttr := attr,
      serverAttr := attr);
    IF toAttic THEN
      fs.type := FileStatus.Type.FileDead;
    ELSE
      fs.type := FileStatus.Type.FileLive;
    END;
    self.statusFile.put(fs);

    StatsUpdate(self, FileUpdater.UpdateType.Other, 0);
  END HardLink;

PROCEDURE CreateDirectory(self: T;
                          sfr: SupFileRec.T;
		          name: Pathname.T)
  RAISES {Error, FileStatus.Error, Thread.Alerted} =
  VAR
    path := SupMisc.CatPath(sfr.clientPrefix, name);
    destPath := SupMisc.CatPath(self.destDir, path);
    attr := NEW(FileAttr.T).init(FileAttr.FileType.Directory);
  BEGIN
    (* If we are not going to be setting the modes of the directory
       later, then create it with reasonable default modes.  Otherwise,
       let MakeNode create it with more restrictive modes initially. *)
    IF NOT SupFileRec.Option.SetMode IN sfr.options THEN
      attr := FileAttr.MergeDefault(attr);
    END;
    IF NOT Text.Equal(path, destPath) THEN  (* Testing -> separate destDir. *)
      MakeDirectories(destPath, sfr.umask);
    END;
    TRY
      FileAttr.MakeNode(attr, destPath);
      Trace(self, " Mkdir " & name);
      (* Push the directory name onto the stack so that we will be able
	 to find out that we created it when the matching
	 SetDirectoryAttributes call comes along. *)
      self.dirsCreated.addhi(name);
    EXCEPT OSError.E(l) =>
      IF l.head # EexistAtom THEN
	Trace(self, " Mkdir " & name);
	RAISE Error("Cannot create directory \"" & destPath & "\": " &
	  ErrMsg.StrError(l));
      END;
    END;

    self.statusFile.put(NEW(FileStatus.T,
      name := name,
      type := FileStatus.Type.DirDown));

    StatsUpdate(self, FileUpdater.UpdateType.Other, 0);
  END CreateDirectory;

PROCEDURE SetDirectoryAttributes(self: T;
                                 sfr: SupFileRec.T;
		                 name: Pathname.T;
				 attr: FileAttr.T)
  RAISES {Error, FileStatus.Error, Thread.Alerted} =
  VAR
    path := SupMisc.CatPath(sfr.clientPrefix, name);
    destPath := SupMisc.CatPath(self.destDir, path);
    changed := FALSE;
  BEGIN
    IF self.dirsCreated.size() > 0
    AND Text.Equal(name, self.dirsCreated.gethi()) THEN  (* We created it. *)
      changed := TRUE;
      EVAL self.dirsCreated.remhi();  (* Pop it from the stack. *)
    END;

    attr := FileAttr.MergeDefault(attr);
    attr := FileAttr.Umask(attr, sfr.umask);

    TRY
      IF FileAttr.Install(attr, destPath) THEN
	Trace(self, " SetAttrs " & name);
	changed := TRUE;
      END;
    EXCEPT OSError.E(l) =>
      Trace(self, " SetAttrs " & name);
      (* Ignore it if a different destination directory was specified
	 and the file doesn't exist. *)
      IF l.head # EnoentAtom OR Text.Equal(path, destPath) THEN
	RAISE Error("Cannot set attributes for \"" & destPath & "\": " &
	  ErrMsg.StrError(l));
      END;
    END;

    IF changed THEN  (* We changed the attributes and/or created it. *)
      DoQueuedExecutes(self, sfr);
    END;

    (* We weren't necessarily able to set all the directory attributes
       to the desired values, and any executes may have altered the
       attributes.  To make sure we record the actual attribute
       values, we fetch them from the directory. *)
    TRY
      attr := FileAttr.FromPathname(destPath, follow := FALSE);
    EXCEPT OSError.E(l) =>
      RAISE Error("Cannot stat \"" & destPath & "\": " & ErrMsg.StrError(l));
      (* FIXME - This could happen if the execute deleted the directory.
	 Maybe we should simply delete its entry from the status file. *)
    END;

    self.statusFile.put(NEW(FileStatus.T,
      name := name,
      type := FileStatus.Type.DirUp,
      clientAttr := attr,
      serverAttr := attr));

    StatsUpdate(self, FileUpdater.UpdateType.Other, 0);
  END SetDirectoryAttributes;

PROCEDURE RemoveDirectory(self: T;
                          sfr: SupFileRec.T;
			  name: Pathname.T)
  RAISES {FileStatus.Error, Thread.Alerted} =
  VAR
    path := SupMisc.CatPath(sfr.clientPrefix, name);
    destPath := SupMisc.CatPath(self.destDir, path);
  BEGIN
    Trace(self, " Rmdir " & name);
    IF Text.Equal(path, destPath) THEN
      TRY
	FS.DeleteDirectory(destPath);
      EXCEPT OSError.E(l) =>
	IF l.head # EnoentAtom THEN
	  Warn(self, "Cannot remove directory \"" & destPath & "\": " &
	    ErrMsg.StrError(l));
	END;
      END;
    END;
    self.statusFile.delete(name, isDirUp := TRUE);

    StatsUpdate(self, FileUpdater.UpdateType.Other, 0);
  END RemoveDirectory;

(*****************************************************************************)

PROCEDURE DoQueuedExecutes(self: T;
                           sfr: SupFileRec.T)
  RAISES {Error} =
  BEGIN
    FOR i := 1 TO self.executes.size() DO
      WITH pe = NARROW(self.executes.remlo(), ParsedExecute) DO
	ExecCommand(self, sfr, pe.name, pe.command);
      END;
    END;
  END DoQueuedExecutes;

PROCEDURE ExecCommand(self: T;
                      sfr: SupFileRec.T;
		      name: Pathname.T;
		      command: TEXT)
  RAISES {Error} =
  VAR
    path := SupMisc.CatPath(sfr.clientPrefix, name);
    pos := 0;
    len := Text.Length(command);
    expCmd := "";
    pctPos: INTEGER;
    stdin, stdout, stderr: File.T;
    child: Process.T;
    status: Process.ExitCode;
    modTime: Time.T;
  BEGIN
    (* Expand "%" escapes. *)
    WHILE pos < len DO
      pctPos := Text.FindChar(command, '%', pos);
      IF pctPos = -1 THEN pctPos := len END;
      expCmd := expCmd & Text.Sub(command, pos, pctPos-pos);
      pos := pctPos;
      IF pos < len THEN  (* Found a "%". *)
	IF pos+1 < len THEN
	  WITH ch = Text.GetChar(command, pos+1) DO
	    IF ch = 's' THEN
	      expCmd := expCmd & path;
	    ELSIF ch = '%' THEN
	      expCmd := expCmd & "%";
	    ELSE
	      expCmd := expCmd & "%" & Text.FromChar(ch);
	    END;
	    INC(pos, 2);
	  END;
	ELSE
	  expCmd := expCmd & "%";
	  INC(pos);
	END;
      END;
    END;

    IF SupFileRec.Option.Execute IN sfr.options THEN
      Trace(self, " Execute " & expCmd);
      TRY
	modTime := FS.Status(path).modificationTime;
      EXCEPT OSError.E => modTime := -1.0d0 END;
      Process.GetStandardFileHandles(stdin, stdout, stderr);
      TRY
	child := Process.Create(
	  cmd := SupMisc.ShellPath,
	  params := ARRAY [0..1] OF TEXT{"-c", expCmd},
	  stdin := stdin, stdout := stdout, stderr := stderr);
      EXCEPT OSError.E(l) =>
	RAISE Error("Cannot execute command: " & ErrMsg.StrError(l));
      END;
      status := Process.Wait(child);
      (* FIXME - This is Unix specific. *)
      WITH sig = Word.Extract(status, 0, 7),
	code = Word.Extract(status, 8, 8)
      DO
	IF sig # 0 THEN
	  Trace(self, " [signal " & Fmt.Int(sig) & "]");
	ELSIF code # 0 THEN
	  Trace(self, " [exit code " & Fmt.Int(code) & "]");
	END;
      END;
      IF modTime # -1.0d0 THEN
	TRY
	  FS.SetModificationTime(path, modTime)
	EXCEPT OSError.E => (* Ignore *) END;
      END;
    ELSE
      Trace(self, " NoExecute " & expCmd);
    END;
  END ExecCommand;

(*****************************************************************************)

PROCEDURE Init(self: T;
	       proto: CVProto.T;
	       rd: StreamRd.T;
	       collections: SupFileRecSeq.T;
	       fixups: SyncFixupQueue.T;
	       deleteLimit := -1;
	       reaper: Reaper.T := NIL;
	       destDir: TEXT := NIL;
               stats: Stats := NIL;
	       trace: Logger.T := NIL): T =
  BEGIN
    self.proto := proto;
    self.userDestDir := destDir;
    self.wireRd := rd;
    self.rd := rd;
    self.collections := collections;
    self.executes := NEW(RefSeq.T).init();
    self.fixups := fixups;
    self.deleteLimit := deleteLimit;
    self.reaper := reaper;
    self.stats := stats;
    self.trace := trace;
    RETURN self;
  END Init;

PROCEDURE Delete(self: T;
                 sfr: SupFileRec.T;
		 name: Pathname.T)
  RAISES {Error} =
  VAR
    srcPath := SupMisc.CatPath(sfr.clientPrefix, name);
    destPath := SupMisc.CatPath(self.destDir, srcPath);
  BEGIN
    IF SupFileRec.Option.Delete IN sfr.options
    AND Text.Equal(destPath, srcPath) THEN
      Trace(self, " Delete " & name);
      IF self.deleteLimit >= 0 AND self.deleteCount >= self.deleteLimit THEN
	RAISE Error("File deletion limit exceeded");
      END;
      INC(self.deleteCount);
      IF NOT SupFileRec.Option.CheckoutMode IN sfr.options THEN
	(* Try the attic. *)
	WITH atticName = SupMisc.AtticName(destPath) DO
	  DeleteFile(self, atticName);
	  (* We always delete the Attic directory when it becomes empty.
	     FIXME - Is that the right thing to do?. *)
	  TRY
	    FileAttr.Delete(SupMisc.PathPrefix(atticName));
	  EXCEPT OSError.E => (* Ignore. *) END;
	END;
      END;
      DeleteFile(self, destPath);
      IF SupFileRec.Option.CheckoutMode IN sfr.options
      OR NOT self.proto.v.dirsAreExplicit THEN
	(* Delete the directory automatically if it is now empty. *)
	DeleteAncestorDirectories(self, sfr, name);
      END;
    ELSE
      Trace(self, " NoDelete " & name);
    END;
    StatsUpdate(self, FileUpdater.UpdateType.Delete, 0);
  END Delete;

PROCEDURE DeleteAncestorDirectories(<*UNUSED*> self: T;
                                               sfr: SupFileRec.T;
			                       name: Pathname.T) =
(* Delete the ancestor directories of "name" up to the first one that
   can't be deleted, but stop at the client prefix. *)
  VAR
    path := SupMisc.CatPath(sfr.clientPrefix, name);
    prefixLen := Text.Length(sfr.clientPrefix);
  BEGIN
    TRY
      LOOP
	path := SupMisc.PathPrefix(path);
	IF Text.Length(path) <= prefixLen THEN EXIT END;
	FS.DeleteDirectory(path);
      END;
    EXCEPT OSError.E => (* Break out of the loop. *) END;
  END DeleteAncestorDirectories;

PROCEDURE MakeDirectories(path: Pathname.T; umask: INTEGER)
  RAISES {Error} =
  BEGIN
    TRY
      SupMisc.MakeDirectories(path, umask);
    EXCEPT OSError.E(list) =>
      RAISE Error("Cannot create directories leading to \"" & path
	& "\": " & ErrMsg.StrError(list));
    END;
  END MakeDirectories;

PROCEDURE DeleteFile(self: T; path: Pathname.T) =
  BEGIN
    TRY
      FileAttr.Delete(path);
    EXCEPT OSError.E(l) =>
      IF l.head # EnoentAtom THEN
	Warn(self, "Cannot delete \"" & path & "\": " &
	  ErrMsg.StrError(l));
      END;
    END;
  END DeleteFile;

PROCEDURE Install(attr: FileAttr.T;
                  from, via, to: Pathname.T)
  RAISES {Error} =
  BEGIN
    TRY
      EVAL FileAttr.Install(attr, from := from, to := via);
    EXCEPT OSError.E(list) =>
      RAISE Error("Cannot install \"" & from & "\" to \""
	& via & "\": " & ErrMsg.StrError(list));
    END;
    IF NOT Text.Equal(via, to) THEN  (* Move file to or from the Attic. *)
      TRY
	WITH noAttr = FileAttr.MaskOut(attr, FileAttr.AllButFileType) DO
	  EVAL FileAttr.Install(noAttr, from := via, to := to);
	END;
      EXCEPT OSError.E(list) =>
	RAISE Error("Cannot move \"" & via & "\" to \""
	  & to & "\": " & ErrMsg.StrError(list));
      END;
    END;
  END Install;

(*****************************************************************************)

REVEAL
  Stats = StatsPublic BRANDED OBJECT
  OVERRIDES
    init := StatsInit;
    start := StatsDoNothing0;
    update := StatsDoNothing1;
    finish := StatsDoNothing0;
  END;

PROCEDURE StatsInit(self: Stats): Stats =
  BEGIN
    RETURN self;
  END StatsInit;

PROCEDURE StatsDoNothing0(<*UNUSED*> self: Stats) =
  BEGIN
  END StatsDoNothing0;

PROCEDURE StatsDoNothing1(<*UNUSED*> self: Stats;
                          <*UNUSED*> type: FileUpdater.UpdateType) =
  BEGIN
  END StatsDoNothing1;

PROCEDURE StatsUpdate(self: T;
                      updateType: FileUpdater.UpdateType;
		      fileBytes: CARDINAL) =
  VAR
    commBytes, wireBytes: LONGREAL;
  BEGIN
    IF self.stats # NIL THEN
      wireBytes := StreamRd.ByteCount(self.wireRd) - self.startingWireBytes;
      commBytes := StreamRd.ByteCount(self.rd) - self.startingCommBytes;
      LOCK self.stats DO
	WITH info = self.stats.updateInfo[updateType] DO
	  INC(info.fileCount);
	  info.fileBytes := info.fileBytes + FLOAT(fileBytes, LONGREAL);
	  info.wireBytes := info.wireBytes + wireBytes;
	  info.commBytes := info.commBytes + commBytes;
	END;
	WITH info = self.stats.totals DO
	  INC(info.fileCount);
	  info.fileBytes := info.fileBytes + FLOAT(fileBytes, LONGREAL);
	  info.wireBytes := info.wireBytes + wireBytes;
	  info.commBytes := info.commBytes + commBytes;
	END;
      END;
      self.stats.update(updateType);
    END;
  END StatsUpdate;

(*****************************************************************************)

PROCEDURE Warn(self: T; msg: TEXT) =
  BEGIN
    IF self.trace # NIL THEN
      Logger.Put(self.trace, Logger.Priority.Warning, msg);
    END;
  END Warn;

PROCEDURE Trace(self: T; msg: TEXT) =
  BEGIN
    IF self.trace # NIL THEN
      Logger.Put(self.trace, Logger.Priority.Notice, msg);
    END;
  END Trace;

BEGIN
END Updater.
