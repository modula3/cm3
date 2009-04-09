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
 * $Id: Detailer.m3,v 1.1.1.1 2009-04-09 17:01:37 jkrell Exp $ *)

MODULE Detailer;

IMPORT
  Attic, CVProto, ErrMsg, FileAttr, FileRd, FileStatus, Fixup, Fmt,
  GzipRd, GzipWr, Logger, MD5, MD5Digest, OSError, Pathname, RCSDelta,
  RCSDeltaTbl, RCSError, RCSFile, RCSKeyword, RCSRevNum, RCSTag, Rd,
  Reaper, RsyncBlock, RsyncFile, StatusFile, StreamRd, StreamWr,
  SupFileRec, SupFileRecSeq, SupMisc, SyncFixupQueue, Text, Thread,
  TokScan, Wr;

EXCEPTION Error(TEXT);

REVEAL
  T = Public BRANDED OBJECT
    proto: CVProto.T;
    wireRd: StreamRd.T;		(* Raw reader. *)
    rd: StreamRd.T;		(* Currently active reader. *)
    wireWr: StreamWr.T;		(* Raw writer. *)
    wr: StreamWr.T;		(* Currently active writer. *)
    collections: SupFileRecSeq.T;
    fixups: SyncFixupQueue.T;
    compLevel: [0..9];
    reaper: Reaper.T;
    stats: Stats;
    logger: Logger.T;
    statusFile: StatusFile.T;
  OVERRIDES
    apply := Apply;
    init := Init;
  END;

PROCEDURE Apply(self: T): REFANY =
  VAR
    ts: TokScan.T;
    collection, release: TEXT;
  BEGIN
    TRY
      IF self.stats # NIL THEN
	self.stats.start();
      END;
      TRY
	FOR i := 0 TO self.collections.size()-1 DO
	  WITH sfr = self.collections.get(i) DO
	    IF NOT SupFileRec.Option.Skip IN sfr.options THEN
	      ts := self.proto.getCmd(self.rd);
	      ts.getFolded("COLL");
	      collection := ts.getToken("collection");
	      release := ts.getToken("release");
	      SupFileRec.Check(sfr, collection, release);
	      sfr.scanTime := ts.getTime("scan time");

	      DetailCollection(self, sfr);
	    END;
	  END;
	END;
	ts := self.proto.getCmd(self.rd);
	ts.getLiteral(".");
	Trace(self, "");
	self.proto.putCmd(self.wr, ".");
	Wr.Flush(self.wr);

	SendFixups(self);
	Trace(self, "");
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
	  message := "Detailer failed: " & msg);
    | Rd.EndOfFile =>
	RETURN NEW(SupMisc.ThreadStatus,
	  status := SupMisc.ExitCode.TransientFailure,
	  message := "Detailer failed: Premature EOF from server");
    | Rd.Failure(list) =>
	RETURN NEW(SupMisc.ThreadStatus,
	  status := SupMisc.ExitCode.TransientFailure,
	  message := "Detailer failed: Network read failure: "
	    & ErrMsg.StrError(list));
    | Thread.Alerted =>
	RETURN NEW(SupMisc.ThreadStatus,
	  status := SupMisc.ExitCode.Failure,
	  message := "Detailer failed: Interrupted");
    | TokScan.Error(msg) =>
	RETURN NEW(SupMisc.ThreadStatus,
	  status := SupMisc.ExitCode.Failure,
	  message := "Detailer failed: Protocol error: " & msg);
    | Wr.Failure(list) =>
	RETURN NEW(SupMisc.ThreadStatus,
	  status := SupMisc.ExitCode.TransientFailure,
	  message := "Detailer failed: Network write failure: "
	    & ErrMsg.StrError(list));
    END;
  END Apply;

PROCEDURE DetailCollection(self: T; sfr: SupFileRec.T)
  RAISES {Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted, TokScan.Error,
	  Wr.Failure} =
  VAR
    ts: TokScan.T;
    compress: BOOLEAN;
    cmd: TEXT;
    name: TEXT;
    linkTo: TEXT;
    attrText: TEXT;
    attr: FileAttr.T;
    cmdCh: CHAR;
    startingBytesIn, startingWireBytesIn: LONGREAL;
    startingBytesOut, startingWireBytesOut: LONGREAL;
  BEGIN
    self.proto.putCmd(self.wr, "COLL", sfr.collection, sfr.release);
    Wr.Flush(self.wr);

    compress := SupFileRec.Option.Compress IN sfr.options;

    IF compress THEN
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
      IF compress THEN
	TRY
	  self.wr := NEW(GzipWr.T).init(self.wireWr, level := self.compLevel,
	    closeChild := FALSE);
	EXCEPT OSError.E(list) =>
	  RAISE Error("Cannot create Gzip writer: " & ErrMsg.StrError(list));
	END;
      END;
      TRY
	TRY
	  self.statusFile := StatusFile.Open(sfr);
	  TRY
	    LOOP
	      IF Thread.TestAlert() THEN
		RAISE Thread.Alerted;
	      END;
	      startingBytesIn := StreamRd.ByteCount(self.rd);
	      startingBytesOut := StreamWr.ByteCount(self.wr);
	      startingWireBytesIn := StreamRd.ByteCount(self.wireRd);
	      startingWireBytesOut := StreamWr.ByteCount(self.wireWr);
	      ts := self.proto.getCmd(self.rd);
	      cmdCh := ts.getChar("command");
	      cmd := Text.FromChar(cmdCh);
	      CASE cmdCh OF
	      | '.' =>
		  EXIT;
	      | 'D' =>  (* Delete file. *)
		  name := ts.getToken("file name");
		  ts.getEnd("End of \"" & cmd & "\" command");
		  Trace(self, name);
		  self.proto.putCmd(self.wr, "D", name);
	      | 'H', 'h' =>  (* Make hard link. *)
		  name := ts.getToken("file name");
		  linkTo := ts.getToken("hard link target");
		  ts.getEnd("End of \"" & cmd & "\" command");
		  Trace(self, name);
		  self.proto.putCmd(self.wr, cmd, name, linkTo);
	      | 'I', 'i', 'j' =>  (* Directory operations. *)
		  name := ts.getToken("directory name");
		  ts.getEnd("End of \"" & cmd & "\" command");
		  self.proto.putCmd(self.wr, cmd, name);
	      | 'J' =>  (* Directory operations. *)
		  name := ts.getToken("directory name");
		  attrText := ts.getToken("attributes");
		  ts.getEnd("End of \"" & cmd & "\" command");
		  self.proto.putCmd(self.wr, cmd, name, attrText);
	      | 'T', 't' =>  (* Add file in CVS mode. *)
		  name := ts.getToken("file name");
		  IF self.proto.v.hasFileAttrs THEN
		    attr := DecodeAttr(self, ts.getToken("attributes"));
		  ELSE
		    attr := NEW(FileAttr.T).init(FileAttr.FileType.File,
		      modTime := ts.getTime("modification time"));
		  END;
		  ts.getEnd("End of \"" & cmd & "\" command");
		  Trace(self, name);
		  CheckFileAttr(self, sfr, name, attr, inAttic := cmdCh = 't');
	      | 'U' =>  (* Add or update file. *)
		  name := ts.getToken("file name");
		  ts.getEnd("End of \"" & cmd & "\" command");
		  Trace(self, name);
		  SendDetails(self, sfr, name);
	      | '!' =>	(* Warning from server. *)
		  Warn(self, "Server warning: " & ts.getRest());
	      ELSE
		RAISE TokScan.Error("Invalid command \"" & cmd & "\"");
	      END;

	      Wr.Flush(self.wr);
	      IF self.stats # NIL THEN
		LOCK self.stats DO
		  INC(self.stats.numRequests);
		  self.stats.bytesIn := self.stats.bytesIn +
		    StreamRd.ByteCount(self.rd) - startingBytesIn;
		  self.stats.wireBytesIn := self.stats.wireBytesIn +
		    StreamRd.ByteCount(self.wireRd) - startingWireBytesIn;
		  self.stats.bytesOut := self.stats.bytesOut +
		    StreamWr.ByteCount(self.wr) - startingBytesOut;
		  self.stats.wireBytesOut := self.stats.wireBytesOut +
		    StreamWr.ByteCount(self.wireWr) - startingWireBytesOut;
		END;
		self.stats.update();
	      END;
	    END;
	  FINALLY
	    self.statusFile.close();
	  END;
	EXCEPT FileStatus.Error(msg) =>
	  RAISE Error(msg);
	END;

	self.proto.putCmd(self.wr, ".");
	Wr.Flush(self.wr);

	IF compress THEN
	  Wr.Close(self.wr);
	  IF NOT Rd.EOF(self.rd) THEN
	    RAISE TokScan.Error(
	      "Expected EOF from compressed stream, didn't get it");
	  END;
	  Rd.Close(self.rd);
	END;
      FINALLY
	IF compress THEN
	  GzipWr.Cleanup(self.wr);
	  self.wr := self.wireWr;
	END;
      END;
    FINALLY
      IF compress THEN
	GzipRd.Cleanup(self.rd);
	self.rd := self.wireRd;
      END;
    END;
  END DetailCollection;

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

PROCEDURE CheckFileAttr(self: T;
                        sfr: SupFileRec.T;
			name: Pathname.T;
			serverAttr: FileAttr.T;
			inAttic: BOOLEAN)
  RAISES {FileStatus.Error, Thread.Alerted, Wr.Failure} =
  VAR
    path := SupMisc.CatPath(sfr.clientPrefix, name);
    clientAttr: FileAttr.T := NIL;
    upToDate := FALSE;
    cmd: TEXT;
    attrText: TEXT;
  BEGIN
    (* This should never get called in checkout mode, but we'll be
       careful anyway. *)
    IF NOT SupFileRec.Option.CheckoutMode IN sfr.options THEN
      (* Check for an already up-to-date version of the file. *)
      IF inAttic THEN path := SupMisc.AtticName(path) END;
      TRY
	clientAttr := FileAttr.FromPathname(path, follow := FALSE);
	upToDate := FileAttr.Equal(clientAttr, serverAttr);
      EXCEPT OSError.E => (* File doesn't exist. *) END;
    END;

    IF upToDate THEN  (* Just make sure the list file gets updated. *)
      IF inAttic THEN cmd := "l" ELSE cmd := "L" END;
      (* We send the client's version of the attributes rather than the
	 server's, and we don't cull attributes that have been negotiated
	 away.  The attributes are going to go directly into our list
	 file, and so we want them to be as complete as possible. *)
      IF self.proto.v.hasFileAttrs THEN
	attrText := FileAttr.Encode(clientAttr);
      ELSE
	attrText := TokScan.EncodeTime(FileAttr.GetModTime(clientAttr));
      END;
      self.proto.putCmd(self.wr, cmd, name, attrText);
    ELSE  (* Detail the file. *)
      SendDetails(self, sfr, name, attr := clientAttr);
    END;
  END CheckFileAttr;

PROCEDURE SendDetails(self: T;
                      sfr: SupFileRec.T;
		      name: Pathname.T;
		      attr: FileAttr.T := NIL)
  RAISES {FileStatus.Error, Thread.Alerted, Wr.Failure} =
  VAR
    path: Pathname.T;
  BEGIN
    IF SupFileRec.Option.CheckoutMode IN sfr.options THEN
      SendCheckoutDetails(self, sfr, name);
    ELSE
      (* Determine whether it is a file or a node. *)
      IF attr = NIL THEN  (* We don't have the attributes yet. *)
	path := SupMisc.CatPath(sfr.clientPrefix, name);
	TRY
	  attr := Attic.FileAttrFromPathname(path, follow := FALSE);
	EXCEPT OSError.E => (* Doesn't exist. *) END;
      END;

      IF attr = NIL THEN  (* The file doesn't exist here, so add it. *)
	self.proto.putCmd(self.wr, "A", name);
      ELSIF attr.fileType = FileAttr.FileType.File THEN  (* Regular file. *)
	IF SupMisc.IsRCS(name)
	AND NOT SupFileRec.Option.NoRCS IN sfr.options THEN
	  SendRCSDetails(self, sfr, name);
	ELSE
	  SendRegularDetails(self, sfr, name);
	END;
      ELSE  (* Some kind of node. *)
	IF self.proto.v.hasFileAttrs THEN
	  self.proto.putCmd(self.wr, "N", name);
	END;
      END;
    END;
  END SendDetails;

PROCEDURE SendRCSDetails(self: T;
                         sfr: SupFileRec.T;
			 name: Pathname.T)
  RAISES {Thread.Alerted, Wr.Failure} =
  VAR
    pathname := SupMisc.CatPath(sfr.clientPrefix, name);
    rf: RCSFile.T;
  BEGIN
    TRY
      rf := Attic.RCSFileOpenReadonly(pathname);
    EXCEPT
    | OSError.E =>
	(* The RCS file doesn't exist on the client.  Just have the server
	   send a whole new file. *)
	self.proto.putCmd(self.wr, "A", name);
	RETURN;
    | RCSError.E =>
	(* The file is not a valid RCS file.  Treat it as a regular file. *)
	SendRegularDetails(self, sfr, name);
	RETURN;
    END;
    TRY
      self.proto.putCmd(self.wr, "V", name);
      SendAdmin(self, rf);
      SendDeltas(self.wr, rf);
      self.proto.putCmd(self.wr, ".");
    FINALLY
      TRY
	RCSFile.Close(rf);
      EXCEPT OSError.E(list) =>
	Err(self, "Cannot close \"" & pathname & "\": " &
	  ErrMsg.StrError(list));
      END;
    END;
  END SendRCSDetails;

PROCEDURE SendAdmin(self: T; rf: RCSFile.T)
  RAISES {Thread.Alerted, Wr.Failure} =
  VAR
    iter: RCSFile.TagIterator;
    tag: RCSTag.T;
  BEGIN
    IF rf.branch = NIL THEN
      self.proto.putCmd(self.wr, "b");
    ELSE
      self.proto.putCmd(self.wr, "B", rf.branch);
    END;

    self.proto.putCmd(self.wr, "E", RCSKeyword.EncodeExpand(rf.expand));

    self.proto.putCmd(self.wr, "T");
    iter := RCSFile.IterateTags(rf);
    WHILE iter.next(tag) DO
      Wr.PutText(self.wr, tag.name & " " & tag.revNum & "\n");
    END;

    self.proto.putCmd(self.wr, ".");
  END SendAdmin;

PROCEDURE SendDeltas(wr: Wr.T; rf: RCSFile.T)
  RAISES {Thread.Alerted, Wr.Failure} =
  VAR
    iter: RCSDeltaTbl.Iterator;
    rev: RCSRevNum.T;
    delta: RCSDelta.T;
  BEGIN
    SupMisc.PutCmd(wr, "D");
    iter := RCSFile.IterateByNumber(rf);
    WHILE iter.next(rev, delta) DO
      Wr.PutText(wr, rev & " " & delta.date & "\n");
    END;
    SupMisc.PutCmd(wr, ".");
  END SendDeltas;

(*****************************************************************************)

PROCEDURE SendRegularDetails(self: T;
                             sfr: SupFileRec.T;
			     name: Pathname.T)
  RAISES {Thread.Alerted, Wr.Failure} =
  BEGIN
    IF NOT SupFileRec.Option.NoRsync IN sfr.options
    AND NOT sfr.noRsync.test(name) THEN
      SendRsyncDetails(self, sfr, name);
    ELSE
      SendSimpleDetails(self, sfr, name);
    END;
  END SendRegularDetails;

PROCEDURE SendRsyncDetails(self: T;
                            sfr: SupFileRec.T;
			    name: Pathname.T)
  RAISES {Thread.Alerted, Wr.Failure} =
  VAR
    pathname := SupMisc.CatPath(sfr.clientPrefix, name);
    rsf: RsyncFile.T;
    bi: RsyncFile.BlockIterator;
    block: RsyncBlock.T;
  BEGIN
    TRY
      rsf := RsyncFile.Open(pathname);
    EXCEPT OSError.E =>  (* The file doesn't exist on the client. *)
      self.proto.putCmd(self.wr, "A", name);
      RETURN;
    END;
    TRY
      self.proto.putCmd(self.wr, "r",
	name,
	Fmt.Unsigned(FileAttr.GetSize(rsf.attr), 10),
	Fmt.Unsigned(rsf.blockSize, 10));
      bi := RsyncFile.IterateBlocks(rsf);
      WHILE bi.next(block) DO
	Wr.PutText(self.wr, Fmt.Unsigned(block.rsum, 16)
	  & " " & MD5Digest.ToText(block.md5)
	  & "\n");
      END;
      self.proto.putCmd(self.wr, ".");
    FINALLY
      TRY
	RsyncFile.Close(rsf);
      EXCEPT OSError.E(l) =>
	Err(self, "Cannot close \"" & pathname & "\": "
	  & ErrMsg.StrError(l));
      END;
    END;
  END SendRsyncDetails;

PROCEDURE SendSimpleDetails(self: T;
                            sfr: SupFileRec.T;
			    name: Pathname.T)
  RAISES {Thread.Alerted, Wr.Failure} =
  VAR
    pathname := SupMisc.CatPath(sfr.clientPrefix, name);
    rd: Rd.T;
    buf: ARRAY [0..8191] OF CHAR;
    md5: MD5.T;
    size: CARDINAL := 0;
    n: CARDINAL;
    cksum: TEXT;
  BEGIN
    TRY
      rd := FileRd.Open(pathname);
    EXCEPT OSError.E =>  (* The file doesn't exist on the client. *)
      self.proto.putCmd(self.wr, "A", name);
      RETURN;
    END;
    TRY
      TRY
	md5 := MD5.New();
	TRY
	  LOOP
	    n := Rd.GetSub(rd, buf);
	    IF n = 0 THEN EXIT END;
	    md5.update(SUBARRAY(buf, 0, n));
	    INC(size, n);
	  END;
	FINALLY
	  cksum := md5.finish();
	END;
	self.proto.putCmd(self.wr, "R",
	  name,
	  Fmt.Unsigned(size, 10),
	  cksum);
      FINALLY
	Rd.Close(rd);
      END;
    EXCEPT Rd.Failure(list) =>
      Err(self, "Read failure from \"" & pathname & "\": "
	& ErrMsg.StrError(list));
    END;
  END SendSimpleDetails;

(*****************************************************************************)
(* Checkout mode. *)
(*****************************************************************************)

PROCEDURE SendCheckoutDetails(self: T;
                              sfr: SupFileRec.T;
                              name: Pathname.T)
  RAISES {FileStatus.Error, Thread.Alerted, Wr.Failure} =
  VAR
    checkoutName := SupMisc.CheckoutName(name);
    pathname := SupMisc.CatPath(sfr.clientPrefix, checkoutName);
    fs: FileStatus.T;
    attr: FileAttr.T;
    cksum: TEXT;
  BEGIN
    TRY
      attr := FileAttr.FromPathname(pathname, follow := FALSE);
    EXCEPT OSError.E =>
      (* We don't have the file, so the only option at this point is to
	 tell the server to send it.  The server may figure out that the
	 file is dead, in which case it will tell us. *)
      self.proto.putCmd(self.wr, "C",
	name,
	sfr.checkoutTag,
	sfr.checkoutDate);
      RETURN;
    END;

    (* At this point, we know we have some version of the file on
       the client.  Get the information we have recorded about the
       file from a previous update, if any. *)
    fs := self.statusFile.get(name);

    (* If our recorded information doesn't match the file that the
       client has, then ignore the recorded information. *)
    IF fs # NIL THEN
      IF fs.type # FileStatus.Type.CheckoutLive
      OR NOT FileAttr.Equal(attr, fs.clientAttr) THEN
	fs := NIL;
      END;
    END;

    (* In earlier versions of this program, we did not record
       the dates associated with revision numbers.  To make the
       transition from old versions to new versions work properly,
       we have to handle the possibility that the revision date
       is not recorded.  We also have to deal with servers that
       are not prepared to receive revision dates. *)

    IF fs # NIL
    AND (NOT Text.Equal(fs.revDate, ".") OR NOT self.proto.v.sendsRevDates) THEN
      (* Our recorded information is up-to-date, and either we know the
	 revision date or the server doesn't support sending it anyway. *)
      IF self.proto.v.sendsRevDates THEN  (* Server accepts revDates. *)
	self.proto.putCmd(self.wr, "U",
	  name,
	  sfr.checkoutTag,
	  sfr.checkoutDate,
	  fs.revNum,
	  fs.revDate);
      ELSE  (* Server does not accept revDates. *)
	self.proto.putCmd(self.wr, "U",
	  name,
	  sfr.checkoutTag,
	  sfr.checkoutDate,
	  fs.revNum);
      END;
      RETURN;
    END;

    (* We don't have complete and/or accurate recorded information
       about what version of the file we have.  Compute the file's
       checksum as an aid toward identifying which version it is. *)

    TRY
      cksum := MD5.FileSignature(pathname);
    EXCEPT OSError.E(list) =>
      Err(self, "Cannot calculate checksum for \"" & pathname & "\": "
	& ErrMsg.StrError(list));
      RETURN;
    END;

    IF fs = NIL THEN
    (* We have no accurate information about the client's file.  The
       server will have to guess its revision number based on the
       checksum. *)
      self.proto.putCmd(self.wr, "S",
	name,
	sfr.checkoutTag,
	sfr.checkoutDate,
	cksum);
    ELSE
      (* Our information is accurate, but we don't trust it because
	 we hadn't recorded the date associated with the revision.
	 If somebody replaced the server's RCS file with a completely
	 different one (it happens!), then our revision number will
	 only mislead the server.  Send the revision number, but
	 require the server to verify the checksum too. *)
      <* ASSERT Text.Equal(fs.revDate, ".") *>
      <* ASSERT self.proto.v.sendsRevDates *>
      self.proto.putCmd(self.wr, "s",
	name,
	sfr.checkoutTag,
	sfr.checkoutDate,
	fs.revNum,
	cksum);
    END;
  END SendCheckoutDetails;

(*****************************************************************************)

PROCEDURE SendFixups(self: T)
  RAISES {Error, Thread.Alerted, Wr.Failure} =
  VAR
    fixup: Fixup.T := NIL;
    compress: BOOLEAN;
  BEGIN
    FOR i := 0 TO self.collections.size()-1 DO
      WITH sfr = self.collections.get(i) DO
	IF NOT SupFileRec.Option.Skip IN sfr.options THEN
	  self.proto.putCmd(self.wr, "COLL", sfr.collection, sfr.release);

	  compress := SupFileRec.Option.Compress IN sfr.options;
	  IF compress THEN
	    Wr.Flush(self.wr);
	    TRY
	      self.wr := NEW(GzipWr.T).init(self.wireWr,
		level := self.compLevel, closeChild := FALSE);
	    EXCEPT OSError.E(list) =>
	      RAISE
		Error("Cannot create Gzip writer: " & ErrMsg.StrError(list));
	    END;
	  END;

	  LOOP  (* Process all the fixups that apply to this collection. *)
	    IF fixup = NIL THEN  (* Get the next fixup. *)
	      TRY
		fixup := self.fixups.get();
	      EXCEPT SyncFixupQueue.EndOfFile => EXIT END;
	    END;
	    IF fixup.sfr # sfr THEN EXIT END;

	    Trace(self, fixup.name);
	    IF SupFileRec.Option.CheckoutMode IN sfr.options THEN
	      self.proto.putCmd(self.wr, "Y",
		fixup.name,
		sfr.checkoutTag,
		sfr.checkoutDate);
	    ELSE
	      self.proto.putCmd(self.wr, "X", fixup.name);
	    END;
	    fixup := NIL;
	  END;

	  self.proto.putCmd(self.wr, ".");  (* Close collection. *)
	  IF compress THEN
	    Wr.Close(self.wr);
	    self.wr := self.wireWr;
	  END;
	END;
      END;
    END;
    <* ASSERT fixup = NIL *>
    self.proto.putCmd(self.wr, ".");  (* Close fixups. *)
    Wr.Flush(self.wr);
  END SendFixups;

(*****************************************************************************)

REVEAL
  Stats = StatsPublic BRANDED OBJECT
  OVERRIDES
    init := StatsInit;
    start := StatsDoNothing;
    update := StatsDoNothing;
    finish := StatsDoNothing;
  END;

PROCEDURE StatsInit(self: Stats): Stats =
  BEGIN
    RETURN self;
  END StatsInit;

PROCEDURE StatsDoNothing(<*UNUSED*> self: Stats) =
  BEGIN
  END StatsDoNothing;

(*****************************************************************************)

PROCEDURE Init(self: T;
               proto: CVProto.T;
	       rd: StreamRd.T;
	       wr: StreamWr.T;
	       collections: SupFileRecSeq.T;
	       fixups: SyncFixupQueue.T;
	       compLevel: [-1..9] := -1;
	       reaper: Reaper.T := NIL;
	       stats: Stats := NIL;
	       logger: Logger.T := NIL): T =
  BEGIN
    self.proto := proto;
    self.wireRd := rd;
    self.rd := rd;
    self.wireWr := wr;
    self.wr := wr;
    self.collections := collections;
    self.fixups := fixups;
    IF compLevel = -1 THEN compLevel := SupMisc.DefaultCompression END;
    self.compLevel := compLevel;
    self.reaper := reaper;
    self.stats := stats;
    self.logger := logger;
    RETURN self;
  END Init;

PROCEDURE Err(self: T; msg: TEXT) =
  BEGIN
    IF self.logger # NIL THEN
      Logger.Put(self.logger, Logger.Priority.Err, msg);
    END;
  END Err;

PROCEDURE Warn(self: T; msg: TEXT) =
  BEGIN
    IF self.logger # NIL THEN
      Logger.Put(self.logger, Logger.Priority.Warning, msg);
    END;
  END Warn;

PROCEDURE Trace(self: T; msg: TEXT) =
  BEGIN
    IF self.logger # NIL THEN
      Logger.Put(self.logger, Logger.Priority.Info, msg);
    END;
  END Trace;

BEGIN
END Detailer.
