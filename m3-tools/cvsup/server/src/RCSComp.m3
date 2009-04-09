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
 * $Id: RCSComp.m3,v 1.1.1.1 2009-04-09 17:01:50 jkrell Exp $ *)

MODULE RCSComp;

IMPORT
  Attic, ClientClass, CVProto, ErrMsg, EscapedWr, ExecRec, File,
  FileAttr, FileRd, Fmt, FS, GlobTree, GzipRd, GzipWr, Logger,
  MD5, MD5Digest, MD5Wr, NullWr, OSError, Pathname, ParsedDelta,
  ParsedDeltaList, RCSDate, RCSDelta, RCSDeltaList, RCSDeltaMerger,
  RCSDeltaTbl, RCSError, RCSFile, RCSKeyword, RCSPhrase, RCSPhrases,
  RCSRevNum, RCSString, RCSTag, RCSTagList, RCSTagListSort,
  RCSTagMerger, Rd, RdCopy, Reaper, RsyncBlock, RsyncFile, StreamRd,
  StreamWr, SortedRCSDeltaTbl, SupFileRec, SupFileRecSeq, SupMisc,
  Text, Thread, TokScan, Word, Wr;
  
IMPORT IO;


EXCEPTION Error(TEXT);

CONST
  MaxChecksumRevisions = 10;
(* Maximum revisions to checksum in attempting to identify a client file
   in checkout mode. *)


REVEAL
  T = Public BRANDED OBJECT
    proto: CVProto.T;
    wireRd: StreamRd.T;		(* Raw reader. *)
    rd: StreamRd.T;		(* Currently active reader. *)
    wireWr: StreamWr.T;		(* Raw writer. *)
    wr: StreamWr.T;		(* Currently active writer. *)
    collections: SupFileRecSeq.T;
    clientClass: ClientClass.T;
    compLevel: [0..9];
    reaper: Reaper.T;
    logger: Logger.T;
  OVERRIDES
    apply := Apply;
    init := Init;
  END;

  Private = Thread.Closure BRANDED OBJECT END;

PROCEDURE Apply(self: T): REFANY =
  BEGIN
    TRY
      TRY
	CompBatch(self);  (* All the collections. *)
	CompBatch(self);  (* All the fixups. *)
      FINALLY
	IF self.reaper # NIL THEN
	  Reaper.Dying(self.reaper);
	END;
      END;
    EXCEPT
    | Error(msg) =>
      RETURN "RCSComp error: " & msg;
    | Rd.EndOfFile =>
      RETURN "RCSComp protocol error: Premature end of file";
    | Rd.Failure(list) =>
      RETURN "Network read failure: " & ErrMsg.StrError(list);
    | Thread.Alerted =>
      RETURN "Interrupted";
    | TokScan.Error(msg) =>
      RETURN "RCSComp protocol error: " & msg;
    | Wr.Failure(list) =>
      RETURN "Network write failure: " & ErrMsg.StrError(list);
    END;
    RETURN NIL;
  END Apply;

PROCEDURE CheckName(self: T; name: Pathname.T): BOOLEAN
  RAISES {Thread.Alerted, Wr.Failure} =
(* Returns "TRUE" if the given filename is safe from a security standpoint.
   Otherwise, logs a warning and returns "FALSE". *)
  VAR
    length := Text.Length(name);
    start, limit: INTEGER;
  BEGIN
    TRY
      IF length = 0 THEN
	RAISE Error("Invalid empty filename");
      END;
      IF Text.GetChar(name, 0) = SupMisc.SlashChar THEN
	RAISE Error("Absolute pathname \"" & name & "\" not allowed");
      END;
      start := 0;
      WHILE start < length DO
	limit := Text.FindChar(name, SupMisc.SlashChar, start);
	IF limit = -1 THEN limit := length END;
	IF start = limit
	OR Text.Equal(Text.Sub(name, start, limit-start), Pathname.Parent)
	THEN
	  RAISE Error("Filename \"" & name & "\" not allowed");
	END;
	start := limit + 1;
      END;
      RETURN TRUE;
    EXCEPT Error(msg) =>
      WarnBoth(self, msg);
      RETURN FALSE;
    END;
  END CheckName;

PROCEDURE CompBatch(self: T)
  RAISES {Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted,
	  TokScan.Error, Wr.Failure} =
(* Process all the collections or all the fixups. *)
  VAR
    ts: TokScan.T;
    collection, release: TEXT;
    initialBytesIn, initialBytesOut: LONGREAL;
  BEGIN
    FOR i := 0 TO self.collections.size()-1 DO
      WITH sfr = self.collections.get(i) DO
	IF NOT SupFileRec.Option.Skip IN sfr.options THEN
	  ts := self.proto.getCmd(self.rd);
	  ts.getFolded("COLL");
	  collection := ts.getToken("collection");
	  release := ts.getToken("release");
	  SupFileRec.Check(sfr, collection, release);
	  initialBytesIn := StreamRd.ByteCount(self.rd);
	  initialBytesOut := StreamWr.ByteCount(self.wr);
	  TRY
	    CompCollection(self, sfr);
	  FINALLY
	    LOCK sfr DO
	      sfr.bytesIn := sfr.bytesIn +
		StreamRd.ByteCount(self.rd) - initialBytesIn;
	      sfr.bytesOut := sfr.bytesOut +
		StreamWr.ByteCount(self.wr) - initialBytesOut;
	    END;
	  END;
	END;
      END;
    END;
    ts := self.proto.getCmd(self.rd);
    ts.getLiteral(".");
    self.proto.putCmd(self.wr, ".");
    Wr.Flush(self.wr);
  END CompBatch;

PROCEDURE CompCollection(self: T; sfr: SupFileRec.T)
  RAISES {Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted,
	  TokScan.Error, Wr.Failure} =
  VAR
    ts: TokScan.T;
    compress: BOOLEAN;
    cmd: TEXT;
    cmdCh: CHAR;
    name: TEXT;
    tag: TEXT;
    date: TEXT;
    revNum: RCSRevNum.T;
    revDate: RCSDate.T;
    size: CARDINAL;
    blockSize: CARDINAL;
    cksum: TEXT;
    attrText: TEXT;
    linkTo: TEXT;
    collectionPartiallyHidden :=
      self.clientClass.collectionIsPartiallyHidden(sfr.collection);
    MsgCheckoutUnsupported := "Checkout from partially hidden " & 
                                  "collection \"" & sfr.collection & 
                                  "\" is currently not suppported.";
  BEGIN
    self.proto.putCmd(self.wr, "COLL", sfr.collection, sfr.release);
    Wr.Flush(self.wr);

    compress := SupFileRec.Option.Compress IN sfr.options;

    IF compress THEN
      TRY
	self.rd := NEW(GzipRd.T).init(self.wireRd, closeChild := FALSE);
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
	LOOP
	  ts := self.proto.getCmd(self.rd);
	  cmdCh := ts.getChar("command");
	  cmd := Text.FromChar(cmdCh);
	  CASE cmdCh OF
	  | '.' =>
	      EXIT;
	  | 'A' =>  (* Add file. *)
	      name := ts.getToken("file name");
	      ts.getEnd("end of \"" & cmd & "\" command");
	      RegularSend(self, sfr, name, isFixup := FALSE);
	  | 'C' =>  (* Checkout file. *)
	      name := ts.getToken("file name");
	      tag := ts.getToken("tag");
	      date := ts.getToken("date");
	      ts.getEnd("end of \"" & cmd & "\" command");
              IF collectionPartiallyHidden THEN
                WarnBoth(self, MsgCheckoutUnsupported);
              ELSE
                CheckoutSend(self, sfr, name, tag, date);
              END;
	  | 'D' =>  (* Delete file. *)
	      name := ts.getToken("file name");
	      ts.getEnd("end of \"" & cmd & "\" command");
	      self.proto.putCmd(self.wr, "D", name);
	  | 'H', 'h' =>  (* Make hard link. *)
	      name := ts.getToken("file name");
	      linkTo := ts.getToken("hard link target");
	      ts.getEnd("end of \"" & cmd & "\" command");
	      SendExecutes(self, sfr, name,
		inAttic := cmdCh = 'h', isCheckout := FALSE);
	      self.proto.putCmd(self.wr, cmd, name, linkTo);
	  | 'I', 'i', 'j' =>  (* Directory operations. *)
	      name := ts.getToken("file name");
	      ts.getEnd("End of \"" & cmd & "\" command");
	      self.proto.putCmd(self.wr, cmd, name);
	  | 'J' =>  (* Set directory attributes. *)
	      name := ts.getToken("file name");
	      attrText := ts.getToken("attributes");
	      ts.getEnd("End of \"" & cmd & "\" command");
	      SendExecutes(self, sfr, name,
		inAttic := FALSE, isCheckout := FALSE);
	      self.proto.putCmd(self.wr, cmd, name, attrText);
	  | 'L', 'l' =>  (* Update list file info for up-to-date file. *)
	      name := ts.getToken("file name");
	      attrText := ts.getToken("attributes");
	      ts.getEnd("end of \"" & cmd & "\" command");
	      self.proto.putCmd(self.wr, cmd, name, attrText);
	  | 'N' =>  (* Update a node. *)
	      name := ts.getToken("file name");
	      ts.getEnd("end of \"" & cmd & "\" command");
	      NodeCompare(self, sfr, name);
	  | 'R' =>  (* Update regular file. *)
	      name := ts.getToken("file name");
	      size := ts.getInt("size");
	      cksum := ts.getToken("checksum");
	      ts.getEnd("end of \"" & cmd & "\" command");
	      RegularCompare(self, sfr, name, size, cksum);
	  | 'r' =>  (* Do Rsync update. *)
	      name := ts.getToken("file name");
	      size := ts.getInt("size");
	      blockSize := ts.getInt("blockSize");
	      ts.getEnd("end of \"" & cmd & "\" command");
	      RsyncCompare(self, sfr, name, size, blockSize);
	  | 'S' =>  (* Update in checkout mode from checksum. *)
	      name := ts.getToken("file name");
	      tag := ts.getToken("tag");
	      date := ts.getToken("date");
	      cksum := ts.getToken("checksum");
	      ts.getEnd("end of \"" & cmd & "\" command");
              IF collectionPartiallyHidden THEN
                WarnBoth(self, MsgCheckoutUnsupported);
              ELSE
                UpdateFromChecksum(self, sfr, name, tag, date, cksum);
              END;
	  | 's' =>  (* Update in checkout mode from revNum and checksum. *)
	      name := ts.getToken("file name");
	      tag := ts.getToken("tag");
	      date := ts.getToken("date");
	      revNum := ts.getToken("revision number");
	      cksum := ts.getToken("checksum");
	      ts.getEnd("end of \"" & cmd & "\" command");
              IF collectionPartiallyHidden THEN
                WarnBoth(self, MsgCheckoutUnsupported);
              ELSE
                UpdateFromChecksum(self, sfr, name, tag, date, cksum, revNum);
              END;
	  | 'U' =>  (* Update in checkout mode from revNum and revDate. *)
	      name := ts.getToken("file name");
	      tag := ts.getToken("tag");
	      date := ts.getToken("date");
	      revNum := ts.getToken("revision number");
	      IF self.proto.v.sendsRevDates THEN
		revDate := ts.getToken("revDate");
	      ELSE
		revDate := ".";
	      END;
	      ts.getEnd("end of \"" & cmd & "\" command");
              IF collectionPartiallyHidden THEN
                WarnBoth(self, MsgCheckoutUnsupported);
              ELSE
                CheckoutUpdate(self, sfr, name, tag, date, revNum, revDate);
              END;
	  | 'V' =>  (* Update RCS file. *)
	      name := ts.getToken("file name");
	      ts.getEnd("end of \"" & cmd & "\" command");
	      RCSCompare(self, sfr, name);
	  | 'X' =>  (* Fixup RCS file. *)
	      name := ts.getToken("file name");
	      ts.getEnd("end of \"" & cmd & "\" command");
	      RegularSend(self, sfr, name, isFixup := TRUE);
	  | 'Y' =>  (* Fixup checkout-mode file. *)
	      name := ts.getToken("file name");
	      tag := ts.getToken("tag");
	      date := ts.getToken("date");
	      ts.getEnd("end of \"" & cmd & "\" command");
              IF collectionPartiallyHidden THEN
                WarnBoth(self, MsgCheckoutUnsupported);
              ELSE
                CheckoutSend(self, sfr, name, tag, date, isFixup := TRUE);
              END;
	  ELSE
	    RAISE TokScan.Error("Invalid command \"" & cmd & "\"");
	  END;

	  Wr.Flush(self.wr);
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
  END CompCollection;

PROCEDURE RCSCompare(self: T;
		     sfr: SupFileRec.T;
		     name: Pathname.T)
  RAISES {Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted,
	  TokScan.Error, Wr.Failure} =
  CONST myname = "RCSCompare: ";
(* The Rd.EndOfFile and Rd.Failure exceptions are raised only for the
   network connection. *)
  VAR
    pathname := SupMisc.CatPath(sfr.serverPrefix, name);
    origPathname := pathname;
    rf: RCSFile.T;
    ts: TokScan.T;
    cmd: TEXT;
    cmdCh: CHAR;
    clientBranch: TEXT := NIL;
    clientDeltas: RCSDeltaList.T := NIL;
    clientTags: RCSTagList.T := NIL;
    expandMode: RCSKeyword.ExpandMode;
    doBranch, doDeltas, doExpand, doTags: BOOLEAN := FALSE;
    deletes: RCSDeltaList.T;
    adds: ParsedDeltaList.T;
    dtab: SortedRCSDeltaTbl.T := NIL;
    inAttic: BOOLEAN;
    md5: MD5.T;
  BEGIN
    (* Read all the information from the client. *)
    Trace(self, myname, name);
    LOOP
      ts := self.proto.getCmd(self.rd);
      cmdCh := ts.getChar("command");
      cmd := Text.FromChar(cmdCh);
      CASE cmdCh OF
      | '.' =>
	  EXIT;
      | 'B' =>  (* Default branch. *)
	  clientBranch := ts.getToken("default branch");
	  ts.getEnd("end of \"" & cmd & "\" command");
	  doBranch := TRUE;
      | 'b' =>  (* Empty default branch. *)
	  clientBranch := "";
	  ts.getEnd("end of \"" & cmd & "\" command");
	  doBranch := TRUE;
      | 'D' =>  (* Deltas. *)
	  ts.getEnd("end of \"" & cmd & "\" command");
	  clientDeltas := GetClientDeltas(self);
	  doDeltas := TRUE;
      | 'E' =>  (* Expand mode. *)
	  TRY
	    expandMode := RCSKeyword.DecodeExpand(
			      ts.getToken("expand mode"));
	  EXCEPT RCSError.E(msg) => RAISE TokScan.Error(msg) END;
	  ts.getEnd("end of \"" & cmd & "\" command");
	  doExpand := TRUE;
      | 'T' =>  (* Tags. *)
	  ts.getEnd("end of \"" & cmd & "\" command");
	  clientTags := GetClientTags(self);
	  doTags := TRUE;
      ELSE
	RAISE TokScan.Error("Invalid RCSComp command \"" & cmd & "\"");
      END;
    END;

    IF NOT CheckName(self, name) OR MaybeSendNode(self, sfr, name) THEN
      RETURN;
    END;

    (* Do the comparisons, and send editing commands to the Updater. *)
    TRY
      rf := Attic.RCSFileOpenReadonly(pathname);
    EXCEPT
    | OSError.E(list) =>
	WarnBoth(self, "Cannot open \"" & pathname
	  & "\": " & ErrMsg.StrError(list));
	RETURN;
    | RCSError.E(text) =>
	WarnBoth(self, "RCS file error in \"" & pathname & "\": " & text);
	RETURN;
    END;
    IF self.clientClass.collectionIsPartiallyHidden(sfr.collection) THEN
      dtab := AccessibleDeltas(rf, self.clientClass, sfr, self);
    END;
    TRY
      IF doDeltas THEN
	(* Parse the deltas now, so that if we have a corrupted RCS file
	   we can still recover from it and carry on. *)
	TRY
	  CompDeltas(self, sfr, clientDeltas, rf, dtab, deletes, adds);
	EXCEPT
	| RCSError.E(msg) =>
	    WarnBoth(self, "RCS file error in \"" & pathname &
	      "\": " & msg);
	    RETURN;
	| Error =>
            Trace(self, myname, 
              "different RCS file, need to start from scratch");
	    (* Date mismatch on one or more revisions.  Just replace the
	       entire file. *)
            RegularSend(self, sfr, name, isFixup := FALSE);
	    RETURN;
	END;
      END;

      inAttic := pathname # origPathname;
      IF inAttic THEN cmd := "v" ELSE cmd := "V" END;
      SendExecutes(self, sfr, name, inAttic := inAttic, isCheckout := FALSE);

      IF NOT SupFileRec.Option.StrictCheckRCS IN sfr.options
      AND self.proto.v.hasLooseRCSCheck THEN
	md5 := MD5.NewRCS();
      ELSE
	md5 := MD5.New();
      END;
      RCSFile.CalculateMD5(rf, md5);

      self.proto.putCmd(self.wr, cmd,
	name,
	AttrOrModTime(self, sfr, RCSFile.GetAttr(rf)),
	RCSFile.EncodeOptions(rf.options),
	md5.finish());

      (* DANGER - Errors beyond this point are fatal. *)

      IF doBranch THEN
	CompBranch(self, sfr, rf, clientBranch);
      END;
      IF doExpand THEN
	IF rf.expand # expandMode THEN
	  self.proto.putCmd(self.wr, "E", RCSKeyword.EncodeExpand(rf.expand));
	END;
      END;
      IF doTags THEN
	IF SupFileRec.Option.ExactRCS IN sfr.options THEN
	  CompTagsExact(self, sfr, clientTags, rf, dtab);
	ELSE
	  CompTagsNonExact(self, sfr, clientTags, rf, dtab);
	END;
      END;
      IF doDeltas THEN
	WHILE deletes # NIL DO
	  self.proto.putCmd(self.wr, "d", deletes.head.revision);
	  deletes := deletes.tail;
	END;

	WHILE adds # NIL DO
	  WITH pd = adds.head DO
	    SendDelta(self, pd.delta, RCSDelta.Predecessor(pd.delta),
	      pd.log, pd.text);
	  END;
	  adds := adds.tail;
	END;
      END;
      self.proto.putCmd(self.wr, ".");
    FINALLY
      TRY
	RCSFile.Close(rf);
      EXCEPT OSError.E(list) =>
	RAISE Error("Cannot close \"" & pathname & "\": " &
	  ErrMsg.StrError(list));
      END;
    END;
  END RCSCompare;

PROCEDURE GetClientDeltas(self: T): RCSDeltaList.T
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted, TokScan.Error} =
(* The Rd.EndOfFile and Rd.Failure exceptions are raised only for the
   network connection. *)
  VAR
    ts: TokScan.T;
    revNum: RCSRevNum.T;
    date: TEXT;
    deltas, last: RCSDeltaList.T := NIL;
    dl: RCSDeltaList.T;
  BEGIN
    LOOP
      ts := self.proto.getCmd(self.rd);
      revNum := ts.getToken("revision number");
      IF Text.Equal(revNum, ".") THEN EXIT END;
      date := ts.getToken("date");
      ts.getEnd("end of delta descriptor");
      dl := RCSDeltaList.List1(NEW(RCSDelta.T,
	revision := revNum, date := date));
      IF last # NIL THEN
	last.tail := dl;
      ELSE
	deltas := dl;
      END;
      last := dl;
    END;
    RETURN deltas;
  END GetClientDeltas;

PROCEDURE GetClientTags(self: T): RCSTagList.T
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted, TokScan.Error} =
(* The Rd.EndOfFile and Rd.Failure exceptions are raised only for the
   network connection. *)
  VAR
    ts: TokScan.T;
    name: TEXT;
    revNum: RCSRevNum.T;
    tags, last: RCSTagList.T := NIL;
    tl: RCSTagList.T;
  BEGIN
    (* The tags arrive in the order of their appearance in the client's
       RCS file. *)
    LOOP
      ts := self.proto.getCmd(self.rd);
      name := ts.getToken("tag name");
      IF Text.Equal(name, ".") THEN EXIT END;
      revNum := ts.getToken("revision number");
      ts.getEnd("end of tag descriptor");
      tl := RCSTagList.List1(NEW(RCSTag.T, name := name, revNum := revNum));
      IF last # NIL THEN
	last.tail := tl;
      ELSE
	tags := tl;
      END;
      last := tl;
    END;
    RETURN tags;
  END GetClientTags;

PROCEDURE CompBranch(self: T;
                     sfr: SupFileRec.T;
		     rf: RCSFile.T;
		     clientBranch: TEXT)
  RAISES {Thread.Alerted, Wr.Failure} =
  VAR
    serverBranch := rf.branch;
  BEGIN
    IF serverBranch = NIL THEN serverBranch := "" END;
    IF NOT Text.Equal(clientBranch, serverBranch) THEN
      (* The default branch is different.  But we don't always
	 want to change it on the client.  Certain kinds of
	 changes can make it invalid, as far as CVS is concerned.
	 If we are allowing deletions of revisions on the client
	 ("exact" mode), then we should keep the default branch
	 the same as the server.  If not, then we change the
	 default branch ONLY if it is not currently empty on
	 the client. *)
      IF SupFileRec.Option.ExactRCS IN sfr.options
      OR NOT Text.Empty(clientBranch) THEN
	IF Text.Empty(serverBranch) THEN
	  self.proto.putCmd(self.wr, "b");
	ELSE
	  self.proto.putCmd(self.wr, "B", serverBranch);
	END;
      END;
    END;
  END CompBranch;

PROCEDURE CompDeltas(self: T;
                     sfr: SupFileRec.T;
                     clientDeltas: RCSDeltaList.T;
                     rf: RCSFile.T;
                     dtab: SortedRCSDeltaTbl.T;
                     VAR deletes: RCSDeltaList.T;
                     VAR adds: ParsedDeltaList.T)
  RAISES {Error, RCSError.E} =
  CONST name = "CompDeltas: ";
  VAR
    dm: DeltaMerger;
    clientDelta: RCSDelta.T;
    serverDelta: RCSDelta.T;
  BEGIN
    dm := NEW(DeltaMerger).init(clientDeltas, rf, dtab);
    Trace(self, name, sfr.collection, " ", sfr.release);
    deletes := NIL;
    adds := NIL;
    WHILE DeltaNext(dm, clientDelta, serverDelta) DO
      IF clientDelta = NIL THEN
        Trace(self, name, "add: ", RCSDelta.ToText(serverDelta));
	adds := ParsedDeltaList.Cons(
	  ParsedDelta.T{
	    delta := serverDelta,
	    log := RCSDelta.GetLog(serverDelta).iterate(),
	    text := RCSDelta.GetText(serverDelta,
	      RCSDelta.Predecessor(serverDelta))},
	  adds);
      ELSIF serverDelta = NIL THEN
        Trace(self, name, "del: ", RCSDelta.ToText(clientDelta));
	IF SupFileRec.Option.ExactRCS IN sfr.options THEN
	  deletes := RCSDeltaList.Cons(clientDelta, deletes);
	END;
      ELSIF NOT RCSDate.Equal(clientDelta.date, serverDelta.date) THEN
        Trace(self, name, "err: ", RCSDelta.ToText(serverDelta), " ",
          RCSDelta.ToText(clientDelta));
	(* The client must have an entirely different RCS file.  We don't
	   even try to edit that.  Just replace the whole file. *)
	RAISE Error("Incorrect date on client delta");
      END;
    END;
    adds := ParsedDeltaList.ReverseD(adds);
  END CompDeltas;

PROCEDURE AccessibleDeltas(rf: RCSFile.T;
                           clientClass: ClientClass.T;
                           sfr: SupFileRec.T;
                           comp: T): SortedRCSDeltaTbl.T =

  CONST myname = "AccessibleDeltas: ";
  VAR (* AccessibleDeltas *)
    ti := RCSFile.IterateTags(rf);
    dt := NEW(SortedRCSDeltaTbl.Default).init();
    tag: RCSTag.T;
    delta, dd: RCSDelta.T;
    found := FALSE;
  BEGIN
    Trace(comp, myname);
    WHILE ti.next(tag) DO
      IF RCSTag.IsCVSBranch(tag) THEN
	IF clientClass.inAllowedCollectionBranches(sfr.collection, tag.name)
	THEN
          Trace(comp, "  exporting branch ", tag.name);
          TRY
            delta := RCSFile.GetTagDelta(rf, tag.name);
            WHILE delta # NIL DO
              found := dt.get(delta.revision, dd);
              IF found THEN
                Trace(comp, "    delta ", delta.revision, " already there",
                      level := 2);
              ELSE
                Trace(comp, "    delta ", delta.revision, " missing",
                      level := 2);
                EVAL dt.put(delta.revision, delta);
              END;
              IF RCSRevNum.IsTrunk(delta.revision) THEN
                IF found THEN
                  (* by construction every predecssing delta has already 
                     been inserted into the table so we can stop here *)
                  delta := NIL;
                ELSE
                  delta := RCSDelta.Predecessor(delta);
                END;
              ELSE
                delta := RCSDelta.GetPrev(delta);
              END;
            END;
          EXCEPT
            RCSError.E(m) => (* FIXME: do something *)
            Trace(comp, "  RCS error for ", tag.name, ": ", m);
          END;
        ELSE
          Trace(comp, "  hiding branch ", tag.name);
        END;
      ELSE
        Trace(comp, "  ignoring non-branch tag ", tag.name, level := 4);
      END;
    END;
    (* check all trunk revisions, too *)
    IF clientClass.inAllowedCollectionBranches(sfr.collection, ".") THEN
      Trace(comp, "  exporting main trunk");
      TRY
        delta := RCSFile.GetTagDelta(rf);
        WHILE delta # NIL DO
          found := dt.get(delta.revision, dd);
          IF found THEN
            Trace(comp, "    delta ", delta.revision, " already there", 
                  level := 2);
            delta := NIL; (* see above *)
          ELSE
            Trace(comp, "    delta ", delta.revision, " missing", level := 2);
            EVAL dt.put(delta.revision, delta);
            delta := RCSDelta.Predecessor(delta);
          END;
        END;
      EXCEPT
        RCSError.E(m) => (* FIXME: do something *)
        Trace(comp, "  RCS error for trunk head: ", m);
      END;
    ELSE
      Trace(comp, "  hiding main trunk");
    END;
    RETURN dt;
  END AccessibleDeltas;

PROCEDURE TagIncluded(dtab: SortedRCSDeltaTbl.T; tag: RCSTag.T): BOOLEAN =
  VAR d: RCSDelta.T;
  BEGIN
    RETURN dtab = NIL OR dtab.get(tag.revNum, d);
  END TagIncluded;

PROCEDURE CompTagsExact(self: T; 
                        sfr: SupFileRec.T;
                        clientTags: RCSTagList.T;
                        rf: RCSFile.T;
                        dtab: SortedRCSDeltaTbl.T)
  RAISES {Thread.Alerted, Wr.Failure} =
  CONST myname = "CompTagsExact: ";
  VAR
    serverTags: RCSTagList.T := NIL;
    iter: RCSFile.TagIterator;
    tag: RCSTag.T;
  BEGIN
    Trace(self, myname);
    (* Reverse the list of client tags, and build a list of the server's
       tags, also in reverse order. *)
    clientTags := RCSTagList.ReverseD(clientTags);
    iter := RCSFile.IterateTags(rf);
    WHILE iter.next(tag) DO
      IF self.clientClass.inAllowedCollectionTags(sfr.collection, tag.name) THEN
        IF RCSTag.IsCVSBranch(tag)
	AND self.clientClass.inAllowedCollectionBranches(sfr.collection,
	  tag.name)
	OR TagIncluded(dtab, tag) THEN
          serverTags := RCSTagList.Cons(tag, serverTags);
        END;
      END;
    END;

    (* Issue edits to make the client's tag list identical to that on
       the server.  Any common tail portion can be left alone.  (That's
       why we work with reversed lists here.) *)
    WHILE clientTags # NIL DO  (* Delete the extra tags on the client. *)
      IF serverTags # NIL AND
	RCSTag.Equal(clientTags.head, serverTags.head) AND
	RCSRevNum.Equal(clientTags.head.revNum, serverTags.head.revNum) THEN
	(* Common portion of the two lists.  We don't need any edits for it. *)
	serverTags := serverTags.tail;
      ELSE  (* An extra or unmatching tag on the client side. *)
        Trace(self, myname, "del tag ", clientTags.head.name, " ",
          clientTags.head.revNum);
	self.proto.putCmd(self.wr, "t",
	  clientTags.head.name,
	  clientTags.head.revNum);
      END;
      clientTags := clientTags.tail;
    END;
    WHILE serverTags # NIL DO  (* Add the missing tags from the server. *)
      IF self.clientClass.inAllowedCollectionTags(sfr.collection,
        serverTags.head.name)
      THEN
        IF RCSTag.IsCVSBranch(serverTags.head)
	AND self.clientClass.inAllowedCollectionBranches(sfr.collection,
	  serverTags.head.name)
	OR TagIncluded(dtab, serverTags.head) THEN
          Trace(self, myname, "adding tag ", serverTags.head.name, " ",
            serverTags.head.revNum);
          self.proto.putCmd(self.wr, "T",
                            serverTags.head.name,
                            serverTags.head.revNum);
        ELSE
          Trace(self, myname, "omitting tag ", serverTags.head.name, " ",
            serverTags.head.revNum);
        END;
      ELSE
        Trace(self, myname, "hiding tag ", serverTags.head.name, " ",
          serverTags.head.revNum);
      END;
      serverTags := serverTags.tail;
    END;
  END CompTagsExact;

PROCEDURE CompTagsNonExact(self: T; 
                           sfr: SupFileRec.T;
                           clientTags: RCSTagList.T;
                           rf: RCSFile.T;
                           dtab: SortedRCSDeltaTbl.T)
  RAISES {Thread.Alerted, Wr.Failure} =
(* Compare tags in non-exact mode.  The client is allowed to have extra,
   local tags.  We don't insist on maintaining an exact match between the
   client's list of tags and the server's, although we do make an attempt
   to keep them similar. *)
  CONST myname = "CompTagsNonExact: ";
  VAR
    tm: TagMerger;
    clientTag: RCSTag.T;
    serverTag: RCSTag.T;
    newNormalTags, newVendorTags, adds, deletes: RCSTagList.T := NIL;
    iter: RCSFile.TagIterator;
    tag: RCSTag.T;
  BEGIN
    Trace(self, myname);
    tm := NEW(TagMerger).init(clientTags, rf, 1);
    WHILE TagNext(tm, clientTag, serverTag) DO
      IF clientTag = NIL THEN
	newVendorTags := RCSTagList.Cons(serverTag, newVendorTags);
      ELSIF serverTag = NIL THEN
	(* Extra tag on client -- just ignore it. *)
      ELSIF NOT RCSRevNum.Equal(clientTag.revNum, serverTag.revNum) THEN
	deletes := RCSTagList.Cons(clientTag, deletes);
	newVendorTags := RCSTagList.Cons(serverTag, newVendorTags);
      END;
    END;

    tm := NEW(TagMerger).init(clientTags, rf, 0);
    WHILE TagNext(tm, clientTag, serverTag) DO
      IF clientTag = NIL THEN
	newNormalTags := RCSTagList.Cons(serverTag, newNormalTags);
      ELSIF serverTag = NIL THEN
	(* Extra tag on client -- just ignore it. *)
      ELSIF NOT RCSRevNum.Equal(clientTag.revNum, serverTag.revNum) THEN
	deletes := RCSTagList.Cons(clientTag, deletes);
	newNormalTags := RCSTagList.Cons(serverTag, newNormalTags);
      END;
    END;

    iter := RCSFile.IterateTags(rf);
    WHILE iter.next(tag) DO
      IF RCSRevNum.NumParts(tag.revNum) MOD 2 = 1 THEN
	IF RCSTagList.Member(newVendorTags, tag) THEN
	  adds := RCSTagList.Cons(tag, adds);
	END;
      ELSE
	IF RCSTagList.Member(newNormalTags, tag) THEN
	  adds := RCSTagList.Cons(tag, adds);
	END;
      END;
    END;

    (* Now "adds" contains a list of the tags we want to send, in reverse
       order relative to how they appear in the server-side RCS file.
       On the client side, each will be pushed onto the front of the
       tag list.  This will result in the proper order on the client
       side. *)

    (* First, send the deletes. *)
    WHILE deletes # NIL DO
      Trace(self, myname, "del tag ", deletes.head.name, " ",
        deletes.head.revNum);
      self.proto.putCmd(self.wr, "t", deletes.head.name, deletes.head.revNum);
      deletes := deletes.tail;
    END;

    (* Now send the adds. *)
    WHILE adds # NIL DO
      IF self.clientClass.inAllowedCollectionTags(sfr.collection,
        adds.head.name)
      THEN
        IF RCSTag.IsCVSBranch(adds.head)
	AND self.clientClass.inAllowedCollectionBranches(sfr.collection,
          adds.head.name)
	OR TagIncluded(dtab, adds.head) THEN
          Trace(self, myname, "adding tag ", adds.head.name, " ",
            adds.head.revNum);
          self.proto.putCmd(self.wr, "T", adds.head.name, adds.head.revNum);
        ELSE
          Trace(self, myname, "omitting tag ", adds.head.name, " ",
            adds.head.revNum);
        END;
      ELSE
        Trace(self, myname, "hiding tag ", adds.head.name, " ",
          adds.head.revNum);
      END;
      adds := adds.tail;
    END;
  END CompTagsNonExact;

PROCEDURE SendDelta(self: T;
          delta: RCSDelta.T;
          diffBase: RCSDelta.T;
	  log: RCSString.Iterator;
	  text: RCSString.Iterator)
  RAISES {Thread.Alerted, Wr.Failure} =
  VAR
    diffBaseRevNum: TEXT;
  BEGIN
    IF diffBase = NIL THEN
      diffBaseRevNum := ".";
    ELSE
      diffBaseRevNum := diffBase.revision;
    END;

    self.proto.putCmd(self.wr, "D",
      delta.revision,
      diffBaseRevNum,
      delta.date,
      delta.author);
    IF delta.state # NIL THEN
      self.proto.putCmd(self.wr, "S", delta.state);
    END;

    IF self.proto.v.sendsDeltaPhrases THEN
      SendPhrases(self, "N", RCSDelta.IterateTreePhrases(delta));
    END;

    IF log # NIL THEN
      self.proto.putCmd(self.wr, "L");
      SendEscaped(self.wr, log);
    END;

    IF self.proto.v.sendsDeltaPhrases THEN
      SendPhrases(self, "n", RCSDelta.IterateTextPhrases(delta));
    END;

    self.proto.putCmd(self.wr, "T");
    SendEscaped(self.wr, text);

    self.proto.putCmd(self.wr, ".");
  END SendDelta;

PROCEDURE SendEscaped(wr: Wr.T;
                      iter: RCSString.Iterator;
		      withChecksum := FALSE)
  RAISES {Thread.Alerted, Wr.Failure} =
  VAR
    eWr: Wr.T;
    md5Wr: MD5Wr.T;
    line: RCSString.T;
  BEGIN
    eWr := NEW(EscapedWr.T).init(wr, closeChild := FALSE);
    IF withChecksum THEN  (* Layer a checksummer on top. *)
      md5Wr := NEW(MD5Wr.T).init(eWr, closeChild := TRUE);
      eWr := md5Wr;
    END;

    TRY
      WHILE iter.next(line) DO
	Wr.PutText(eWr, line.toText());
      END;
    FINALLY
      Wr.Close(eWr);
    END;

    IF withChecksum THEN
      SupMisc.PutCmd(wr, "5", md5Wr.getSignature());
    END;
  END SendEscaped;

PROCEDURE SendPhrases(self: T;
                      cmd: TEXT;
		      iter: RCSPhrases.Iterator)
  RAISES {Thread.Alerted, Wr.Failure} =
  VAR
    eWr: Wr.T;
    phrase: RCSPhrase.T;
    wordIter: RCSPhrase.WordIterator;
    word: TEXT;
    isString: BOOLEAN;
  BEGIN
    WHILE iter.next(phrase) DO
      self.proto.putCmd(self.wr, cmd, RCSPhrase.GetKey(phrase));
      wordIter := RCSPhrase.IterateWords(phrase);
      WHILE wordIter.next(word, isString) DO
	IF isString THEN
	  self.proto.putCmd(self.wr, "S");
	  eWr := NEW(EscapedWr.T).init(self.wr, closeChild := FALSE);
	  TRY
	    Wr.PutText(eWr, word);
	  FINALLY
	    Wr.Close(eWr);
	  END;
	ELSE
	  self.proto.putCmd(self.wr, "W", word);
	END;
      END;
      self.proto.putCmd(self.wr, ".");
    END;
  END SendPhrases;

PROCEDURE Init(self: T;
               proto: CVProto.T;
               rd: StreamRd.T;
               wr: StreamWr.T;
	       collections: SupFileRecSeq.T;
               clientClass: ClientClass.T;
	       compLevel: [-1..9] := -1;
               reaper: Reaper.T := NIL;
	       logger: Logger.T := NIL): T =
  BEGIN
    self.proto := proto;
    self.wireRd := rd;
    self.rd := rd;
    self.wireWr := wr;
    self.wr := wr;
    self.collections := collections;
    self.clientClass := clientClass;
    IF compLevel = -1 THEN compLevel := SupMisc.DefaultCompression END;
    self.compLevel := compLevel;
    self.reaper := reaper;
    self.logger := logger;
    RETURN self;
  END Init;

(*****************************************************************************)
(* Merging of deltas. *)
(*****************************************************************************)

TYPE
  DeltaMerger = RCSDeltaMerger.T OBJECT
    clientDeltas: RCSDeltaList.T;
    iter: RCSDeltaTbl.Iterator;
  METHODS
    init(clientDeltas: RCSDeltaList.T;
	 rf: RCSFile.T;
         dtab: SortedRCSDeltaTbl.T): DeltaMerger := DeltaMergerInit;
  OVERRIDES
    getA := DeltaFromClient;
    getB := DeltaFromServer;
  END;

PROCEDURE DeltaMergerInit(dm: DeltaMerger;
			  clientDeltas: RCSDeltaList.T;
			  rf: RCSFile.T;
                          dtab: SortedRCSDeltaTbl.T): DeltaMerger =
  BEGIN
    dm.clientDeltas := clientDeltas;
    IF dtab = NIL THEN
      dm.iter := RCSFile.IterateByNumber(rf);
    ELSE
      dm.iter := dtab.iterateOrdered();
    END;
    RETURN dm;
  END DeltaMergerInit;

PROCEDURE DeltaNext(dm: DeltaMerger;
                    VAR clientDelta, serverDelta: RCSDelta.T): BOOLEAN =
(* This is a wrapper to correct the "RAISES" clause for "DeltaMerger.next".
   It raises the union of what is raised by "DeltaFromClient" and
   "DeltaFromServer". *)
  <* FATAL ANY *>
  BEGIN
    RETURN dm.next(clientDelta, serverDelta);
  END DeltaNext;

PROCEDURE DeltaFromClient(dm: DeltaMerger): RCSDelta.T =
(* If you add any exceptions, add them to "DeltaNext" also. *)
  VAR
    delta: RCSDelta.T;
  BEGIN
    IF dm.clientDeltas = NIL THEN
      delta := NIL;
    ELSE
      delta := dm.clientDeltas.head;
      dm.clientDeltas := dm.clientDeltas.tail;
    END;
    RETURN delta;
  END DeltaFromClient;

PROCEDURE DeltaFromServer(dm: DeltaMerger): RCSDelta.T =
(* If you add any exceptions, add them to "DeltaNext" also. *)
  VAR
    rev: RCSRevNum.T;
    delta: RCSDelta.T;
  BEGIN
    IF dm.iter.next(rev, delta) THEN RETURN delta END;
    RETURN NIL;
  END DeltaFromServer;

(*****************************************************************************)
(* Merging of tags. *)
(*****************************************************************************)

TYPE
  TagMerger = RCSTagMerger.T OBJECT
    clientTags: RCSTagList.T;
    iter: RCSFile.TagIterator;
    rem: [0..1];
  METHODS
    init(clientTags: RCSTagList.T;
	 rf: RCSFile.T;
	 rem: [0..1]): TagMerger := TagMergerInit;
  OVERRIDES
    getA := TagFromClient;
    getB := TagFromServer;
  END;

PROCEDURE TagMergerInit(tm: TagMerger;
			clientTags: RCSTagList.T;
                        rf: RCSFile.T;
			rem: [0..1]): TagMerger =
  BEGIN
    tm.clientTags := RCSTagListSort.Sort(clientTags);
    tm.iter := RCSFile.IterateTagsByName(rf);
    tm.rem := rem;
    RETURN tm;
  END TagMergerInit;

PROCEDURE TagNext(tm: TagMerger;
                  VAR clientTag, serverTag: RCSTag.T): BOOLEAN =
(* This is a wrapper to correct the "RAISES" clause for "TagMerger.next".
   It raises the union of what is raised by "TagFromClient" and
   "TagFromServer". *)
  <* FATAL ANY *>
  BEGIN
    RETURN tm.next(clientTag, serverTag);
  END TagNext;

PROCEDURE TagFromClient(tm: TagMerger): RCSTag.T =
(* If you add any exceptions, add them to "TagNext" also. *)
  VAR
    tag: RCSTag.T;
  BEGIN
    REPEAT
      IF tm.clientTags = NIL THEN RETURN NIL END;
      tag := tm.clientTags.head;
      tm.clientTags := tm.clientTags.tail;
    UNTIL RCSRevNum.NumParts(tag.revNum) MOD 2 = tm.rem;
    RETURN tag;
  END TagFromClient;

PROCEDURE TagFromServer(tm: TagMerger): RCSTag.T =
(* If you add any exceptions, add them to "TagNext" also. *)
  VAR
    tag: RCSTag.T;
  BEGIN
    REPEAT
      IF NOT tm.iter.next(tag) THEN RETURN NIL END;
    UNTIL RCSRevNum.NumParts(tag.revNum) MOD 2 = tm.rem;
    RETURN tag;
  END TagFromServer;

(*****************************************************************************)
(* Sending regular (non-RCS) files. *)
(*****************************************************************************)

PROCEDURE RegularCompare(self: T;
                         sfr: SupFileRec.T;
                         name: Pathname.T;
                         clientSize: CARDINAL;
                         clientCksum: TEXT)
  RAISES {Error, Thread.Alerted, Wr.Failure} =
  CONST myName = "RegularCompare: ";
  VAR
    pathname := SupMisc.CatPath(sfr.serverPrefix, name);
    file: File.T;
    attr: FileAttr.T;
    rd: Rd.T;
    md5: MD5.T;
    buf: ARRAY [0..8191] OF CHAR;
    nRead, nGot: CARDINAL;
    serverCksum: TEXT;
  BEGIN
    Trace(self, myName);
    IF NOT CheckName(self, name) OR MaybeSendNode(self, sfr, name) THEN
      RETURN;
    END;

    TRY
      file := FS.OpenFileReadonly(pathname);
      rd := NEW(FileRd.T).init(file);
    EXCEPT OSError.E(list) =>
      WarnBoth(self, "Cannot open \"" & pathname
	& "\": " & ErrMsg.StrError(list));
      RETURN;
    END;
    TRY
      TRY
	attr := FileAttr.FromFile(file);
      EXCEPT OSError.E(list) =>
	RAISE Error("Cannot stat \"" & pathname & "\": " &
	  ErrMsg.StrError(list));
      END;

      md5 := MD5.New();

      IF clientSize > 0 AND FileAttr.GetSize(attr) >= clientSize THEN
	(* See if it's a simple append. *)
	nRead := 0;
	WHILE nRead < clientSize DO
	  WITH nWant = MIN(NUMBER(buf), clientSize - nRead) DO
	    TRY
	      nGot := Rd.GetSub(rd, SUBARRAY(buf, 0, nWant));
	    EXCEPT Rd.Failure(list) =>
	      RAISE Error("Read failure from \"" & pathname & "\": " &
		ErrMsg.StrError(list));
	    END;
	  END;
	  IF nGot = 0 THEN EXIT END;
	  md5.update(SUBARRAY(buf, 0, nGot));
	  INC(nRead, nGot);
	END;
	serverCksum := md5.clone().finish();  (* Non-destructive. *)

	IF nRead # clientSize OR NOT Text.Equal(serverCksum, clientCksum) THEN
	  (* We're going to have to send the entire file. *)
	  TRY
	    Rd.Seek(rd, 0);
	    md5 := MD5.New();
	  EXCEPT Rd.Failure(list) =>
	    RAISE Error("Cannot seek \"" & pathname & "\": " &
	      ErrMsg.StrError(list));
	  END;
	END;
      END;

      RegularUpdate(self, sfr, name, attr, rd, pathname, md5);
    FINALLY
      TRY
	Rd.Close(rd);
      EXCEPT Rd.Failure(list) =>
	RAISE Error("Cannot close \"" & pathname & "\": " &
	  ErrMsg.StrError(list));
      END;
    END;
  END RegularCompare;

PROCEDURE RegularUpdate(self: T;
                        sfr: SupFileRec.T;
                        name: TEXT;
			attr: FileAttr.T;
			rd: Rd.T;
			path: Pathname.T;
			md5: MD5.T)
  RAISES {Error, Thread.Alerted, Wr.Failure} =
  CONST myName = "RegularUpdate: ";
  VAR
    pos := Rd.Index(rd);
    size := FileAttr.GetSize(attr);
    nBytes: CARDINAL := size - pos;
  BEGIN
    Trace(self, myName, sfr.collection, ", ", name, ", ", path);
    SendExecutes(self, sfr, name, inAttic := FALSE, isCheckout := FALSE);
    IF pos = 0 THEN  (* Replacing entire file. *)
      self.proto.putCmd(self.wr, "R", name, AttrOrModTimeSize(self, sfr, attr));
    ELSE  (* Appending to the file. *)
      self.proto.putCmd(self.wr, "Z", name, more := TRUE);
      IF self.proto.v.hasFileAttrs THEN
	self.proto.putCmd(self.wr, NIL,
	  FileAttr.Encode(attr, support := self.proto.v.attrSupport,
	    ignore := sfr.attrIgnore),
	  more := TRUE);
      ELSE
	self.proto.putCmd(self.wr, NIL,
	  TokScan.EncodeTime(FileAttr.GetModTime(attr)),
	  Fmt.Unsigned(nBytes, 10),
	  more := TRUE);
      END;
      self.proto.putCmd(self.wr, NIL, Fmt.Unsigned(pos, 10));
    END;
    TRY
      SendCounted(rd, self.wr, nBytes, md5);
    EXCEPT Rd.Failure(list) =>
      RAISE Error("Read failure from \"" & path & "\": " &
	ErrMsg.StrError(list));
    END;
  END RegularUpdate;

PROCEDURE RegularSend(self: T;
		      sfr: SupFileRec.T;
		      name: Pathname.T;
		      isFixup: BOOLEAN)
  RAISES {Error, Thread.Alerted, Wr.Failure} =
  CONST
    CmdTab = ARRAY BOOLEAN,BOOLEAN OF TEXT{  (* inAttic, isFixup *)
      ARRAY BOOLEAN OF TEXT{ "A", "X" },     (* inAttic = FALSE *)
      ARRAY BOOLEAN OF TEXT{ "a", "x" }      (* inAttic = TRUE *)
    };
    MyName = "RegularSend: ";
  VAR
    pathname := SupMisc.CatPath(sfr.serverPrefix, name);
    origPathname := pathname;
    file: File.T;
    attr: FileAttr.T;
    attrText: TEXT;
    rd: Rd.T;
    inAttic: BOOLEAN;
    md5: MD5.T;
  BEGIN
    Trace(self, MyName, sfr.collection, " ", name);
    IF NOT CheckName(self, name) OR MaybeSendNode(self, sfr, name) THEN
      RETURN;
    END;

    IF SupMisc.IsRCS(pathname)
    AND self.clientClass.collectionIsPartiallyHidden(sfr.collection) THEN
      PartialRCSSend(self, sfr, name, isFixup);
      RETURN;
    END;

    TRY
      file := Attic.FSOpenFileReadonly(pathname);
      rd := NEW(FileRd.T).init(file);
    EXCEPT OSError.E(list) =>
      WarnBoth(self, "Cannot open \"" & pathname
	& "\": " & ErrMsg.StrError(list));
      RETURN;
    END;
    TRY
      TRY
	attr := FileAttr.FromFile(file);
      EXCEPT OSError.E(list) =>
	RAISE Error("Cannot stat \"" & pathname & "\": " &
	  ErrMsg.StrError(list));
      END;
      inAttic := pathname # origPathname;

      SendExecutes(self, sfr, name, inAttic := inAttic, isCheckout := FALSE);
      IF isFixup THEN
	attrText := AttrOrModTimeSize(self, sfr, attr);
      ELSE
	attrText := AttrOrModTimeSizeMode(self, sfr, attr);
      END;
      self.proto.putCmd(self.wr, CmdTab[inAttic, isFixup], name, attrText);

      md5 := MD5.New();

      TRY
	SendCounted(rd, self.wr, FileAttr.GetSize(attr), md5);
      EXCEPT Rd.Failure(list) =>
	RAISE Error("Read failure from \"" & pathname & "\": " &
	  ErrMsg.StrError(list));
      END;
    FINALLY
      TRY
	Rd.Close(rd);
      EXCEPT Rd.Failure(list) =>
	RAISE Error("Cannot close \"" & pathname & "\": " &
	  ErrMsg.StrError(list));
      END;
    END;
  END RegularSend;

PROCEDURE PartialRCSSend(self: T;
			 sfr: SupFileRec.T;
			 name: Pathname.T;
			 isFixup: BOOLEAN)
  RAISES {Error, Thread.Alerted, Wr.Failure} =
  CONST
    CmdTab = ARRAY BOOLEAN,BOOLEAN OF TEXT{  (* inAttic, isFixup *)
      ARRAY BOOLEAN OF TEXT{ "A", "X" },     (* inAttic = FALSE *)
      ARRAY BOOLEAN OF TEXT{ "a", "x" }      (* inAttic = TRUE *)
    };
  VAR
    pathname := SupMisc.CatPath(sfr.serverPrefix, name);
    origPathname := pathname;
    rf: RCSFile.T;
    dtab: SortedRCSDeltaTbl.T;
    di: RCSDeltaTbl.Iterator;
    ti: RCSFile.TagIterator;
    revNum: RCSRevNum.T;
    d1, d2: RCSDelta.T;
    tag: RCSTag.T;
    tagHidden: BOOLEAN;
    hiddenDeltas: RCSDeltaList.T := NIL;
    hiddenTags: RCSTagList.T := NIL;
    attr: FileAttr.T;
    attrText: TEXT;
    inAttic: BOOLEAN;
    signature: TEXT;
  BEGIN
    TRY
      TRY
	rf := Attic.RCSFileOpenReadonly(pathname);
      EXCEPT OSError.E(list) =>
	WarnBoth(self, "Cannot open \"" & pathname
	  & "\": " & ErrMsg.StrError(list));
	RETURN;
      END;

      TRY
	(* Enumerate the hidden deltas. *)
	dtab := AccessibleDeltas(rf, self.clientClass, sfr, self);
	di := RCSFile.IterateByNumber(rf);
	WHILE di.next(revNum, d1) DO
	  IF NOT dtab.get(revNum, d2) THEN
	    hiddenDeltas := RCSDeltaList.Cons(d1, hiddenDeltas);
	  END;
	END;

	(* Enumerate the hidden tags. *)
	ti := RCSFile.IterateTags(rf);
	WHILE ti.next(tag) DO
	  tagHidden := TRUE;
	  IF self.clientClass.inAllowedCollectionTags(sfr.collection, tag.name)
	  THEN
	    IF RCSTag.IsCVSBranch(tag)
	    AND self.clientClass.inAllowedCollectionBranches(sfr.collection,
	      tag.name)
	    OR TagIncluded(dtab, tag) THEN
	      tagHidden := FALSE;
	    END;
	  END;
	  IF tagHidden THEN
	    hiddenTags := RCSTagList.Cons(tag, hiddenTags);
	  END;
	END;

	(* Remove the hidden tags and deltas. *)
	WHILE hiddenTags # NIL DO
	  RCSFile.DeleteTag(rf, hiddenTags.head.name, hiddenTags.head.revNum);
	  hiddenTags := hiddenTags.tail;
	END;
	WHILE hiddenDeltas # NIL DO
	  RCSFile.DeleteDelta(rf, hiddenDeltas.head);
	  hiddenDeltas := hiddenDeltas.tail;
	END;

	(* Find the size of the edited file and insert it into the attributes. *)
	attr := RCSFile.GetAttr(rf);
	WITH wr = NEW(NullWr.T).init() DO
	  RCSFile.ToWr(rf, wr);
	  attr := FileAttr.Override(attr,
	    NEW(FileAttr.T).init(attr.fileType, size := Wr.Index(wr)));
	  Wr.Close(wr);
	END;

	inAttic := pathname # origPathname;
	SendExecutes(self, sfr, name, inAttic := inAttic, isCheckout := FALSE);
	IF isFixup THEN
	  attrText := AttrOrModTimeSize(self, sfr, attr);
	ELSE
	  attrText := AttrOrModTimeSizeMode(self, sfr, attr);
	END;
	self.proto.putCmd(self.wr, CmdTab[inAttic, isFixup], name, attrText);

	WITH md5wr = NEW(MD5Wr.T).init(self.wr, closeChild := FALSE) DO
	  RCSFile.ToWr(rf, md5wr);
	  Wr.Close(md5wr);
	  signature := md5wr.getSignature();
	END;
	SupMisc.PutCmd(self.wr, ".");
	IF signature # NIL THEN
	  SupMisc.PutCmd(self.wr, "5", signature);
	END;
      FINALLY
	TRY
	  RCSFile.Close(rf);
	EXCEPT OSError.E(list) =>
	  RAISE Error("Cannot close \"" & pathname & "\": " &
	    ErrMsg.StrError(list));
	END;
      END;
    EXCEPT RCSError.E(text) =>
      WarnBoth(self, "RCS file error in \"" & pathname & "\": " & text);
    END;
  END PartialRCSSend;

PROCEDURE SendCounted(rd: Rd.T;
                      wr: Wr.T;
		      count: CARDINAL;
		      md5: MD5.T)
  RAISES {Rd.Failure, Thread.Alerted, Wr.Failure} =
  VAR
    nSent: CARDINAL;
    md5Wr: MD5Wr.T;
    signature: TEXT;
  BEGIN
    IF md5 # NIL THEN
      md5Wr := NEW(MD5Wr.T).init(wr, md5 := md5, closeChild := FALSE);
      TRY
	nSent := RdCopy.ToWriter(rd, md5Wr, count);
      FINALLY
	Wr.Close(md5Wr);
      END;
      signature := md5Wr.getSignature();
    ELSE
      nSent := RdCopy.ToWriter(rd, wr, count);
      signature := NIL;
    END;

    IF nSent < count THEN  (* The file got shorter *)
      FOR i := 1 TO count-nSent DO  (* Pad it out *)
	Wr.PutChar(wr, '\000');
      END;
      SupMisc.PutCmd(wr, ".<");
    ELSE
      (* We used to raise an error if the file grew on the server
         while it was being transferred.  Now we just ignore that
         case and transfer the number of bytes originally decided
         upon, assuming we'll transfer the rest on the next update.
         The previous policy caused big problems for huge mail
         archive files which took a very long time to transfer but
         were also grown frequently on the server host. *)
      SupMisc.PutCmd(wr, ".");
    END;

    IF signature # NIL THEN
      SupMisc.PutCmd(wr, "5", signature);
    END;
  END SendCounted;

(*****************************************************************************)
(* Rsync updates. *)
(*****************************************************************************)

PROCEDURE RsyncCompare(self: T;
                       sfr: SupFileRec.T;
                       name: Pathname.T;
                       size: CARDINAL;
                       blockSize: CARDINAL)
  RAISES {Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted,
	  TokScan.Error, Wr.Failure} =
  VAR
    pathname := SupMisc.CatPath(sfr.serverPrefix, name);
    remainder: CARDINAL;       (* Bytes at tail after all full blocks. *)
    blockCount: CARDINAL;      (* Blocks including partial block at end. *)
    fullBlockCount: CARDINAL;  (* Full blocks, excluding partial at end. *)
    blocks: REF ARRAY OF RsyncBlock.T;
    ts: TokScan.T;
    rsum: Word.T;
    md5: MD5Digest.T;
    rsf: RsyncFile.T;
    di: RsyncFile.DiffIterator;
    br: RsyncFile.BlockRange;
    wr: Wr.T;
  BEGIN
    fullBlockCount := size DIV blockSize;
    remainder := size MOD blockSize;
    IF remainder = 0 THEN
      blockCount := fullBlockCount;
    ELSE
      blockCount := fullBlockCount + 1;
    END;

    (* NOTE: The client sends information about all blocks, including
       any partial block at the end of the file.  However, at present
       we do not use the information from the partial block -- we simply
       discard it.  We might use it in the future, though. *)

    blocks := NEW(REF ARRAY OF RsyncBlock.T, fullBlockCount);

    (* Read the block info. *)
    FOR i := 0 TO blockCount-1 DO
      ts := self.proto.getCmd(self.rd);
      rsum := ts.getInt("rolling checksum", 16);
      md5 := ts.getMD5("md5 checksum");
      ts.getEnd("end of rsync block info line");
      IF i < fullBlockCount THEN  (* Not the last, partial block. *)
	WITH b = blocks[i] DO
	  b.num := i;
	  b.rsum := rsum;
	  b.md5 := md5;
	END;
      END;
    END;
    ts := self.proto.getCmd(self.rd);
    ts.getLiteral(".");

    IF NOT CheckName(self, name) OR MaybeSendNode(self, sfr, name) THEN
      RETURN;
    END;

    TRY
      rsf := RsyncFile.Open(pathname, blockSize);
    EXCEPT OSError.E(l) =>
      WarnBoth(self, "Cannot open \"" & pathname
	& "\": " & ErrMsg.StrError(l));
      RETURN;
    END;
    TRY
      SendExecutes(self, sfr, name, inAttic := FALSE, isCheckout := FALSE);
      self.proto.putCmd(self.wr, "r",
	name,
	AttrOrModTime(self, sfr, rsf.attr),
	Fmt.Unsigned(blockSize, 10),
	RsyncFile.GetMD5(rsf));
      di := RsyncFile.IterateDiffs(rsf, blocks);
      LOOP
	wr := NEW(EscapedWr.T).init(self.wr, closeChild := FALSE);
	TRY
	  IF NOT di.next(wr, br) THEN EXIT END;
	FINALLY
	  Wr.Close(wr);
	END;
	Wr.PutText(self.wr, Fmt.Int(br.start)
	  & " " & Fmt.Int(br.count)
	  & "\n");
      END;
      self.proto.putCmd(self.wr, ".");
    FINALLY
      TRY
	RsyncFile.Close(rsf);
      EXCEPT OSError.E(l) =>
	RAISE Error("Cannot close \"" & pathname & "\": " &
	  ErrMsg.StrError(l));
      END;
    END;
  END RsyncCompare;

(*****************************************************************************)
(* Node operations. *)
(*****************************************************************************)

PROCEDURE NodeCompare(self: T;
                      sfr: SupFileRec.T;
		      name: Pathname.T)
  RAISES {Error, Thread.Alerted, Wr.Failure} =
  CONST myName = "NodeCompare: ";
  BEGIN
    Trace(self, myName, sfr.collection, " ", name);
    IF NOT CheckName(self, name) OR MaybeSendNode(self, sfr, name) THEN
      RETURN;
    END;
    (* Uh-oh.  The client has a node, but we don't. *)
    RegularSend(self, sfr, name, isFixup := FALSE);
  END NodeCompare;

PROCEDURE MaybeSendNode(self: T;
                        sfr: SupFileRec.T;
			name: Pathname.T): BOOLEAN
  RAISES {Thread.Alerted, Wr.Failure} =
(* If the file is a node, sends it to the client if the negotiated protocol
   allows that, then returns "TRUE".  Returns "FALSE" if the file is a
   regular file or does not exist at all. *)
  CONST myName = "MaybeSendNode: ";
  VAR
    pathname := SupMisc.CatPath(sfr.serverPrefix, name);
    origPathname := pathname;
    attr: FileAttr.T;
    cmd: TEXT;
    attrText: TEXT;
  BEGIN
    Trace(self, myName, sfr.collection, " ", name);
    TRY
      attr := Attic.FileAttrFromPathname(pathname,
	follow := GlobTree.Not(sfr.symlink).test(name));
      IF attr.fileType = FileAttr.FileType.File THEN RETURN FALSE END;
    EXCEPT OSError.E =>
      RETURN FALSE;
    END;

    (* It is a node. *)
    IF self.proto.v.hasFileAttrs THEN (* We can send it. *)
      IF pathname = origPathname THEN cmd := "N" ELSE cmd := "n" END;
      attrText := FileAttr.Encode(attr,
	support := self.proto.v.attrSupport,
	ignore := sfr.attrIgnore);
      SendExecutes(self, sfr, name, inAttic := FALSE, isCheckout := FALSE);
      Trace(self, myName, "command ", cmd, " ", name);
      self.proto.putCmd(self.wr, cmd, name, attrText);
    END;
    RETURN TRUE;
  END MaybeSendNode;

(*****************************************************************************)
(* Checkout mode operations. *)
(*****************************************************************************)

PROCEDURE CheckoutSend(self: T;
                       sfr: SupFileRec.T;
                       name: Pathname.T;
                       tag: TEXT;
                       date: TEXT;
		       deleteIfDead := FALSE;
		       isFixup := FALSE)
  RAISES {Error, Thread.Alerted, Wr.Failure} =
  CONST myName = "CheckoutSend: ";
  VAR
    pathname := SupMisc.CatPath(sfr.serverPrefix, name);
    origPathname := pathname;
    keywordName := name;
    inAttic: BOOLEAN;
    rf: RCSFile.T;
    delta: RCSDelta.T;
    attr: FileAttr.T;
    deltaText: RCSString.Iterator;
    xTag: TEXT;
    cmd: TEXT;
  BEGIN
    Trace(self, myName, sfr.collection, " ", name);
    IF NOT CheckName(self, name) THEN RETURN END;
    TRY
      rf := Attic.RCSFileOpenReadonly(pathname);
    EXCEPT
    | OSError.E(list) =>
	WarnBoth(self, "Cannot open \"" & pathname
	  & "\": " & ErrMsg.StrError(list));
	RETURN;
    | RCSError.E(text) =>
	WarnBoth(self, "RCS file error in \"" & pathname & "\": " & text);
	RETURN;
    END;
    TRY
      inAttic := pathname # origPathname;
      IF inAttic THEN
	keywordName := SupMisc.AtticName(keywordName);
      END;
      attr := RCSFile.GetAttr(rf);

      delta := GetCheckoutDelta(rf, tag, date);
      IF delta = NIL OR RCSDelta.Dead(delta, inAttic := inAttic) THEN
	(* File is non-existent or dead for this tag and date. *)
	IF deleteIfDead THEN cmd := "u" ELSE cmd := "c" END;
	self.proto.putCmd(self.wr, cmd,
	  name,
	  tag,
	  date,
	  AttrOrModTime(self, sfr, attr));
      ELSE  (* File is alive for this tag and date. *)
	IF NOT Text.Equal(tag, ".") THEN xTag := tag ELSE xTag := NIL END;
	TRY
	  deltaText := RCSDelta.GetText(delta);
	EXCEPT RCSError.E(text) =>
	  WarnBoth(self, "RCS file error in \"" & pathname & "\": " & text);
	  RETURN;
	END;
	deltaText := sfr.expander.expand(deltaText,
	  hideAttic := self.proto.v.hidesAtticInCVSHeader,
	  cvsRoot := sfr.keywordPrefix,
	  name := keywordName,
	  delta := delta,
	  tag := xTag);
	IF isFixup THEN cmd := "Y" ELSE cmd := "C" END;
	SendExecutes(self, sfr, name, inAttic := inAttic, isCheckout := TRUE);
	self.proto.putCmd(self.wr, cmd,
	  name,
	  tag,
	  date,
	  delta.revision,
	  delta.date,
	  AttrOrModTime(self, sfr, attr));
	SendEscaped(self.wr, deltaText, withChecksum := TRUE);
      END;
    FINALLY
      TRY
	RCSFile.Close(rf);
      EXCEPT OSError.E(list) =>
	RAISE Error("Cannot close \"" & pathname & "\": " &
	  ErrMsg.StrError(list));
      END;
    END;
  END CheckoutSend;

PROCEDURE UpdateFromChecksum(self: T;
                             sfr: SupFileRec.T;
                             name: Pathname.T;
                             tag: TEXT;
                             date: TEXT;
                             cksum: TEXT;
			     revNumHint: RCSRevNum.T := NIL)
  RAISES {Error, Thread.Alerted, Wr.Failure} =
  (* If "revNumHint" is given, then only that revision is checked. *)
  CONST myName = "UpdateFromChecksum: ";
  VAR
    pathname := SupMisc.CatPath(sfr.serverPrefix, name);
    origPathname := pathname;
    keywordName := name;
    rf: RCSFile.T;
    delta: RCSDelta.T;
    bp: RCSDelta.T;
    gotSum: TEXT;
    numToCheck: CARDINAL;
  BEGIN
    Trace(self, myName, sfr.collection, " ", name);
    IF NOT CheckName(self, name) THEN RETURN END;
    TRY
      rf := Attic.RCSFileOpenReadonly(pathname);
    EXCEPT
    | OSError.E(list) =>
	WarnBoth(self, "Cannot open \"" & pathname
	  & "\": " & ErrMsg.StrError(list));
	RETURN;
    | RCSError.E(text) =>
	WarnBoth(self, "RCS file error in \"" & pathname & "\": " & text);
	RETURN;
    END;
    TRY
      TRY
	IF pathname # origPathname THEN  (* File is in the Attic. *)
	  keywordName := SupMisc.AtticName(keywordName);
	END;
	IF revNumHint # NIL THEN  (* Check just the specified revision. *)
	  TRY
	    delta := RCSFile.GetDelta(rf, revNumHint);
	    numToCheck := 1;
	  EXCEPT RCSError.E =>  (* Client specified a non-existent revision. *)
	    delta := NIL;
	  END;
	ELSE  (* Search backward on the specified branch. *)
	  delta := GetCheckoutDelta(rf, tag, ".");
	  numToCheck := MaxChecksumRevisions;
	END;
	IF delta # NIL THEN
	  IF RCSRevNum.NumParts(delta.revision) > 2 THEN
	    bp := RCSFile.GetDelta(rf, 
	      RCSRevNum.Prefix(RCSRevNum.Prefix(delta.revision)));
	  ELSE
	    bp := NIL;
	  END;
	  REPEAT
	    gotSum := DeltaChecksum(self, sfr, delta, keywordName, tag);
	    DEC(numToCheck);
	    IF Text.Equal(gotSum, cksum) THEN EXIT END;
	    IF delta = bp OR numToCheck = 0 THEN
	      delta := NIL;  (* Give up. *)
	    ELSE
	      delta := RCSDelta.Predecessor(delta);
	    END;
	  UNTIL delta = NIL;
	END;
      EXCEPT RCSError.E(text) =>
	WarnBoth(self, "RCS file error in \"" & pathname & "\": " & text);
	RETURN;
      END;
    FINALLY
      TRY
	RCSFile.Close(rf);
      EXCEPT OSError.E(list) =>
	RAISE Error("Cannot close \"" & pathname & "\": " &
	  ErrMsg.StrError(list));
      END;
    END;

    IF delta # NIL THEN  (* We found the matching delta. *)
      CheckoutUpdate(self, sfr, name, tag, date, delta.revision, delta.date);
    ELSE  (* Just send the whole file. *)
      CheckoutSend(self, sfr, name, tag, date, deleteIfDead := TRUE);
    END;
  END UpdateFromChecksum;

PROCEDURE DeltaChecksum(self: T;
                        sfr: SupFileRec.T;
                        delta: RCSDelta.T;
			keywordName: Pathname.T;
			tag: TEXT): TEXT
  RAISES {RCSError.E} =
(* Computes the MD5 checksum of the given delta, and returns it. *)
  VAR
    md5: MD5.T;
    xTag: TEXT;
    iter: RCSString.Iterator;
    lineStr: RCSString.T;
    sum: TEXT;
  BEGIN
    IF NOT Text.Equal(tag, ".") THEN xTag := tag ELSE xTag := NIL END;
    iter := sfr.expander.expand(RCSDelta.GetText(delta),
      hideAttic := self.proto.v.hidesAtticInCVSHeader,
      cvsRoot := sfr.keywordPrefix,
      name := keywordName,
      delta := delta,
      tag := xTag);
    md5 := MD5.New();
    TRY
      WHILE iter.next(lineStr) DO
	md5.updateText(lineStr.toText());
      END;
    FINALLY
      sum := md5.finish();
    END;
    RETURN sum;
  END DeltaChecksum;

PROCEDURE CheckoutUpdate(self: T;
                         sfr: SupFileRec.T;
                         name: Pathname.T;
                         tag: TEXT;
                         date: TEXT;
                         oldRevNum: RCSRevNum.T;
			 oldRevDate: RCSDate.T)
  RAISES {Error, Thread.Alerted, Wr.Failure} =
  CONST myName = "CheckoutUpdate: ";
  VAR
    pathname := SupMisc.CatPath(sfr.serverPrefix, name);
    origPathname := pathname;
    inAttic: BOOLEAN;
    keywordName := name;
    rf: RCSFile.T;
    fromAtticFlag: TEXT;
    oldDelta, newDelta: RCSDelta.T;
    attr: FileAttr.T;
    parsedDeltas: ParsedDeltaList.T := NIL;
    diffBase: RCSDelta.T;
    oldLogLines: CARDINAL;
    xTag: TEXT;
    dText: RCSString.Iterator;
    md5: MD5.T;
    sLine: RCSString.T;
    cksum: TEXT;
    checkOutWholeThing := FALSE;
  BEGIN
    Trace(self, myName, sfr.collection, " ", name);
    IF NOT CheckName(self, name) THEN RETURN END;
    TRY
      rf := Attic.RCSFileOpenReadonly(pathname);
    EXCEPT
    | OSError.E(list) =>
	WarnBoth(self, "Cannot open \"" & pathname
	  & "\": " & ErrMsg.StrError(list));
	RETURN;
    | RCSError.E(text) =>
	WarnBoth(self, "RCS file error in \"" & pathname & "\": " & text);
	RETURN;
    END;
    TRY
      inAttic := pathname # origPathname;
      IF inAttic THEN
	keywordName := SupMisc.AtticName(keywordName);
      END;
      attr := RCSFile.GetAttr(rf);

      newDelta := GetCheckoutDelta(rf, tag, date);
      IF newDelta = NIL OR RCSDelta.Dead(newDelta, inAttic := inAttic) THEN
	(* File is non-existent or dead for this tag and date. *)
	self.proto.putCmd(self.wr, "u",
	  name,
	  tag,
	  date,
	  AttrOrModTime(self, sfr, attr));
      ELSE  (* File is alive for this tag and date. *)
	(* See whether the client already has the right revision number.
	   If the client sent the revDate, then verify it too. *)
	IF RCSRevNum.Equal(newDelta.revision, oldRevNum)
	AND (NOT self.proto.v.sendsRevDates OR
	     RCSDate.Equal(newDelta.date, oldRevDate))
	THEN
	  (* The RCS file's modTime has changed, but there have been
	     no changes affecting the client's checked-out version.
	     Just tell the client to update its notion of the RCS
	     file's modTime. *)
	  self.proto.putCmd(self.wr, "T",
	    name,
	    tag,
	    date,
	    newDelta.revision,
	    more := TRUE);
	  IF self.proto.v.sendsRevDates THEN
	    self.proto.putCmd(self.wr, NIL,
	      newDelta.date,
	      more := TRUE);
	  END;
	  self.proto.putCmd(self.wr, NIL,
	    AttrOrModTime(self, sfr, attr));
	ELSE
	  (* We have to send some deltas to the client. *)
	  TRY
	    oldDelta := RCSFile.GetDelta(rf, oldRevNum);
	    IF self.proto.v.sendsRevDates
	    AND NOT RCSDate.Equal(oldRevDate, oldDelta.date) THEN
	      (* We found the revision the client says he has.  But it
		 doesn't have the date that the client says it should
		 have.  Probably somebody completely replaced the RCS
		 file on the server. *)
	      RAISE RCSError.E("revDate mismatch for revision " & oldRevNum);
	    END;
	    parsedDeltas := DeltaPath(oldDelta, newDelta);

	    (* Count the number of log lines that the client will have to
	       remove from the old version of the file.  Note that trailing
	       blank lines are discarded. *)
	    oldLogLines := 0;
	    VAR
	      line: RCSString.T;
	      text: TEXT;
	      iter := RCSDelta.GetLog(oldDelta).iterate();
	      lineNum := 0;
	    BEGIN
	      WHILE iter.next(line) DO
		text := line.toText();
		IF NOT SupMisc.IsBlankLine(text) THEN
		  oldLogLines := lineNum + 1;
		END;
		INC(lineNum);
	      END;
	    END;

	    (* Compute the checksum. *)
	    IF Text.Equal(tag, ".") THEN xTag := NIL ELSE xTag := tag END;
	    dText := sfr.expander.expand(RCSDelta.GetText(newDelta),
	      hideAttic := self.proto.v.hidesAtticInCVSHeader,
	      cvsRoot := sfr.keywordPrefix,
	      name := keywordName,
	      delta := newDelta,
	      tag := xTag);
	    md5 := MD5.New();
	    TRY
	      WHILE dText.next(sLine) DO
		md5.updateText(sLine.toText());
	      END;
	    FINALLY
	      cksum := md5.finish();
	    END;
	  EXCEPT RCSError.E =>
            (* For one reason or another, we weren't able to come
               up with an update from the client's revision to the
               one we want.  This happened once, when the delta the
               client said it had didn't even exist in the RCS file.
               (Somebody had clobbered the RCS file.)  Just check out
               the desired revision from scratch.  We used to warn
               about this, but it put too much scary stuff into the
               server logs on those rare occasions when it happened. *)
            checkOutWholeThing := TRUE;
	  END;
	  IF NOT checkOutWholeThing THEN
	    (* We must pass along an indication of whether the RCS
	       file was in the Attic or not, because that affects the
	       expansion of the "Source" and "Header" RCS keywords. *)
	    IF pathname = origPathname THEN
	      fromAtticFlag := "0";
	    ELSE
	      fromAtticFlag := "1";
	    END;
	    SendExecutes(self, sfr, name, inAttic := FALSE, isCheckout := TRUE);
	    self.proto.putCmd(self.wr, "U",
	      name,
	      tag,
	      date,
	      oldRevNum,
	      fromAtticFlag,
	      Fmt.Int(oldLogLines),
	      RCSKeyword.EncodeExpand(rf.expand),
	      AttrOrModTime(self, sfr, attr),
	      cksum);

	    (* DANGER - Errors beyond this point are fatal. *)

	    diffBase := oldDelta;
	    REPEAT
	      WITH pd = parsedDeltas.head DO
		SendDelta(self, pd.delta, diffBase, pd.log, pd.text);
		diffBase := pd.delta;
	      END;
	      parsedDeltas := parsedDeltas.tail;
	    UNTIL parsedDeltas = NIL;

	    self.proto.putCmd(self.wr, ".");
	  END;
	END;
      END;
    FINALLY
      TRY
	RCSFile.Close(rf);
      EXCEPT OSError.E(list) =>
	RAISE Error("Cannot close \"" & pathname & "\": " &
	  ErrMsg.StrError(list));
      END;
    END;
    IF checkOutWholeThing THEN  (* We weren't able to edit the file. *)
      CheckoutSend(self, sfr, name, tag, date, deleteIfDead := TRUE);
    END;
  END CheckoutUpdate;

PROCEDURE DeltaPath(delta1, delta2: RCSDelta.T): ParsedDeltaList.T
  RAISES {RCSError.E} =
(* Returns a list of parsed deltas leading from "delta1" to "delta2".
   The text for each one is filled in, but the log is present only for
   the last one. *)
  VAR
    d1 := delta1;
    d2 := delta2;
    list1, list2: ParsedDeltaList.T := NIL;
    l: ParsedDeltaList.T;
    diffBase: RCSDelta.T;
  BEGIN
    (* Find the paths from each delta back to a common branch. *)
    WITH commLen = SupMisc.CommonLength(d1.revision, d2.revision) DO
      (* Notice that the following loops are not quite the same.  That is
	 because we want to include "delta2" in our final list, but
	 not "delta1". *)
      WHILE Text.Length(RCSRevNum.Prefix(d1.revision)) > commLen
      AND NOT RCSRevNum.IsTrunk(d1.revision) DO
	d1 := RCSDelta.Predecessor(d1);
	list1 := ParsedDeltaList.Cons(ParsedDelta.T{delta := d1}, list1);
      END;
      WHILE Text.Length(RCSRevNum.Prefix(d2.revision)) > commLen
      AND NOT RCSRevNum.IsTrunk(d2.revision) DO
	list2 := ParsedDeltaList.Cons(ParsedDelta.T{delta := d2}, list2);
	d2 := RCSDelta.Predecessor(d2);
      END;
    END;

    (* Find the connecting path along the common branch. *)
    WITH c = RCSRevNum.Compare(d1.revision, d2.revision) DO
      IF c < 0 THEN
	REPEAT
	  list2 := ParsedDeltaList.Cons(ParsedDelta.T{delta := d2}, list2);
	  d2 := RCSDelta.Predecessor(d2);
	UNTIL d2 = d1;
      ELSIF c > 0 THEN
	REPEAT
	  d1 := RCSDelta.Predecessor(d1);
	  list1 := ParsedDeltaList.Cons(ParsedDelta.T{delta := d1}, list1);
	UNTIL d1 = d2;
      END;
    END;

    (* Combine the paths, by popping entries from "list1" and pushing them
       onto "list2". *)
    WHILE list1 # NIL DO
      l := list1;
      list1 := list1.tail;
      l.tail := list2;
      list2 := l;
    END;

    (* Traverse the final path, filling the texts for all the deltas, and
       the log for the last one. *)
    l := list2;
    diffBase := delta1;
    WHILE l # NIL DO
      WITH pd = l.head DO
	IF l.tail = NIL THEN  (* Last delta in the list. *)
	  pd.log := RCSDelta.GetLog(pd.delta).iterate();
	END;
	pd.text := RCSDelta.GetText(pd.delta, diffBase);
	diffBase := pd.delta;
      END;
      l := l.tail;
    END;

    RETURN list2;
  END DeltaPath;

PROCEDURE GetCheckoutDelta(rf: RCSFile.T;
                           tag: TEXT;
                           date: RCSDate.T): RCSDelta.T =
  BEGIN
    IF Text.Equal(tag, ".") THEN tag := NIL END;
    IF Text.Equal(date, ".") THEN date := NIL END;
    TRY
      RETURN RCSFile.GetTagDelta(rf, tag, date);
    EXCEPT RCSError.E =>
      RETURN NIL;
    END;
  END GetCheckoutDelta;

(*****************************************************************************)

PROCEDURE SendExecutes(self: T;
                       sfr: SupFileRec.T;
		       name: Pathname.T;
		       inAttic: BOOLEAN;
		       isCheckout: BOOLEAN)
  RAISES {Thread.Alerted, Wr.Failure} =
  CONST myName = "SendExecutes: ";
  VAR
    clientName: Pathname.T := NIL;
    er: ExecRec.T;
  BEGIN
    Trace(self, myName, sfr.collection, " ", name);
    FOR i := 0 TO sfr.executes.size()-1 DO
      er := sfr.executes.get(i);
      IF er.pattern.test(name) THEN
	IF clientName = NIL THEN  (* First match. *)
	  IF isCheckout THEN
	    clientName := SupMisc.CheckoutName(name);
	  ELSIF inAttic THEN
	    clientName := SupMisc.AtticName(name);
	  ELSE
	    clientName := name;
	  END;
	END;
	self.proto.putCmd(self.wr, "E", clientName, er.command);
      END;
    END;
  END SendExecutes;

(*****************************************************************************)

(* Legacy protocol support. *)

PROCEDURE AttrOrModTime(self: T;
                        sfr: SupFileRec.T;
			attr: FileAttr.T): TEXT =
  BEGIN
    IF self.proto.v.hasFileAttrs THEN
      RETURN FileAttr.Encode(attr,
	support := self.proto.v.attrSupport,
	ignore := sfr.attrIgnore);
    ELSE
      RETURN TokScan.EncodeTime(FileAttr.GetModTime(attr));
    END;
  END AttrOrModTime;

PROCEDURE AttrOrModTimeSize(self: T;
                            sfr: SupFileRec.T;
			    attr: FileAttr.T): TEXT =
  BEGIN
    IF self.proto.v.hasFileAttrs THEN
      RETURN FileAttr.Encode(attr,
	support := self.proto.v.attrSupport,
	ignore := sfr.attrIgnore);
    ELSE
      RETURN TokScan.EncodeTime(FileAttr.GetModTime(attr))
	& " " & Fmt.Unsigned(FileAttr.GetSize(attr), 10);
    END;
  END AttrOrModTimeSize;

PROCEDURE AttrOrModTimeSizeMode(self: T;
                                sfr: SupFileRec.T;
				attr: FileAttr.T): TEXT =
  BEGIN
    IF self.proto.v.hasFileAttrs THEN
      (* Note that we never ignore the file mode, since we are creating
	 a brand new file in this case. *)
      RETURN FileAttr.Encode(attr,
	support := self.proto.v.attrSupport,
	ignore := sfr.attrIgnore - FileAttr.AttrTypes{FileAttr.AttrType.Mode});
    ELSE
      RETURN TokScan.EncodeTime(FileAttr.GetModTime(attr))
	& " " & Fmt.Unsigned(FileAttr.GetSize(attr), 10)
	& " " & Fmt.Unsigned(FileAttr.GetMode(attr), 10);
    END;
  END AttrOrModTimeSizeMode;

(*****************************************************************************)

PROCEDURE Warning(self: T; msg: TEXT) =
(* Logs a warning message. *)
  BEGIN
    IF self.logger # NIL THEN
      Logger.Put(self.logger, Logger.Priority.Warning, msg);
    END;
  END Warning;

PROCEDURE WarnBoth(self: T; msg: TEXT)
(* Logs a warning message and sends it to the client. *)
  RAISES {Thread.Alerted, Wr.Failure} =
  BEGIN
    Warning(self, msg);
    WarnClient(self, msg);
  END WarnBoth;

PROCEDURE WarnClient(self: T; msg: TEXT)
(* Sends a warning message to the client. *)
  RAISES {Thread.Alerted, Wr.Failure} =
  BEGIN
    self.proto.putCmd(self.wr, "!", msg);
    Wr.Flush(self.wr);
  END WarnClient;

PROCEDURE Trace(self: T; m1, m2, m3, m4, m5, m6, m7, m8: TEXT := NIL;
                level := 1) =
(* Logs a trace message. *)
  BEGIN
    IF traceLevel >= level THEN
      VAR 
        msg := "";
      BEGIN
        IF m1 # NIL THEN msg := msg & m1 END;
        IF m2 # NIL THEN msg := msg & m2 END;
        IF m3 # NIL THEN msg := msg & m3 END;
        IF m4 # NIL THEN msg := msg & m4 END;
        IF m5 # NIL THEN msg := msg & m5 END;
        IF m6 # NIL THEN msg := msg & m6 END;
        IF m7 # NIL THEN msg := msg & m7 END;
        IF m8 # NIL THEN msg := msg & m8 END;
        IF self.logger # NIL THEN
          Logger.Debug(self.logger, msg);
        ELSE
          IO.Put("logger = NIL: " & msg & "\n");
        END;
      END;
    END;
  END Trace;

BEGIN
END RCSComp.
