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
 * $Id$ *)

MODULE TreeList;

IMPORT
  Attic, CVProto, ErrMsg, FileAttr, FileAttrSeq, FileRd, FileStatus,
  GzipWr, Logger, OSError, OSErrorPosix, Pathname, Rd, Reaper,
  StreamWr, SupFileRec, SupFileRecSeq, SupMisc, Text, Thread, TokScan,
  Uerror, Wr;

EXCEPTION Error(TEXT);

REVEAL
  T = Public BRANDED OBJECT
      proto: CVProto.T;
      wireWr: StreamWr.T;	(* Raw writer. *)
      wr: StreamWr.T;		(* Currently active writer. *)
      collections: SupFileRecSeq.T;
      compLevel: [0..9];
      reaper: Reaper.T;
      stats: Stats;
      trace: Logger.T;
    OVERRIDES
      apply := Apply;
      init := Init;
    END;

PROCEDURE Apply(self: T): REFANY =
  BEGIN
    TRY
      TRY
	IF self.stats # NIL THEN
	  self.stats.start();
	END;

	FOR i := 0 TO self.collections.size()-1 DO
	  ListCollection(self, self.collections.get(i));
	END;

	Trace(self, "");
	self.proto.putCmd(self.wr, ".");
	Wr.Flush(self.wr);
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
	  message := "TreeList failed: " & msg);
    | Thread.Alerted =>
	RETURN NEW(SupMisc.ThreadStatus,
	  status := SupMisc.ExitCode.Failure,
	  message := "TreeList failed: Interrupted");
    | Wr.Failure(list) =>
	RETURN NEW(SupMisc.ThreadStatus,
	  status := SupMisc.ExitCode.TransientFailure,
	  message := "TreeList failed: Network write failure: "
	    & ErrMsg.StrError(list));
    END;
  END Apply;

PROCEDURE Init(self: T;
               proto: CVProto.T;
               wr: StreamWr.T;
               collections: SupFileRecSeq.T;
	       compLevel: [-1..9] := -1;
	       reaper: Reaper.T := NIL;
	       stats: Stats := NIL;
               trace: Logger.T := NIL): T =
  BEGIN
    self.proto := proto;
    self.wireWr := wr;
    self.wr := wr;
    self.collections := collections;
    IF compLevel = -1 THEN compLevel := SupMisc.DefaultCompression END;
    self.compLevel := compLevel;
    self.reaper := reaper;
    self.stats := stats;
    self.trace := trace;
    RETURN self;
  END Init;

PROCEDURE ListCollection(self: T;
                         sfr: SupFileRec.T)
  RAISES {Error, Thread.Alerted, Wr.Failure} =
  BEGIN
    IF NOT SupFileRec.Option.Skip IN sfr.options THEN
      self.proto.putCmd(self.wr, "COLL", sfr.collection, sfr.release);
      Wr.Flush(self.wr);

      IF SupFileRec.Option.Compress IN sfr.options THEN
	TRY
	  self.wr := NEW(GzipWr.T).init(self.wireWr, level := self.compLevel,
	    closeChild := FALSE);
	EXCEPT OSError.E(list) =>
	  RAISE Error("Cannot create Gzip writer: " & ErrMsg.StrError(list));
	END;
      END;
      TRY
	PutCollectionList(self, sfr);

	self.proto.putCmd(self.wr, ".");
	Wr.Flush(self.wr);

	IF SupFileRec.Option.Compress IN sfr.options THEN
	  Wr.Close(self.wr);
	END;
      FINALLY
	IF SupFileRec.Option.Compress IN sfr.options THEN
	  GzipWr.Cleanup(self.wr);
	  self.wr := self.wireWr;
	END;
      END;
    END;
  END ListCollection;

(*****************************************************************************)
(* Listing a collection. *)
(*****************************************************************************)

PROCEDURE PutCollectionList(self: T;
                            sfr: SupFileRec.T)
  RAISES {Error, Thread.Alerted, Wr.Failure} =
  VAR
    sfPath := SupMisc.CatPath(
      SupMisc.ResolvePath(sfr.clientBase, sfr.clientCollDir),
	SupMisc.CatPath(sfr.collection, SupMisc.StatusFileName(sfr)));
    sfReader: FileStatus.Reader;
    fs: FileStatus.T;
    attr: FileAttr.T;
    sendCmd: TEXT;
    sendName: Pathname.T;
    sendAttr: FileAttr.T;
    needToFlush: BOOLEAN;
    path, origPath: Pathname.T;
    attrStack := NEW(FileAttrSeq.T).init(30);  (* Per directory level. *)
    depth: CARDINAL := 0;
    pruneDepth: CARDINAL := LAST(CARDINAL);
    protoOffset, wireOffset: LONGREAL;
  BEGIN
    TRY
      TRY
	sfReader := FileStatus.FromRd(FileRd.Open(sfPath));
      EXCEPT OSError.E => (* Just use an empty source. *)
	sfReader := FileStatus.FromNull();
      END;
      TRY
	depth := 0;
	pruneDepth := LAST(CARDINAL);
	IF self.stats # NIL THEN
	  LOCK self.stats DO
	    protoOffset := StreamWr.ByteCount(self.wr) - self.stats.bytesOut;
	    wireOffset :=
	      StreamWr.ByteCount(self.wireWr) - self.stats.wireBytesOut;
	  END;
	END;
	LOOP
	  TRY fs := sfReader.get() EXCEPT Rd.EndOfFile => EXIT END;
	  sendCmd := NIL;
	  sendName := NIL;
	  sendAttr := NIL;
	  needToFlush := FALSE;

	  CASE fs.type OF
	  | FileStatus.Type.DirDown =>
	      INC(depth);
	      IF depth < pruneDepth THEN  (* Not currently pruning. *)
		IF sfr.dirFilter.test(fs.name) THEN
		  path := SupMisc.CatPath(sfr.clientPrefix, fs.name);
		  TRY
		    (* We don't send the directory attributes on the way
		       down the tree.  When descending, existence is all
		       that matters.  Nevertheless, on the way down is
		       the time to check the attributes, in case the
		       directory doesn't exist. *)
		    IF SupFileRec.Option.TrustStatusFile IN sfr.options THEN
		      attr := NEW(FileAttr.T).init(FileAttr.FileType.Directory);
		    ELSE
		      attr := FileAttr.FromPathname(path, follow := FALSE);
		    END;

		    IF attr.fileType = FileAttr.FileType.SymLink THEN
		      (* It is recorded as a directory, but it is
			 really a symlink.  Check for the case in
			 which it is a symlink pointing to a
			 directory.  In that case, follow
			 the link.  Older versions of the client
			 followed such links, and we don't want to
			 replace the symlink with a new copy of
			 the entire subtree.  That might fill up
			 the wrong filesystem. *)
		      TRY
			WITH fa = FileAttr.FromPathname(path, follow := TRUE) DO
			  IF fa.fileType = FileAttr.FileType.Directory THEN
			    attr := fa;  (* Follow link to the directory. *)
			    IF NOT SupFileRec.Option.CheckoutMode IN sfr.options
			    THEN
			      Warn(self, "\"" & fs.name & "\" should be a"
				& " directory, but is a symlink to a directory"
				& " -- check your prefix setting");
			    END;
			  END;
			END;
		      EXCEPT OSError.E => (* Link to nowhere. *) END;
		    END;

		    IF attr.fileType = FileAttr.FileType.Directory THEN
		      (* It really is a directory. *)
		      attrStack.addhi(attr);  (* Save attributes for later. *)
		      IF depth <= 3 THEN Trace(self, fs.name) END;
		      sendCmd := "D";
		      sendName := fs.name;
		    ELSE  (* It is not really a directory. *)
		      pruneDepth := depth;  (* Start pruning. *)
		      (* Report it as something bogus so that it will be
			 replaced. *)
		      sendCmd := "F";
		      sendName := fs.name;
		      sendAttr := FileAttr.Bogus;
		    END;
		  EXCEPT OSError.E =>
		    (* The directory doesn't exist.  Prune everything
		       below it. *)
		    pruneDepth := depth;
		  END;
		ELSE  (* Start pruning. *)
		  pruneDepth := depth;
		END;
	      END;
	  | FileStatus.Type.DirUp =>
	      IF depth < pruneDepth THEN  (* Not currently pruning. *)
		needToFlush := TRUE;  (* FIXME - Be smarter. *)
		sendCmd := "U";
		IF SupFileRec.Option.TrustStatusFile IN sfr.options THEN
		  EVAL attrStack.remhi();
		  attr := fs.clientAttr;
		ELSE
		  attr := attrStack.remhi();
		END;
		IF self.proto.v.dirsAreExplicit THEN
		  IF FileAttr.Equal(attr, fs.clientAttr) THEN
		    sendAttr := attr;
		  ELSE  (* Force the attributes to be updated. *)
		    sendAttr := FileAttr.Bogus;
		  END;
		END;
		IF self.stats # NIL THEN  (* Update the statistics. *)
		  LOCK self.stats DO
		    self.stats.bytesOut :=
		      StreamWr.ByteCount(self.wr) - protoOffset;
		    self.stats.wireBytesOut :=
		      StreamWr.ByteCount(self.wireWr) - wireOffset;
		  END;
		  self.stats.update();
		END;
	      ELSIF depth = pruneDepth THEN  (* Finished pruning. *)
		pruneDepth := LAST(CARDINAL);
	      END;
	      DEC(depth);
	  | FileStatus.Type.CheckoutLive =>
	      IF depth < pruneDepth THEN  (* Not currently pruning. *)
		IF SupFileRec.Option.CheckoutMode IN sfr.options
		AND sfr.fileFilter.test(fs.name) THEN
		  path := SupMisc.CatPath(sfr.clientPrefix,
		    SupMisc.CheckoutName(fs.name));
		  TRY
		    sendCmd := "F";
		    sendName := fs.name;
		    IF SupFileRec.Option.TrustStatusFile IN sfr.options THEN
		      attr := fs.clientAttr;
		    ELSE
		      attr := FileAttr.FromPathname(path, follow := FALSE);
		    END;
		    IF FileAttr.Equal(attr, fs.clientAttr)
		    AND FileAttr.Equal(attr,
		      FileAttr.ForCheckout(fs.serverAttr, sfr.umask))
		    AND Text.Equal(sfr.checkoutTag, fs.tag)
		    AND Text.Equal(sfr.checkoutDate, fs.date) THEN
		      (* The file corresponds to the information we have
			 recorded about it, and its mode is correct for
			 the requested umask setting. *)
		      sendAttr := fs.serverAttr;
		    ELSE
		      (* Either the file has been touched, or we are
                         asking for a different revision than the one
                         we recorded information about, or its mode
                         isn't right (because it was last updated
                         using a version of CVSup that wasn't so
                         strict about modes). *)
		      sendAttr := FileAttr.Bogus;
		    END;
		    (* If the protocol supports preserving
                       checkout-mode file permissions but we don't
                       have any recorded information about the file,
                       it means that this is the user's first update
                       since upgrading to a CVSup version that
                       preserves checkout-mode permissions.  For that
                       case, we report bogus attributes to force
                       the system to make sure the permissions are
                       correct. *)
		    IF self.proto.v.clientSendsUmask AND NOT
		      FileAttr.AttrType.Mode IN FileAttr.GetMask(fs.clientAttr)
		    THEN
		      sendAttr := FileAttr.Bogus;
		    END;
		  EXCEPT OSError.E =>
		    (* According to the checkouts file we should have this
		       file, but we don't.  Maybe the user deleted the file,
		       or maybe the checkouts file is wrong.  List the file
		       with bogus attributes to cause the server to get
		       things back in sync again. *)
		    sendAttr := FileAttr.Bogus;
		  END;
		END;
	      END;
	  | FileStatus.Type.CheckoutDead =>
	      IF depth < pruneDepth THEN  (* Not currently pruning. *)
		IF SupFileRec.Option.CheckoutMode IN sfr.options
		AND sfr.fileFilter.test(fs.name) THEN
		  path := SupMisc.CatPath(sfr.clientPrefix,
		    SupMisc.CheckoutName(fs.name));
		  TRY
		    IF SupFileRec.Option.TrustStatusFile IN sfr.options THEN
		      OSErrorPosix.Raise0(Uerror.ENOENT);
		    ELSE
		      attr := FileAttr.FromPathname(path, follow := FALSE);
		      (* The file exists.  Make sure it is really a file,
		         and not a directory.  If it is a directory it
			 doesn't correspond to this checkouts file record,
			 so it should be ignored. *)
		      IF attr.fileType = FileAttr.FileType.Directory THEN
			OSErrorPosix.Raise0(Uerror.ENOENT);
		      END;
		    END;
		    (* We shouldn't have this file, but we do.  Report it to
		       the server, which will either send a deletion request,
		       or (if the file has come alive) send the correct
		       version. *)
		    sendCmd := "F";
		    sendName := fs.name;
		    sendAttr := FileAttr.Bogus;
		  EXCEPT OSError.E =>
		    (* It is correct that we don't have this file.  List
		       it, so that the server can determine whether it
		       has come alive. *)
		    sendCmd := "f";
		    sendName := fs.name;
		    IF Text.Equal(sfr.checkoutTag, fs.tag)
		    AND Text.Equal(sfr.checkoutDate, fs.date) THEN
		      sendAttr := fs.serverAttr;
		    ELSE
		      sendAttr := FileAttr.Bogus;
		    END;
		  END;
		END;
	      END;
	  | FileStatus.Type.FileLive, FileStatus.Type.FileDead =>
	      IF depth < pruneDepth THEN  (* Not currently pruning. *)
		IF NOT SupFileRec.Option.CheckoutMode IN sfr.options THEN
		  path := SupMisc.CatPath(sfr.clientPrefix, fs.name);
		  origPath := path;
		  TRY
		    IF SupFileRec.Option.TrustStatusFile IN sfr.options THEN
		      attr := fs.clientAttr;
		      IF fs.type = FileStatus.Type.FileDead THEN
			path := SupMisc.AtticName(path);
		      END;
		    ELSE
		      attr := Attic.FileAttrFromPathname(path, follow := FALSE);
		    END;
		    IF FileAttr.AttrType.FileType IN
		    self.proto.v.attrSupport[attr.fileType] THEN
		      (* Both the client and the server can handle this file
			 type, so list it to the server. *)
		      IF path = origPath THEN  (* Live. *)
			IF sfr.fileFilter.test(fs.name) THEN
			  sendCmd := "F";
			END;
		      ELSE  (* Dead. *)
			IF sfr.fileFilter.test(SupMisc.AtticName(fs.name)) THEN
			  sendCmd := "f";
			END;
		      END;
		    END;
		    IF sendCmd # NIL THEN  (* We are listing this file. *)
		      sendName := fs.name;
		      IF FileAttr.Equal(attr, fs.clientAttr) THEN
			(* The recorded info corresponds to the actual file.
			   We send the attributes from the file itself, since
			   they may contain more information than what we
			   recorded. *)
			sendAttr := attr;
			(* If it is an RCS file, and we are using "loose"
			   equality for RCS files, then the sizes may
			   disagree because of harmless differences in
			   white space. *)
			IF SupMisc.IsRCS(fs.name)
			AND NOT SupFileRec.Option.NoRCS IN sfr.options
			AND NOT SupFileRec.Option.StrictCheckRCS IN sfr.options
			AND self.proto.v.hasLooseRCSCheck THEN
			  sendAttr := FileAttr.MaskOut(sendAttr,
			    FileAttr.AttrTypes{ FileAttr.AttrType.Size });
			END;
		      ELSE
			(* The file's attributes disagree with what we set them
			   to the last time we updated the file.  Perhaps the
			   user has edited the file in the meantime.  We return
			   a set of attributes with an impossible modTime
			   rather than the actual one.  This forces a full
			   comparison to be done on the file.

			   We used to return the actual modtime, and that works
			   virtually all the time.  But if by chance the user
			   edited the file at the precise same time that it
			   was modified on the server, then we would have
			   missed updating it.  The current technique avoids
			   any possibility of that, at very little cost. *)
			sendAttr := FileAttr.Bogus;
		      END;
		    END;
		  EXCEPT OSError.E =>
		    (* According to the checkouts file we should have this
		       file, but we don't.  Maybe the user deleted the file,
		       or maybe the checkouts file is wrong.  List the file
		       with bogus attributes to cause the server to get
		       things back in sync again. *)
		    sendCmd := "F";
		    sendName := fs.name;
		    sendAttr := FileAttr.Bogus;
		  END;
		END;
	      END;
	  END;

	  IF sendCmd # NIL THEN  (* List the file to the server. *)
	    IF Thread.TestAlert() THEN
	      RAISE Thread.Alerted;
	    END;
	    self.proto.putCmd(self.wr, sendCmd, more := TRUE);
	    IF sendName # NIL THEN
	      self.proto.putCmd(self.wr, NIL, SupMisc.PathLast(sendName),
		more := TRUE);
	    END;
	    IF sendAttr # NIL THEN  (* Send the attributes too. *)
	      IF self.proto.v.hasFileAttrs THEN
		IF NOT self.proto.v.clientSendsUmask AND sfr.umask # 0 THEN
		  (* We are talking to an old server which doesn't
                     know how to consider the client's umask when
                     comparing file modes, and we are using a umask
                     that is not a no-op.  Therefore we must not send
                     the file modes.  If we did, the server might note
                     a modes mismatch but be unable to correct it --
		     i.e., the file would be fruitlessly "updated" every
		     time. *)
		  sendAttr := FileAttr.MaskOut(sendAttr,
		    FileAttr.AttrTypes{ FileAttr.AttrType.Mode });
		END;
		self.proto.putCmd(self.wr, NIL,
		  FileAttr.Encode(sendAttr,
		    support := self.proto.v.attrSupport,
		    ignore := sfr.attrIgnore),
		  more := TRUE);
	      ELSE
		self.proto.putCmd(self.wr, NIL,
		  TokScan.EncodeTime(FileAttr.GetModTime(sendAttr)),
		  more := TRUE);
	      END;
	    END;
	    self.proto.putCmd(self.wr, NIL);
	    IF needToFlush THEN Wr.Flush(self.wr) END;
	  END;
	END;
	<* ASSERT depth = 0 *>
      FINALLY
	sfReader.close();
      END;
    EXCEPT
    | FileStatus.Error(msg) =>
	RAISE Error("Error in \"" & sfPath & "\": " & msg
	  & ".  Delete it and try again.");
    | Rd.Failure(list) =>
	RAISE Error("Read failure from \"" & sfPath & "\": "
	  & ErrMsg.StrError(list));
    END;
  END PutCollectionList;

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

PROCEDURE Warn(self: T; msg: TEXT) =
  BEGIN
    IF self.trace # NIL THEN
      Logger.Put(self.trace, Logger.Priority.Warning, msg);
    END;
  END Warn;

PROCEDURE Trace(self: T; msg: TEXT) =
  BEGIN
    IF self.trace # NIL THEN
      Logger.Put(self.trace, Logger.Priority.Info, msg);
    END;
  END Trace;

BEGIN
END TreeList.
