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
 * $Id: TreeComp.m3,v 1.1.1.1 2009-04-09 17:01:51 jkrell Exp $ *)

MODULE TreeComp;

IMPORT
  ClientClass, CVProto, CVTree, ErrMsg, FileAttr, FileID, FileInfo,
  FileInfoMerger, FileRd, FileStatus, GlobTree, GzipRd, GzipWr, IO,
  LinkTbl, Logger, OSError, PathComp, Pathname, Rd, Reaper, StreamRd,
  StreamWr, SupFileRec, SupFileRecSeq, SupMisc, Text, Thread, Time,
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
    clientClass: ClientClass.T;
    compLevel: [0..9];
    reaper: Reaper.T;
    logger: Logger.T;
  OVERRIDES
    apply := Apply;
    init := Init;
  END;

PROCEDURE Apply(self: T): REFANY =
  VAR
    ts: TokScan.T;
    collection, release: TEXT;
    initialBytesIn, initialBytesOut: LONGREAL;
  BEGIN
    TRY
      TRY
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
      FINALLY
	IF self.reaper # NIL THEN
	  Reaper.Dying(self.reaper);
	END;
      END;
    EXCEPT
    | CVTree.Error(msg) =>
      RETURN msg;
    | Error(msg) =>
      RETURN msg;
    | Rd.EndOfFile =>
      RETURN "Premature EOF from client";
    | Rd.Failure(list) =>
      RETURN "Network read failure: " & ErrMsg.StrError(list);
    | Thread.Alerted =>
      RETURN "Interrupted";
    | TokScan.Error(msg) =>
      RETURN "TreeComp protocol error: " & msg;
    | Wr.Failure(list) =>
      RETURN "Network write failure: " & ErrMsg.StrError(list);
    END;
    RETURN NIL;
  END Apply;

PROCEDURE CompCollection(self: T; sfr: SupFileRec.T)
  RAISES {CVTree.Error, Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted,
	  TokScan.Error, Wr.Failure} =
  VAR
    compress: BOOLEAN;
    merger: Merger;
  BEGIN
    sfr.scanTime := Time.Now();

    self.proto.putCmd(self.wr, "COLL",
      sfr.collection,
      sfr.release,
      TokScan.EncodeTime(sfr.scanTime));
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
	merger := NEW(Merger).init(self, sfr);
	TRY
	  IF SupFileRec.Option.CheckoutMode IN sfr.options THEN
	    CompCheckoutMode(self, sfr, merger);
	  ELSE
	    CompCVSMode(self, sfr, merger);
	  END;
	FINALLY
	  merger.close();
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

PROCEDURE CompCVSMode(self: T; sfr: SupFileRec.T; merger: Merger)
  RAISES {Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted,
	  TokScan.Error, Wr.Failure} =
  CONST myName = "CompCVSMode: ";
  VAR
    linkTbl: LinkTbl.T := NIL;
    clientFile, serverFile: FileInfo.T;
  BEGIN
    Trace(self, myName, " ", sfr.collection);
    IF self.proto.v.hasHardLinks THEN
      linkTbl := NEW(LinkTbl.Default).init();
    END;

    WHILE GetNext(merger, clientFile, serverFile) DO
      IF clientFile = NIL THEN  (* Add file on client. *)
	IF NOT SupFileRec.Option.DoDeletesOnly IN sfr.options THEN
	  CASE serverFile.type OF
	  | FileInfo.Type.DirDown =>  (* Create directory. *)
              Trace(self, myName, " Create directory ", serverFile.name);
	      self.proto.putCmd(self.wr, "I", serverFile.name);
	  | FileInfo.Type.DirUp =>  (* Set directory attributes. *)
              Trace(self, myName, " Set directory attributes ",
                serverFile.name);
	      self.proto.putCmd(self.wr, "J",
		serverFile.name,
		EncodeAttr(self, sfr, serverFile.attr));
	  | FileInfo.Type.Live, FileInfo.Type.Dead =>
	      (* Create the file if it doesn't already exist with the proper
		 attributes. *)
	      VAR
		cmd := "T";
		sendAttr := serverFile.attr;
	      BEGIN
                Trace(self, myName, " Create file with proper attributes ",
                  serverFile.name);
		IF serverFile.type = FileInfo.Type.Dead THEN cmd := "t" END;
		(* If it is an RCS file, and we are using "loose"
		   equality for RCS files, then the sizes may
		   disagree because of harmless differences in
		   white space. *)
		IF SupMisc.IsRCS(serverFile.name)
		AND NOT SupFileRec.Option.NoRCS IN sfr.options
		AND NOT SupFileRec.Option.StrictCheckRCS IN sfr.options
		AND self.proto.v.hasLooseRCSCheck THEN
		  sendAttr := FileAttr.MaskOut(sendAttr,
		    FileAttr.AttrTypes{ FileAttr.AttrType.Size });
		END;
		IF NOT HardLink(self, linkTbl, serverFile) THEN
		  self.proto.putCmd(self.wr, cmd,
		    serverFile.name,
		    EncodeAttr(self, sfr, sendAttr));
		END;
	      END;
	  END;
	END;
      ELSIF serverFile = NIL THEN  (* Delete file on client. *)
	CASE clientFile.type OF
	| FileInfo.Type.DirDown =>  (* Remove listfile entry for directory. *)
            Trace(self, myName, " Remove listfile entry for directory ",
              clientFile.name);
	    self.proto.putCmd(self.wr, "i", clientFile.name);
	| FileInfo.Type.DirUp =>  (* Remove directory. *)
            Trace(self, myName, " Remove directory ", clientFile.name);
	    self.proto.putCmd(self.wr, "j", clientFile.name);
	| FileInfo.Type.Live, FileInfo.Type.Dead =>  (* Delete file. *)
            Trace(self, myName, " Delete file ", clientFile.name);
	    self.proto.putCmd(self.wr, "D", clientFile.name);
	END;
      ELSE  (* File exists on both the server and the client. *)
	IF NOT SupFileRec.Option.DoDeletesOnly IN sfr.options THEN
	  CASE serverFile.type OF
	  | FileInfo.Type.DirDown =>
	      IF clientFile.type # FileInfo.Type.DirDown THEN
		<* ASSERT clientFile.type # FileInfo.Type.DirUp *>
		ReplaceFileWithDirectory(self,
		  clientFile := clientFile,
		  serverDir := serverFile);
	      END;
	      (* Otherwise, do nothing at this point.  The work will be done
		 when we reach the DirUp. *)
	  | FileInfo.Type.DirUp =>
	      <* ASSERT clientFile.type = FileInfo.Type.DirUp *>
	      IF NOT FileAttr.Equal(clientFile.attr,
		FileAttr.Umask(serverFile.attr, sfr.umask))
	      THEN
		(* Set directory attributes. *)
               Trace(self, myName, " Set directory attributes ",
                 serverFile.name);
		self.proto.putCmd(self.wr, "J",
		  serverFile.name,
		  EncodeAttr(self, sfr, serverFile.attr));
	      END;
	  | FileInfo.Type.Live, FileInfo.Type.Dead =>
	      IF clientFile.type = FileInfo.Type.DirDown THEN
		ReplaceDirectoryWithFile(self, merger,
		  clientDir := clientFile,
		  serverFile := serverFile);
	      ELSE
		<* ASSERT clientFile.type # FileInfo.Type.DirUp *>
		IF NOT HardLink(self, linkTbl, serverFile) THEN
		  IF NOT FileAttr.Equal(clientFile.attr,
		    FileAttr.Umask(serverFile.attr, sfr.umask))
		  OR clientFile.type # serverFile.type
                  OR SupFileRec.Option.DetailAllRCSFiles IN sfr.options AND
                     SupMisc.IsRCS(serverFile.name) THEN
		    (* FIXME - check for BogusModTime? *)
                    IF self.clientClass.collectionIsPartiallyHidden(
		      sfr.collection)
		    THEN
                      Trace(self, myName, 
                        " Update partially hidden file (fix) ",
                        serverFile.name);
                    ELSE
                      Trace(self, myName, " Update file (fix) ",
                        serverFile.name);
                    END;
                    self.proto.putCmd(self.wr, "U", serverFile.name);
		  END;
		END;
	      END;
	  END;
	END;
      END;
      Wr.Flush(self.wr);
    END;
  END CompCVSMode;

PROCEDURE ReplaceDirectoryWithFile(self: T;
				   merger: Merger;
				   clientDir: FileInfo.T;
				   serverFile: FileInfo.T)
  RAISES {Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted,
	  TokScan.Error, Wr.Failure} =
  CONST myName = "ReplaceDirectoryWithFile: ";
  VAR
    cf, sf: FileInfo.T;
    depth: CARDINAL;
  BEGIN
    Trace(self, myName, " ", clientDir.name, " ", serverFile.name);
    (* Remove the entire directory tree on the client. *)
    (* FIXME - Make the client remove empty Attic directories automatically. *)
    cf := clientDir;
    sf := NIL;
    depth := 0;
    REPEAT
      (* We should get only client files from the merger until we have
	 consumed the entire directory tree. *)
      <* ASSERT sf = NIL *>
      CASE cf.type OF
      | FileInfo.Type.DirDown =>  (* Remove listfile entry. *)
          Trace(self, myName, " Remove listfile entry ", cf.name);
	  self.proto.putCmd(self.wr, "i", cf.name);
	  INC(depth);
      | FileInfo.Type.DirUp =>  (* Remove directory. *)
          Trace(self, myName, " Remove directory ", cf.name);
	  self.proto.putCmd(self.wr, "j", cf.name);
	  DEC(depth);
      | FileInfo.Type.Live, FileInfo.Type.Dead =>  (* Delete file. *)
          Trace(self, myName, " Delete file ", cf.name);
	  self.proto.putCmd(self.wr, "D", cf.name);
      END;
    UNTIL depth = 0 OR NOT GetNext(merger, cf, sf);

    IF depth # 0 THEN
      RAISE Error("Unmatched DirDown from client");
    END;

    (* Add the file. *)
    Trace(self, myName, " Update file ", serverFile.name);
    self.proto.putCmd(self.wr, "U", serverFile.name);
  END ReplaceDirectoryWithFile;

PROCEDURE ReplaceFileWithDirectory(self: T;
				   clientFile: FileInfo.T;
				   serverDir: FileInfo.T)
  RAISES {Thread.Alerted, Wr.Failure} =
  CONST myName = "ReplaceFileWithDirectory: ";
  BEGIN
    Trace(self, myName, " ", clientFile.name, " ", serverDir.name);
    (* Delete the file, then create the directory. *)
    self.proto.putCmd(self.wr, "D", clientFile.name);
    self.proto.putCmd(self.wr, "I", serverDir.name);
  END ReplaceFileWithDirectory;

PROCEDURE CompCheckoutMode(self: T;
                           sfr: SupFileRec.T;
			   merger: Merger)
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted, TokScan.Error,
          Wr.Failure} =
  CONST myName = "CompCheckoutMode: ";
  VAR
    clientFile, serverFile: FileInfo.T;
  BEGIN
    Trace(self, myName, sfr.collection);
    <* ASSERT self.clientClass.inAllowedCollections(sfr.collection) *>
    <* ASSERT NOT
       self.clientClass.collectionIsPartiallyHidden(sfr.collection) *>
    WHILE GetNext(merger, clientFile, serverFile) DO
      IF clientFile = NIL THEN  (* Add file on client. *)
	IF NOT FileInfo.IsDir(serverFile) THEN
          Trace(self, myName, " Update file (add) ", serverFile.name);
	  self.proto.putCmd(self.wr, "U", serverFile.name);
	END;
      ELSIF serverFile = NIL THEN  (* Delete file on client. *)
	IF NOT FileInfo.IsDir(clientFile) THEN
          Trace(self, myName, " Delete file ", clientFile.name);
	  self.proto.putCmd(self.wr, "D", clientFile.name);
	END;
      ELSE  (* File exists on both the server and the client. *)
	(* Note, the client records the exact attributes of the
           server's RCS file in checkout mode, unmodified by the
           client's umask.  So we need not do anything with the umask
           on this end. *)
	IF NOT FileInfo.IsDir(serverFile)
	AND NOT FileAttr.Equal(clientFile.attr, serverFile.attr)
	THEN
	  (* FIXME - check for BogusModTime? *)
          Trace(self, myName, " Update file (fix)");
	  self.proto.putCmd(self.wr, "U", serverFile.name);
	END;
      END;
      Wr.Flush(self.wr);
    END;
  END CompCheckoutMode;

PROCEDURE HardLink(self: T;
		   linkTbl: LinkTbl.T;
                   fi: FileInfo.T): BOOLEAN
  RAISES {Thread.Alerted, Wr.Failure} =
(* If there is a known hard link to the given file, emits a link command
   and returns "TRUE".  Otherwise, returns "FALSE". *)
  VAR
    linkTo: Pathname.T;
    cmd: TEXT;
  BEGIN
    IF linkTbl # NIL AND NOT FileInfo.IsDir(fi)
    AND FileAttr.AttrType.LinkCount IN FileAttr.GetMask(fi.attr)
    AND FileAttr.GetLinkCount(fi.attr) > 1 THEN
      WITH id = FileID.FromAttr(fi.attr) DO
	IF id # NIL THEN
	  IF linkTbl.get(id, linkTo) THEN
	    IF fi.type = FileInfo.Type.Live THEN cmd := "H" ELSE cmd := "h" END;
	    self.proto.putCmd(self.wr, cmd, fi.name, linkTo);
	    RETURN TRUE;
	  END;
	  EVAL linkTbl.put(id, fi.name);
	END;
      END;
    END;
    RETURN FALSE;
  END HardLink;

PROCEDURE DecodeAttr(<*UNUSED*> self: T; t: TEXT): FileAttr.T
  RAISES {TokScan.Error} =
  BEGIN
    LOOP
      TRY
	RETURN FileAttr.Decode(t);
      EXCEPT FileAttr.UnknownGroup, FileAttr.UnknownOwner =>
	(* Ignore unknown attributes from the client. *)
      END;
    END;
  END DecodeAttr;

PROCEDURE EncodeAttr(self: T;
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
  END EncodeAttr;

PROCEDURE GetNext(m: Merger;
                  VAR clientFile, serverFile: FileInfo.T): BOOLEAN
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted, TokScan.Error} =
(* This is a wrapper around "Merger.next()", to correct its "RAISES"
   clause.  This procedure should raise the union of the exceptions
   raised by "GetFromClient" and "GetFromServer". *)
  <* FATAL ANY *>
  BEGIN
    RETURN m.next(clientFile, serverFile);
  END GetNext;

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
    self.rd := rd;  (* Start out uncompressed. *)
    self.wireWr := wr;
    self.wr := wr;  (* Start out uncompressed. *)
    self.collections := collections;
    self.clientClass := clientClass;
    IF compLevel = -1 THEN compLevel := SupMisc.DefaultCompression END;
    self.compLevel := compLevel;
    self.reaper := reaper;
    self.logger := logger;
    RETURN self;
  END Init;

(*****************************************************************************)

TYPE
  Merger = FileInfoMerger.T OBJECT
    treeComp: T;
    rd: Rd.T;
    sfr: SupFileRec.T;
    iter: CVTree.Iterator;
    serverPruning: BOOLEAN;
    clientDecomp: PathComp.Decompressor;
  METHODS
    init(self: T; sfr: SupFileRec.T): Merger
      RAISES {CVTree.Error, Thread.Alerted} := MergerInit;
    close()
      RAISES {CVTree.Error, Thread.Alerted} := MergerClose;
  OVERRIDES
    getA := GetFromClient;
    getB := GetFromServer;
  END;

PROCEDURE MergerInit(m: Merger;
                     self: T;
		     sfr: SupFileRec.T): Merger
  RAISES {CVTree.Error, Thread.Alerted} =
  BEGIN
    m.treeComp := self;
    m.rd := self.rd;
    m.sfr := sfr;
    m.iter := NIL;
    IF sfr.serverScanFile # NIL THEN  (* Use the scan file. *)
      TRY
	m.iter := NEW(FSIter).init(sfr.serverScanFile);
      EXCEPT CVTree.Error => (* Ignore. *) END;
    END;
    IF m.iter = NIL THEN  (* Do a full tree walk. *)
      m.iter := CVTree.Iterate(
	root := sfr.serverPrefix,
	follow := GlobTree.Not(sfr.symlink));
    END;
    m.serverPruning := FALSE;
    m.clientDecomp := NEW(PathComp.Decompressor).init();
    RETURN m;
  END MergerInit;

PROCEDURE MergerClose(m: Merger)
  RAISES {CVTree.Error, Thread.Alerted} =
  BEGIN
    m.iter.close();
  END MergerClose;

PROCEDURE GetFromClient(m: Merger): FileInfo.T
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted, TokScan.Error} =
(* If you add any exceptions, add them to "GetNext" also. *)
  VAR
    ts: TokScan.T;
    cmd: TEXT;
    name: TEXT;
    attr: FileAttr.T;
    cmdCh: CHAR;
    type: FileInfo.Type;
  BEGIN
    LOOP
      ts := m.treeComp.proto.getCmd(m.rd);
      cmdCh := ts.getChar("command");
      cmd := Text.FromChar(cmdCh);
      CASE cmdCh OF
      | '.' =>
	  EXIT;
      | 'D' =>  (* Down into directory. *)
	  name := ts.getToken("directory name");
	  ts.getEnd("end of \"" & cmd & "\" command");
	  WITH path = m.clientDecomp.put(PathComp.Type.DirDown, name) DO
	    IF m.treeComp.proto.v.dirsAreExplicit
	    AND NOT SupFileRec.Option.CheckoutMode IN m.sfr.options THEN
	      RETURN NEW(FileInfo.T,
		name := path,
		attr := NIL,
		type := FileInfo.Type.DirDown);
	    END;
	  END;
      | 'F', 'f' =>  (* Live file, dead file. *)
	  IF cmdCh = 'F' THEN
	    type := FileInfo.Type.Live;
	  ELSE 
	    type := FileInfo.Type.Dead;
	  END;
	  name := ts.getToken("file name");
	  IF m.treeComp.proto.v.hasFileAttrs THEN
	    attr := DecodeAttr(m.treeComp, ts.getToken("attributes"));
	  ELSE
	    attr := NEW(FileAttr.T).init(FileAttr.FileType.File,
	      modTime := ts.getTime("modTime"));
	  END;
	  ts.getEnd("end of \"" & cmd & "\" command");
	  WITH path = m.clientDecomp.put(PathComp.Type.File, name) DO
	    RETURN NEW(FileInfo.T,
	      name := path,
	      attr := attr,
	      type := type);
	  END;
      | 'U' =>  (* Up out of directory. *)
	  IF m.treeComp.proto.v.dirsAreExplicit THEN
	    attr := DecodeAttr(m.treeComp, ts.getToken("attributes"));
	  END;
	  ts.getEnd("end of \"" & cmd & "\" command");
	  WITH path = m.clientDecomp.put(PathComp.Type.DirUp, NIL) DO
	    IF m.treeComp.proto.v.dirsAreExplicit
	    AND NOT SupFileRec.Option.CheckoutMode IN m.sfr.options THEN
	      RETURN NEW(FileInfo.T,
		name := path,
		attr := attr,
		type := FileInfo.Type.DirUp);
	    END;
	  END;
      ELSE
	RAISE TokScan.Error("Invalid command \"" & cmd & "\"");
      END;
    END;
    RETURN NIL;
  END GetFromClient;

PROCEDURE GetFromServer(m: Merger): FileInfo.T
  RAISES {Thread.Alerted} =
(* If you add any exceptions, add them to "GetNext" also. *)
  VAR
    type: CVTree.FileType;
    path: Pathname.T;
    attr: FileAttr.T;
    listIt: BOOLEAN;
  BEGIN
    LOOP
      TRY
	IF NOT m.iter.next(type, path, attr) THEN EXIT END;
	CASE type OF
	| CVTree.FileType.DirDown =>
	    IF m.sfr.dirFilter.test(path) THEN
	      IF m.treeComp.proto.v.dirsAreExplicit
	      AND NOT SupFileRec.Option.CheckoutMode IN m.sfr.options THEN
		RETURN NEW(FileInfo.T,
		  name := path,
		  attr := attr,
		  type := FileInfo.Type.DirDown);
	      END;
	    ELSE
	      m.iter.prune();
	      m.serverPruning := TRUE;
	    END;
	| CVTree.FileType.File =>
	    IF SupFileRec.Option.CheckoutMode IN m.sfr.options THEN
	      (* List RCS files only. *)
	      listIt := attr.fileType = FileAttr.FileType.File
		AND SupMisc.IsRCS(path);
	    ELSE
	      (* List everything that is supported by the protocol.  Ignore
		 the unsupported file types. *)
	      listIt := FileAttr.AttrType.FileType IN
		m.treeComp.proto.v.attrSupport[attr.fileType];
	    END;
	    IF listIt AND m.sfr.fileFilter.test(path) THEN
	      RETURN NEW(FileInfo.T,
		name := path,
		attr := attr,
		type := FileInfo.Type.Live);
	    END;
	| CVTree.FileType.AtticFile =>
	    WITH atticPath = SupMisc.AtticName(path) DO
	      IF attr.fileType = FileAttr.FileType.File
	      AND SupMisc.IsRCS(path) THEN
		IF m.sfr.fileFilter.test(atticPath) THEN
		  RETURN NEW(FileInfo.T,
		    name := path,
		    attr := attr,
		    type := FileInfo.Type.Dead);
		END;
	      ELSE  (* Non-RCS file in the Attic?! *)
		WITH fullPath = SupMisc.CatPath(m.sfr.serverPrefix, atticPath)
		DO
		  Warning(m.treeComp,
		    "Non-RCS file \"" & fullPath & "\" in Attic");
		END;
	      END;
	    END;
	| CVTree.FileType.DirUp =>
	    IF NOT m.serverPruning THEN
	      IF m.treeComp.proto.v.dirsAreExplicit
	      AND NOT SupFileRec.Option.CheckoutMode IN m.sfr.options THEN
		RETURN NEW(FileInfo.T,
		  name := path,
		  attr := attr,
		  type := FileInfo.Type.DirUp);
	      END;
	    ELSE
	      m.serverPruning := FALSE;
	    END;
	END;
      EXCEPT CVTree.Error(msg) =>
	Warning(m.treeComp, msg);
      END;
    END;
    RETURN NIL;
  END GetFromServer;

(*****************************************************************************)

TYPE
  FSIter = CVTree.Iterator OBJECT
    path: Pathname.T;
    fsrd: FileStatus.Reader;
    pruning := FALSE;
  METHODS
    init(path: Pathname.T): FSIter
      RAISES {CVTree.Error, Thread.Alerted} := FSIterInit;
  OVERRIDES
    next := FSIterNext;
    prune := FSIterPrune;
    close := FSIterClose;
  END;

PROCEDURE FSIterClose(self: FSIter)
  RAISES {CVTree.Error, Thread.Alerted} =
  BEGIN
    TRY
      self.fsrd.close();
    EXCEPT Rd.Failure(l) =>
      RAISE CVTree.Error("Read failure on \"" & self.path & "\": "
	& ErrMsg.StrError(l));
    END;
  END FSIterClose;

PROCEDURE FSIterInit(self: FSIter; path: Pathname.T): FSIter
  RAISES {CVTree.Error, Thread.Alerted} =
  VAR
    rd: Rd.T;
  BEGIN
    TRY
      self.path := path;
      rd := FileRd.Open(self.path);
      self.fsrd := NIL;
      TRY
	TRY
	  self.fsrd := FileStatus.FromRd(rd);
	  IF self.fsrd.version() < 5 THEN  (* Too old. *)
	    self.fsrd.close();
	    RAISE CVTree.Error("Scan file \"" & self.path
	      & "\" format is too old");
	  END;
	  RETURN self;
	EXCEPT
	| FileStatus.Error(msg) =>
	    RAISE CVTree.Error("Error in \"" & self.path & "\": " & msg);
	| Rd.Failure(l) =>
	    RAISE CVTree.Error("Read failure on \"" & self.path & "\": "
	      & ErrMsg.StrError(l));
	END;
      FINALLY
	IF self.fsrd = NIL THEN
	  TRY Rd.Close(rd) EXCEPT ELSE END;
	END;
      END;
    EXCEPT
    | OSError.E(l) =>
	RAISE CVTree.Error("Cannot open \"" & self.path & "\": "
	  & ErrMsg.StrError(l));
    END;
  END FSIterInit;

PROCEDURE FSIterNext(self: FSIter;
                     VAR type: CVTree.FileType;
                     VAR name: Pathname.T;
		     VAR attr: FileAttr.T): BOOLEAN
  RAISES {CVTree.Error, Thread.Alerted} =
  VAR
    fs: FileStatus.T;
  BEGIN
    TRY
      IF self.pruning THEN
	self.pruning := FALSE;
	fs := self.fsrd.prune();
      ELSE
	fs := self.fsrd.get();
      END;

      name := fs.name;
      attr := fs.clientAttr;
      CASE fs.type OF
      | FileStatus.Type.DirDown =>
	  type := CVTree.FileType.DirDown;
      | FileStatus.Type.DirUp =>
	  type := CVTree.FileType.DirUp;
      | FileStatus.Type.FileLive =>
	  type := CVTree.FileType.File;
      | FileStatus.Type.FileDead =>
	  type := CVTree.FileType.AtticFile;
      | FileStatus.Type.CheckoutLive, FileStatus.Type.CheckoutDead =>
	  RAISE CVTree.Error("Invalid checkout-mode scan file \""
	    & self.path & "\"");
      END;
      RETURN TRUE;
    EXCEPT
    | FileStatus.Error(msg) =>
	RAISE CVTree.Error("Error in \"" & self.path & "\": "
	  & msg);
    | Rd.EndOfFile =>
	RETURN FALSE;
    | Rd.Failure(l) =>
	RAISE CVTree.Error("Read error on \"" & self.path & "\": "
	  & ErrMsg.StrError(l));
    END;
  END FSIterNext;

PROCEDURE FSIterPrune(self: FSIter) =
  BEGIN
    self.pruning := TRUE;
  END FSIterPrune;

(*****************************************************************************)

PROCEDURE Warning(self: T; msg: TEXT) =
(* Logs a warning message. *)
  BEGIN
    IF self.logger # NIL THEN
      Logger.Put(self.logger, Logger.Priority.Warning, msg);
    END;
  END Warning;

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
END TreeComp.
