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

MODULE FileStatus EXPORTS FileStatus, FileStatusRaw;

IMPORT
  FileAttr, Fmt, PathComp, Rd, SupMisc, Text, Thread, Time, TokScan, Wr;

REVEAL
  T = Public BRANDED OBJECT
    line: TEXT := NIL;
    ts: TokScan.T := NIL;
    lineNum := 0;
  END;

  Reader = RawRdPublic BRANDED OBJECT
  END;

  Writer = RawWrPublic BRANDED OBJECT
  END;

CONST
  Version = 5;  (* File format version number. *)

VAR (*CONST*)
  HugeText := Text.FromChars( ARRAY [0..15] OF CHAR{ LAST(CHAR), .. } );
(* Assumed to compare greater than any pathname component we might encounter. *)

PROCEDURE Compare(a, b: T): [-1..1] =
  VAR
    aName := a.name;
    bName := b.name;
  BEGIN
    IF a.type = Type.DirUp THEN
      aName := SupMisc.CatPath(aName, HugeText);
    END;
    IF b.type = Type.DirUp THEN
      bName := SupMisc.CatPath(bName, HugeText);
    END;
    RETURN SupMisc.PathCompare(aName, bName);
  END Compare;

PROCEDURE Equal(a, b: T): BOOLEAN =
  BEGIN
    RETURN Text.Equal(a.name, b.name)
      AND (a.type = Type.DirUp) = (b.type = Type.DirUp);
  END Equal;

PROCEDURE MakeCooked(fs: T; version: CARDINAL)
  RAISES {Error} =
  BEGIN
    IF fs.line = NIL THEN  (* Already cooked. *)
      RETURN;
    END;
    TRY
      CASE fs.type OF
      | Type.DirDown =>
	  (* Nothing to do. *)
      | Type.DirUp =>
	  IF version >= 5 THEN
	    fs.clientAttr := GetAttr(fs.ts, version, "directory attributes");
	  ELSE
	    fs.clientAttr := FileAttr.Bogus;
	  END;
	  fs.serverAttr := fs.clientAttr;
      | Type.CheckoutLive =>
	  fs.tag := fs.ts.getToken("tag");
	  fs.date := fs.ts.getToken("date");
	  fs.serverAttr := GetAttr(fs.ts, version, "server file attributes");
	  fs.revNum := fs.ts.getToken("revision number");
	  IF version >= 4 THEN
	    fs.revDate := fs.ts.getToken("revDate");
	  ELSE
	    fs.revDate := ".";
	  END;
	  fs.clientAttr :=
	    GetAttr(fs.ts, version, "client file attributes");
      | Type.CheckoutDead =>
	  fs.tag := fs.ts.getToken("tag");
	  fs.date := fs.ts.getToken("date");
	  fs.serverAttr := GetAttr(fs.ts, version, "server file attributes");
      | Type.FileLive =>
	  fs.clientAttr := GetAttr(fs.ts, version, "file attributes");
	  fs.serverAttr := fs.clientAttr;
      | Type.FileDead =>
	  fs.clientAttr := GetAttr(fs.ts, version, "file attributes");
	  fs.serverAttr := fs.clientAttr;
      END;
      fs.line := NIL;
      fs.ts := NIL;
    EXCEPT
    | Error(msg) =>
	RAISE Error(Fmt.Int(fs.lineNum) & ": " & msg);
    | TokScan.Error(msg) =>
	RAISE Error(Fmt.Int(fs.lineNum) & ": " & msg);
    END;
  END MakeCooked;

PROCEDURE GetAttr(ts: TokScan.T; version: CARDINAL; what: TEXT): FileAttr.T
  RAISES {Error, TokScan.Error} =
  BEGIN
    IF version < 3 THEN
      RETURN NEW(FileAttr.T).init(FileAttr.FileType.File,
	modTime := ts.getTime(what));
    ELSE
      TRY
	RETURN FileAttr.Decode(ts.getToken(what));
      EXCEPT
      | FileAttr.UnknownGroup(name) =>
	  RAISE Error("Unknown group name \"" & name & "\"");
      | FileAttr.UnknownOwner(name) =>
	  RAISE Error("Unknown user name \"" & name & "\"");
      END;
    END;
  END GetAttr;

(*****************************************************************************)

TYPE
  RdReader = Reader OBJECT
    rd: Rd.T;
    vers: CARDINAL;
    time: Time.T;
    statusCode: CHAR;
    originHost: TEXT;
    delta: Time.T;
    lineNum := 0;
    comp: PathComp.Compressor := NIL;
    decomp: PathComp.Decompressor := NIL;
    previous: T := NIL;
    current: T := NIL;
    eofSeen := FALSE;
    depth: CARDINAL := 0;
  OVERRIDES
    version := RdVersion;
    get := RdGet;
    getRaw := RdGetRaw;
    prune := RdPrune;
    scanTime := RdScanTime;
    status := RdStatus;
    origin := RdOrigin;
    timeDelta := RdTimeDelta;
    close := RdClose;
  END;

PROCEDURE FromRd(rd: Rd.T): Reader
  RAISES {Error, Rd.Failure, Thread.Alerted} =
  VAR
    rr := NEW(RdReader, rd := rd);
    line: TEXT;
    ts: TokScan.T;
  BEGIN
    TRY
      TRY
	line := SupMisc.GetCmdLine(rd);
	INC(rr.lineNum);
	ts := TokScan.NewDec(line);
	ts.getLiteral("F");
	rr.vers := ts.getInt("file format version number");
	IF rr.vers < 1 OR rr.vers > Version THEN
	  RAISE Error("Invalid format version " & Fmt.Int(rr.vers));
	END;
	rr.time := ts.getTime("scan time");
	TRY 
	  rr.statusCode := ts.getChar("status");
	  rr.originHost := ts.getToken("origin");
	  rr.delta := ts.getTime("timeDelta");
	EXCEPT TokScan.Error =>
	  rr.statusCode := '?';
	  rr.originHost := "?";
	  rr.delta := 0.0d0;
	END;
	IF rr.vers < 3 THEN  (* No explicit directories in the file. *)
	  rr.comp := NEW(PathComp.Compressor).init();
	  rr.decomp := NEW(PathComp.Decompressor).init();
	END;
	RETURN rr;
      EXCEPT
      | Rd.EndOfFile =>
	  RAISE Error("Premature EOF");
      | TokScan.Error(msg) =>
	  RAISE Error(msg);
      END;
    EXCEPT Error(msg) =>
      RAISE Error(Fmt.Int(rr.lineNum) & ": " & msg);
    END;
  END FromRd;

PROCEDURE RdClose(rr: RdReader)
  RAISES {Rd.Failure, Thread.Alerted} =
  BEGIN
    Rd.Close(rr.rd);
  END RdClose;

PROCEDURE RdGet(rr: RdReader): T
  RAISES {Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  BEGIN
    WITH fs = rr.getRaw() DO
      MakeCooked(fs, rr.vers);
      RETURN fs;
    END;
  END RdGet;

PROCEDURE RdGetRaw(rr: RdReader): T
  RAISES {Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  VAR
    line: TEXT;
    typeCh: CHAR;
    pcType: PathComp.Type;
    pcName: TEXT;
  BEGIN
    TRY
      LOOP
	IF rr.comp # NIL THEN  (* Synthesizing directory entries. *)
	  (* If anything is queued up in the compressor, return it. *)
	  IF rr.comp.get(pcType, pcName) THEN
	    WITH fullName = rr.decomp.put(pcType, pcName) DO
	      CASE pcType OF
	      | PathComp.Type.DirDown =>
		  RETURN NEW(T,
		    type := Type.DirDown,
		    name := fullName,
		    line := "D " & fullName);
	      | PathComp.Type.DirUp =>
		  RETURN NEW(T,
		    type := Type.DirUp,
		    name := fullName,
		    line := "U " & fullName);
	      | PathComp.Type.File =>
		  <* ASSERT Text.Equal(fullName, rr.current.name) *>
		  rr.previous := rr.current;
		  rr.current := NIL;
		  RETURN rr.previous;
	      END;
	    END;
	  END;
	END;

	(* Get the next record from the file. *)
	IF rr.eofSeen THEN
	  IF rr.depth # 0 THEN
	    RAISE Error("File is truncated");
	  END;
	  RAISE Rd.EndOfFile;
	END;
	TRY
	  line := SupMisc.GetCmdLine(rr.rd);
	  INC(rr.lineNum);
	  rr.current := NEW(T,
	    line := line,
	    lineNum := rr.lineNum,
	    ts := TokScan.NewDec(line));

	  typeCh := rr.current.ts.getChar("file type");
	  rr.current.name := rr.current.ts.getToken("filename");

	  CASE typeCh OF
	  | 'D' =>
	      rr.current.type := Type.DirDown;
	      IF rr.comp # NIL THEN
		RAISE Error("Unexpected directory entry in version "
		  & Fmt.Int(rr.vers) & " file");
	      END;
	      INC(rr.depth);
	  | 'C' =>
	      rr.current.type := Type.CheckoutLive;
	      IF rr.vers < 2 THEN
		rr.current.name := SupMisc.RCSName(rr.current.name);
	      END;
	  | 'c' =>
	      rr.current.type := Type.CheckoutDead;
	      IF rr.vers < 2 THEN
		rr.current.name := SupMisc.RCSName(rr.current.name);
	      END;
	  | 'V' =>
	      rr.current.type := Type.FileLive;
	  | 'v' =>
	      rr.current.type := Type.FileDead;
	  | 'U' =>
	      rr.current.type := Type.DirUp;
	      IF rr.comp # NIL THEN
		RAISE Error("Unexpected directory entry in version "
		  & Fmt.Int(rr.vers) & " file");
	      END;
	      IF rr.depth <= 0 THEN
		RAISE Error("\"U\" entry has no matching \"D\"");
	      END;
	      DEC(rr.depth);
	  ELSE
	    RAISE Error("Invalid file type \"" & Text.FromChar(typeCh) & "\"");
	  END;

	  IF rr.previous # NIL AND Compare(rr.previous, rr.current) >= 0 THEN
	    RAISE Error("File is not sorted properly");
	  END;

	  IF rr.comp # NIL THEN  (* Synthesizing directory entries. *)
	    TRY
	      CASE rr.current.type OF
	      | Type.DirDown =>	pcType := PathComp.Type.DirDown;
	      | Type.DirUp =>	pcType := PathComp.Type.DirUp;
	      ELSE
		pcType := PathComp.Type.File;
	      END;
	      rr.comp.put(pcType, rr.current.name);
	    EXCEPT PathComp.Error(msg) =>
	      RAISE Error(msg);
	    END;
	  ELSE
	    rr.previous := rr.current;
	    rr.current := NIL;
	    RETURN rr.previous;
	  END;
	EXCEPT
	| Rd.EndOfFile =>
	    rr.eofSeen := TRUE;
	    IF rr.comp # NIL THEN
	      rr.comp.finish();
	    END;
	| TokScan.Error(msg) =>
	    RAISE Error(msg);
	END;
      END;
    EXCEPT Error(msg) =>
      RAISE Error(Fmt.Int(rr.lineNum) & ": " & msg);
    END;
  END RdGetRaw;

PROCEDURE RdPrune(rr: RdReader): T
  RAISES {Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  VAR
    fs: T;
    pruneDepth := rr.depth;
  BEGIN
    WHILE rr.depth >= pruneDepth DO
      fs := rr.getRaw();
    END;
    <* ASSERT fs.type = Type.DirUp *>
    MakeCooked(fs, rr.vers);
    RETURN fs;
  END RdPrune;

PROCEDURE RdScanTime(rr: RdReader): Time.T =
  BEGIN
    RETURN rr.time;
  END RdScanTime;

PROCEDURE RdStatus(rr: RdReader): CHAR =
  BEGIN
    RETURN rr.statusCode;
  END RdStatus;

PROCEDURE RdOrigin(rr: RdReader): TEXT =
  BEGIN
    RETURN rr.originHost;
  END RdOrigin;

PROCEDURE RdTimeDelta(rr: RdReader): Time.T =
  BEGIN
    RETURN rr.delta;
  END RdTimeDelta;

PROCEDURE RdVersion(rr: RdReader): CARDINAL =
  BEGIN
    RETURN rr.vers;
  END RdVersion;

(*****************************************************************************)

PROCEDURE FromNull(): Reader =
  BEGIN
    RETURN NEW(Reader,
      version := NullVersion,
      scanTime := NullScanTime,
      get := NullGet,
      getRaw := NullGetRaw,
      close := NullClose);
  END FromNull;

PROCEDURE NullClose(<*UNUSED*> r: Reader) =
  BEGIN
  END NullClose;

PROCEDURE NullGet(<*UNUSED*> r: Reader): T
  RAISES {Rd.EndOfFile} =
  BEGIN
    RAISE Rd.EndOfFile;
  END NullGet;

PROCEDURE NullGetRaw(<*UNUSED*> r: Reader): T
  RAISES {Rd.EndOfFile} =
  BEGIN
    RAISE Rd.EndOfFile;
  END NullGetRaw;

PROCEDURE NullScanTime(<*UNUSED*> r: Reader): Time.T =
  BEGIN
    RETURN -1.0d0;
  END NullScanTime;

PROCEDURE NullVersion(<*UNUSED*> r: Reader): CARDINAL =
  BEGIN
    RETURN Version;
  END NullVersion;

(*****************************************************************************)

TYPE
  WrWriter = Writer OBJECT
    wr: Wr.T;
    current: T := NIL;
    comp: PathComp.Compressor;
    decomp: PathComp.Decompressor;
    cannotWrite := FALSE;
  OVERRIDES
    version := WrVersion;
    put := WrPut;
    putRaw := WrPutRaw;
    close := WrClose;
  END;

PROCEDURE ToWr(wr: Wr.T; scanTime: Time.T): Writer
  RAISES {Thread.Alerted, Wr.Failure} =
  VAR
    ww := NEW(WrWriter,
      wr := wr,
      comp := NEW(PathComp.Compressor).init(),
      decomp := NEW(PathComp.Decompressor).init());
  BEGIN
    SupMisc.PutCmd(wr, "F",
      Fmt.Int(Version),
      TokScan.EncodeTime(scanTime),
      encode := TRUE);
    RETURN ww;
  END ToWr;

PROCEDURE WrClose(ww: WrWriter)
  RAISES {Thread.Alerted, Wr.Failure} =
  VAR
    pcType: PathComp.Type;
    pcName: TEXT;
  BEGIN
    IF NOT ww.cannotWrite THEN
      (* Close off all the open directories. *)
      ww.comp.finish();
      WHILE ww.comp.get(pcType, pcName) DO
	WITH fullName = ww.decomp.put(pcType, pcName) DO
	  CASE pcType OF
	  | PathComp.Type.DirDown =>
	      SupMisc.PutCmd(ww.wr, "D",
		fullName,
		encode := TRUE);
	  | PathComp.Type.DirUp =>
	      SupMisc.PutCmd(ww.wr, "U",
		fullName,
		FileAttr.Encode(FileAttr.Bogus),
		encode := TRUE);
	  | PathComp.Type.File =>  (* Should never happen. *)
	      <* ASSERT FALSE *>
	  END;
	END;
      END;
    END;
    Wr.Close(ww.wr);
  END WrClose;

PROCEDURE WrPut(ww: WrWriter; fs: T)
  RAISES {Error, Thread.Alerted, Wr.Failure} =
  VAR
    pcType: PathComp.Type;
    pcName: TEXT;
    dirUpAttr: FileAttr.T;
  BEGIN
    IF ww.cannotWrite THEN  (* Don't even try. *)
      RETURN;
    END;
    IF ww.current # NIL THEN  (* Don't ever write the records out of order. *)
      <* ASSERT Compare(ww.current, fs) < 0 *>
    END;
    ww.current := fs;

    TRY
      (* Rather than rely on the proper DirDown and DirUp entries being
	 supplied, we synthesize them on our own.  This is necessary in
	 certain situations when we are reading from an older version of
	 the status file which did not have explicit directory entries. *)

      TRY
	dirUpAttr := FileAttr.Bogus;

	CASE fs.type OF
	| Type.DirDown =>
	    ww.comp.put(PathComp.Type.DirDown, fs.name);
	| Type.DirUp =>
	    ww.comp.put(PathComp.Type.DirUp, fs.name);
	    dirUpAttr := fs.clientAttr;
	ELSE
	  ww.comp.put(PathComp.Type.File, fs.name);
	END;

	WHILE ww.comp.get(pcType, pcName) DO
	  WITH fullName = ww.decomp.put(pcType, pcName) DO
	    CASE pcType OF
	    | PathComp.Type.DirDown =>
		SupMisc.PutCmd(ww.wr, "D",
		  fullName,
		  encode := TRUE);
	    | PathComp.Type.DirUp =>
		SupMisc.PutCmd(ww.wr, "U",
		  fullName,
		  FileAttr.Encode(dirUpAttr),
		  encode := TRUE);
		dirUpAttr := FileAttr.Bogus;
	    | PathComp.Type.File =>
		(* Just swallow it. *)
		<* ASSERT Text.Equal(fullName, fs.name) *>
	    END;
	  END;
	END;
      EXCEPT PathComp.Error(msg) =>
	RAISE Error(msg);
      END;

      CASE fs.type OF
      | Type.DirDown, Type.DirUp =>
	  (* Already emitted above. *)
      | Type.CheckoutLive =>
	  SupMisc.PutCmd(ww.wr, "C",
	    fs.name,
	    fs.tag,
	    fs.date,
	    FileAttr.Encode(fs.serverAttr),
	    fs.revNum,
	    fs.revDate,
	    FileAttr.Encode(fs.clientAttr),
	    encode := TRUE);
      | Type.CheckoutDead =>
	  SupMisc.PutCmd(ww.wr, "c",
	    fs.name,
	    fs.tag,
	    fs.date,
	    FileAttr.Encode(fs.serverAttr),
	    encode := TRUE);
      | Type.FileLive =>
	  SupMisc.PutCmd(ww.wr, "V",
	    fs.name,
	    FileAttr.Encode(fs.clientAttr),
	    encode := TRUE);
      | Type.FileDead =>
	  SupMisc.PutCmd(ww.wr, "v",
	    fs.name,
	    FileAttr.Encode(fs.clientAttr),
	    encode := TRUE);
      END;
    EXCEPT Wr.Failure(l) =>
      ww.cannotWrite := TRUE;
      RAISE Wr.Failure(l);
    END;
  END WrPut;

PROCEDURE WrPutRaw(ww: WrWriter; fs: T)
  RAISES {Error, Thread.Alerted, Wr.Failure} =
  VAR
    pcType: PathComp.Type;
    pcName: TEXT;
    gotOne: BOOLEAN;
  BEGIN
    IF ww.cannotWrite THEN  (* Don't even try. *)
      RETURN;
    END;
    TRY
      (* Unfortunately, we have to do the work to keep the pathname
	 compressor in sync. *)
      TRY
	CASE fs.type OF
	| Type.DirDown =>	ww.comp.put(PathComp.Type.DirDown, fs.name);
	| Type.DirUp =>	ww.comp.put(PathComp.Type.DirUp, fs.name);
	ELSE
	  ww.comp.put(PathComp.Type.File, fs.name);
	END;
	(* Calls to putRaw() always originate from a getRaw().  Therefore,
	   we expect them to have all the necessary DirDowns and DirUps.
	   The compressor should return exactly one value, which is whatever
	   we put into it. *)
	gotOne := ww.comp.get(pcType, pcName);
	<* ASSERT gotOne *>
	EVAL ww.decomp.put(pcType, pcName);
	gotOne := ww.comp.get(pcType, pcName);
	<* ASSERT NOT gotOne *>
      EXCEPT PathComp.Error(msg) =>
	RAISE Error(msg);
      END;

      <* ASSERT fs.line # NIL *>
      Wr.PutText(ww.wr, fs.line & "\n");
    EXCEPT Wr.Failure(l) =>
      ww.cannotWrite := TRUE;
      RAISE Wr.Failure(l);
    END;
  END WrPutRaw;

PROCEDURE WrVersion(<*UNUSED*> ww: WrWriter): CARDINAL =
  BEGIN
    RETURN Version;
  END WrVersion;

BEGIN
END FileStatus.
