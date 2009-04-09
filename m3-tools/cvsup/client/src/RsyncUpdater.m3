(* Copyright 1997-2003 John D. Polstra.
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
 * $Id: RsyncUpdater.m3,v 1.1.1.1 2009-04-09 17:01:39 jkrell Exp $ *)

MODULE RsyncUpdater;

IMPORT
  CVProto, ErrMsg, FileAttr, FileRd, FileStatus, FileUpdater, FS,
  Logger, OSError, Pathname, Rd, Receive, SupFileRec, SupMisc,
  Text, Thread, TokScan, Wr;

REVEAL
  T = Public BRANDED OBJECT
    blockSize: CARDINAL;
    attr: FileAttr.T;
    wantSum: TEXT;
  OVERRIDES
    init := Init;
    update := Update;
  END;

PROCEDURE Init(self: T;
	       blockSize: CARDINAL;
	       attr: FileAttr.T;
	       wantSum: TEXT): T =
  BEGIN
    self.blockSize := blockSize;
    self.attr := attr;
    self.wantSum := wantSum;
    RETURN self;
  END Init;

PROCEDURE Update(self: T;
                 sfr: SupFileRec.T;
		 name: Pathname.T;
      <*UNUSED*> toAttic: BOOLEAN;
                 proto: CVProto.T;
		 trace: Logger.T;
		 protoRd: Rd.T;
	         wr: Wr.T;
		 VAR status: FileUpdater.Status)
      RAISES {FileUpdater.Error, FileUpdater.FixupNeeded, Rd.EndOfFile,
	      Rd.Failure, Thread.Alerted, TokScan.Error, Wr.Failure} =
  VAR
    srcPath := SupMisc.CatPath(sfr.clientPrefix, name);
    rd: Rd.T;
    ts: TokScan.T;
    tok: TEXT;
    blockStart: CARDINAL;
    blockCount: CARDINAL;
    errMsg: TEXT := NIL;
  BEGIN
    Logger.Notice(trace, " Rsync " & name);
    status.updateType := FileUpdater.UpdateType.Rsync;
    TRY
      WITH f = FS.OpenFileReadonly(srcPath) DO
	self.attr := FileAttr.Merge(self.attr, FileAttr.FromFile(f));
	rd := NEW(FileRd.T).init(f);
      END;
    EXCEPT OSError.E(l) =>
      RAISE FileUpdater.Error("Cannot open: " & ErrMsg.StrError(l));
    END;
    TRY
      LOOP
	(* Write 0 or more bytes to the file. *)
	EVAL Receive.Escaped(protoRd, wr, withChecksum := FALSE);

	(* Copy 0 or more blocks to the file. *)
	ts := proto.getCmd(protoRd);
	tok := ts.getToken();
	IF Text.Equal(tok, ".") THEN EXIT END;
	blockStart := TokScan.AtoI(tok);
	blockCount := ts.getInt("block count");
	ts.getEnd("end of block range");

	IF errMsg = NIL THEN
	  TRY
	    CopyBlocks(rd, wr, blockStart, blockCount, self.blockSize);
	  EXCEPT
	  | FileUpdater.Error(msg) =>
	      errMsg := "Block copy failed: " & msg;
	  | Rd.Failure(l) =>
	      errMsg := "Read failure: " & ErrMsg.StrError(l);
	  END;
	END;
      END;
    FINALLY
      TRY
	Rd.Close(rd);
      EXCEPT Rd.Failure(l) =>
	errMsg := "Read failure: " & ErrMsg.StrError(l);
      END;
    END;

    IF errMsg # NIL THEN
      RAISE FileUpdater.FixupNeeded(errMsg);
    END;

    status.fs := NEW(FileStatus.T,
      name := name,
      clientAttr := self.attr,
      serverAttr := self.attr,
      type := FileStatus.Type.FileLive);
    status.fromAttic := FALSE;
    status.modified := TRUE;  (* FIXME - Could be more accurate. *)
    status.wantSum := self.wantSum;
  END Update;

PROCEDURE CopyBlocks(rd: Rd.T;
                     wr: Wr.T;
		     blockStart: CARDINAL;
		     blockCount: CARDINAL;
		     blockSize: CARDINAL)
  RAISES {FileUpdater.Error, Rd.Failure, Thread.Alerted, Wr.Failure} =
  VAR
    byteStart := blockStart * blockSize;
    byteCount := blockCount * blockSize;
    buff: ARRAY [0..8191] OF CHAR;
    nWant, nGot: CARDINAL;
  BEGIN
    IF byteStart + byteCount > Rd.Length(rd) THEN
      RAISE FileUpdater.Error("Blocks out of range");
    END;

    Rd.Seek(rd, byteStart);
    WHILE byteCount > 0 DO
      nWant := MIN(byteCount, NUMBER(buff));
      nGot := Rd.GetSub(rd, SUBARRAY(buff, 0, nWant));
      IF nGot = 0 THEN
	RAISE FileUpdater.Error("Premature EOF");
      END;
      Wr.PutString(wr, SUBARRAY(buff, 0, nGot));
      DEC(byteCount, nGot);
    END;
  END CopyBlocks;

BEGIN
END RsyncUpdater.
