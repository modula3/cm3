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
 *)

MODULE RegularUpdater;

IMPORT
  CVProto, ErrMsg, FileAttr, FileRd, FileStatus, FileUpdater,
  Logger, OSError, Pathname, Rd, RdCopy, Receive, SupFileRec,
  SupMisc, Thread, TokScan, Wr;

REVEAL
  T = Public BRANDED OBJECT
    pos: CARDINAL;
    attr: FileAttr.T;
  OVERRIDES
    init := Init;
    update := Update;
  END;

PROCEDURE Init(self: T;
	       pos: CARDINAL;
	       attr: FileAttr.T): T =
  BEGIN
    self.pos := pos;
    self.attr := attr;
    RETURN self;
  END Init;

PROCEDURE Update(self: T;
                 sfr: SupFileRec.T;
		 name: Pathname.T;
      <*UNUSED*> toAttic: BOOLEAN;
      <*UNUSED*> proto: CVProto.T;
		 trace: Logger.T;
		 protoRd: Rd.T;
	         wr: Wr.T;
		 VAR status: FileUpdater.Status)
      RAISES {FileUpdater.FixupNeeded, Rd.EndOfFile, Rd.Failure,
	      Thread.Alerted, TokScan.Error, Wr.Failure} =
  VAR
    srcPath := SupMisc.CatPath(sfr.clientPrefix, name);
    oldAttr: FileAttr.T;
    rd: Rd.T;
    srcSize: CARDINAL := 0;
    errMsg: TEXT := NIL;
    wantSum: TEXT;
    newSize := FileAttr.GetSize(self.attr);
  BEGIN
    (* First, copy the existing file, unless we're replacing it. *)
    TRY
      oldAttr := FileAttr.FromPathname(srcPath, follow := FALSE);
      self.attr := FileAttr.Merge(self.attr, oldAttr);
      IF newSize = self.pos THEN
	VAR
	  msg: TEXT;
	BEGIN
	  status.updateType := FileUpdater.UpdateType.Touch;
	  IF FileAttr.Equal(self.attr,
	  FileAttr.MaskOut(oldAttr, FileAttr.AllButModTime)) THEN
	    msg := " SetAttrs ";
	  ELSE
	    msg := " Touch ";
	  END;
	  msg := msg & name;
	  Logger.Notice(trace, msg);
	END;
      ELSIF self.pos = 0 THEN
	Logger.Notice(trace, " Replace " & name);
	status.updateType := FileUpdater.UpdateType.Replace;
      ELSE
	Logger.Notice(trace, " Append to " & name);
	status.updateType := FileUpdater.UpdateType.Append;
      END;
      IF self.pos # 0 THEN
	rd := FileRd.Open(srcPath);
	TRY
	  srcSize := RdCopy.ToWriter(rd, wr, self.pos);
	  IF srcSize = self.pos AND NOT Rd.EOF(rd) THEN
	    (* We are appending to an existing file, but it got longer on
	       the client. *)
	    INC(srcSize);
	  END;
	FINALLY
	  Rd.Close(rd);
	END;
      END;
    EXCEPT
    | OSError.E(l) =>
	errMsg := "Cannot open: " & ErrMsg.StrError(l);
    | Rd.Failure(l) =>
	errMsg := "Read failure: " & ErrMsg.StrError(l);
    END;

    (* Append any data from the server. *)
    TRY
      wantSum := Receive.Counted(protoRd, wr, newSize - self.pos,
	withChecksum := TRUE);
    EXCEPT Receive.Error(msg) =>
      RAISE FileUpdater.FixupNeeded(msg);
    END;

    IF errMsg # NIL THEN
      RAISE FileUpdater.FixupNeeded(errMsg);
    END;
    IF srcSize > self.pos THEN
      RAISE FileUpdater.FixupNeeded("File grew on client");
    ELSIF srcSize < self.pos THEN
      RAISE FileUpdater.FixupNeeded("File shrank on client");
    END;

    status.fs := NEW(FileStatus.T,
      name := name,
      clientAttr := self.attr,
      serverAttr := self.attr,
      type := FileStatus.Type.FileLive);
    status.fromAttic := FALSE;
    status.modified := newSize # self.pos;
    status.wantSum := wantSum;
  END Update;

BEGIN
END RegularUpdater.
