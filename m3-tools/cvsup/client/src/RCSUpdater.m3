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
 * $Id$ *)

MODULE RCSUpdater;

IMPORT
  Attic, CVProto, ErrMsg, FileAttr, FileStatus, FileUpdater, Logger,
  OSError, Pathname, RCSError, RCSFile, RCSKeyword, RCSRevNum, Rd,
  Receive, SupFileRec, SupMisc, Text, Thread, TokScan, Wr;

REVEAL
  T = Public BRANDED OBJECT
    attr: FileAttr.T;
    rcsOptions: RCSFile.Options;
    wantSum: TEXT;
  OVERRIDES
    init := Init;
    update := Update;
    isRCS := IsRCS;
  END;

PROCEDURE Init(self: T;
	       attr: FileAttr.T;
	       rcsOptions: RCSFile.Options;
	       wantSum: TEXT): T =
  BEGIN
    self.attr := attr;
    self.rcsOptions := rcsOptions;
    self.wantSum := wantSum;
    RETURN self;
  END Init;

PROCEDURE IsRCS(<*UNUSED*> self: T): BOOLEAN =
  BEGIN
    RETURN TRUE;
  END IsRCS;

PROCEDURE Update(self: T;
                 sfr: SupFileRec.T;
		 name: Pathname.T;
		 toAttic: BOOLEAN;
                 proto: CVProto.T;
		 trace: Logger.T;
		 protoRd: Rd.T;
	         wr: Wr.T;
		 VAR status: FileUpdater.Status)
      RAISES {FileUpdater.Error, FileUpdater.FixupNeeded, Rd.EndOfFile,
	      Rd.Failure, Thread.Alerted, TokScan.Error, Wr.Failure} =
  VAR
    srcPath := SupMisc.CatPath(sfr.clientPrefix, name);
    origSrcPath := srcPath;
    fileChanged := FALSE;
    oldAttr: FileAttr.T;
    rf: RCSFile.T;
    ts: TokScan.T;
    cmd: TEXT;
    cmdCh: CHAR;
    branch: TEXT;
    tagName: TEXT;
    revNum: RCSRevNum.T;
    diffBase: RCSRevNum.T;
    date: TEXT;
    author: TEXT;
    expandText: TEXT;
    expandMode: RCSKeyword.ExpandMode;
  PROCEDURE NoteChange() =
    BEGIN
      IF NOT fileChanged THEN
	IF toAttic THEN
	  Logger.Notice(trace, " Edit " & name & " -> Attic");
	ELSE
	  Logger.Notice(trace, " Edit " & name);
	END;
	fileChanged := TRUE;
      END;
    END NoteChange;
  BEGIN
    TRY
      rf := Attic.RCSFileOpenReadonly(srcPath);
      TRY
	oldAttr := RCSFile.GetAttr(rf);
	self.attr := FileAttr.Merge(self.attr, oldAttr);
	(* If the user wants to keep his files exactly the same as
	   the server, then we aim for byte-for-byte equality.  In
	   that case, we have to write the file using the same
	   whitespace conventions that were used in the server's
	   file.  Otherwise, we stick with whatever format is already
	   being used in the client's file. *)
	IF SupFileRec.Option.ExactRCS IN sfr.options
	AND rf.options # self.rcsOptions THEN
	  rf.options := self.rcsOptions;
	  NoteChange();
	END;

	LOOP
	  ts := proto.getCmd(protoRd);
	  cmdCh := ts.getChar("edit command");
	  cmd := Text.FromChar(cmdCh);
	  CASE cmdCh OF
	  | '.' =>
	      EXIT;
	  | 'B' =>  (* Set default branch. *)
	      NoteChange();
	      branch := ts.getToken("default branch");
	      ts.getEnd("end of \"" & cmd & "\" command");
	      Logger.Info(trace, "  Set default branch to " & branch);
	      rf.branch := branch;
	  | 'b' =>  (* Clear default branch. *)
	      NoteChange();
	      ts.getEnd("end of \"" & cmd & "\" command");
	      Logger.Info(trace, "  Clear default branch");
	      rf.branch := NIL;
	  | 'D' =>  (* Add delta. *)
	      NoteChange();
	      revNum := ts.getToken("revision number");
	      diffBase := ts.getToken("diffBase");
	      date := ts.getToken("date");
	      author := ts.getToken("author");
	      ts.getEnd("end of \"" & cmd & "\" command");
	      Logger.Info(trace, "  Add delta " & revNum & " "
		& date & " " & author);
	      TRY
		EVAL Receive.Delta(protoRd, rf, revNum, diffBase,
		  date, author);
	      EXCEPT RCSError.E(msg) =>
		RAISE FileUpdater.Error("Error adding delta: " & msg);
	      END;
	  | 'd' =>  (* Delete delta. *)
	      NoteChange();
	      revNum := ts.getToken("revision number");
	      ts.getEnd("end of \"" & cmd & "\" command");
	      Logger.Info(trace, "  Delete delta " & revNum);
	      TRY
		RCSFile.DeleteDelta(rf, RCSFile.GetDelta(rf, revNum));
	      EXCEPT RCSError.E(msg) =>
		RAISE FileUpdater.Error("Error deleting delta: " & msg);
	      END;
	  | 'E' =>  (* Change keyword expansion mode. *)
	      NoteChange();
	      expandText := ts.getToken("expand mode");
	      ts.getEnd("end of \"" & cmd & "\" command");
	      TRY
		expandMode := RCSKeyword.DecodeExpand(expandText);
	      EXCEPT RCSError.E(msg) => RAISE TokScan.Error(msg) END;
	      IF expandMode = RCSKeyword.ExpandMode.Default THEN
		Logger.Info(trace, "  Set keyword expansion to default");
	      ELSE
		Logger.Info(trace, "  Set keyword expansion to \""
		  & expandText & "\"");
	      END;
	      rf.expand := expandMode;
	  | 'T' =>  (* Add tag. *)
	      NoteChange();
	      tagName := ts.getToken("tag name");
	      revNum := ts.getToken("revision number");
	      ts.getEnd("end of \"" & cmd & "\" command");
	      Logger.Info(trace, "  Add tag " & tagName & " -> " & revNum);
	      TRY
		EVAL RCSFile.AddTag(rf, tagName, revNum);
	      EXCEPT RCSError.E(msg) =>
		RAISE FileUpdater.Error("Error adding tag: " & msg);
	      END;
	  | 't' =>  (* Delete tag. *)
	      NoteChange();
	      tagName := ts.getToken("tag name");
	      revNum := ts.getToken("revision number");
	      ts.getEnd("end of \"" & cmd & "\" command");
	      Logger.Info(trace, "  Delete tag " & tagName
		& " -> " & revNum);
	      TRY
		RCSFile.DeleteTag(rf, tagName, revNum);
	      EXCEPT RCSError.E(msg) =>
		RAISE FileUpdater.Error("Error deleting tag: " & msg);
	      END;
	  ELSE
	    RAISE TokScan.Error("Invalid edit command \"" & cmd & "\"");
	  END;
	END;

	IF fileChanged THEN
	  status.updateType := FileUpdater.UpdateType.Edit;
	ELSE
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
	    IF toAttic THEN msg := msg & " -> Attic" END;
	    Logger.Notice(trace, msg);
	  END;
	END;

	TRY
	  RCSFile.ToWr(rf, wr);
	EXCEPT RCSError.E(msg) =>
	  WITH errMsg = "Invalid RCS file: " & msg DO
	    IF SupFileRec.Option.ExactRCS IN sfr.options THEN
	      RAISE FileUpdater.FixupNeeded(errMsg);
	    ELSE  (* No fixups allowed if local mods might be present. *)
	      RAISE FileUpdater.Error(errMsg);
	    END;
	  END;
	END;

	status.fs := NEW(FileStatus.T, name := name,
	  clientAttr := self.attr, serverAttr := self.attr);
	IF toAttic THEN
	  status.fs.type := FileStatus.Type.FileDead;
	ELSE
	  status.fs.type := FileStatus.Type.FileLive;
	END;
	status.fromAttic := srcPath # origSrcPath;
	status.modified := fileChanged;
	IF SupFileRec.Option.CheckRCS IN sfr.options THEN
	  status.wantSum := self.wantSum;
	ELSE
	  status.wantSum := NIL;
	END;
      FINALLY
	RCSFile.Close(rf);
      END;
    EXCEPT
    | OSError.E(l) =>
(* FIXME - This exception can come from the Close().  We should change it
   to raise RCSError.E instead, analogous to Wr.Close(). *)
	RAISE FileUpdater.Error("Cannot open: " & ErrMsg.StrError(l));
    | RCSError.E(msg) =>
	RAISE FileUpdater.Error("RCS file error: " & msg);
    END;
  END Update;

BEGIN
END RCSUpdater.
