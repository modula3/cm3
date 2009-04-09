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
 * $Id: CheckoutUpdater.m3,v 1.1.1.1 2009-04-09 17:01:33 jkrell Exp $ *)

MODULE CheckoutUpdater;

IMPORT
  CVProto, ErrMsg, FileAttr, FileStatus, FileUpdater, Logger,
  OSError, Pathname, RCSDate, RCSDelta, RCSError, RCSFile, RCSKeyword,
  RCSRevNum, RCSString, Rd, Receive, SupFileRec, SupMisc, Text,
  Thread, TokScan, Wr;

REVEAL
  T = Public BRANDED OBJECT
    tag: TEXT;
    date: TEXT;
    oldRevNum: RCSRevNum.T;
    oldLogLines: CARDINAL;
    fromAttic: BOOLEAN;
    expand: RCSKeyword.ExpandMode;
    rcsAttr: FileAttr.T;
    wantSum: TEXT;
  OVERRIDES
    init := Init;
    update := Update;
  END;

PROCEDURE Init(self: T;
	       tag: TEXT;
	       date: TEXT;
	       oldRevNum: RCSRevNum.T;
	       oldLogLines: CARDINAL;
	       fromAttic: BOOLEAN;
	       expand: RCSKeyword.ExpandMode;
	       rcsAttr: FileAttr.T;
	       wantSum: TEXT): T =
  BEGIN
    self.tag := tag;
    self.date := date;
    self.oldRevNum := oldRevNum;
    self.oldLogLines := oldLogLines;
    self.fromAttic := fromAttic;
    self.expand := expand;
    self.rcsAttr := rcsAttr;
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
    checkoutName := SupMisc.CheckoutName(name);
    srcPath := SupMisc.CatPath(sfr.clientPrefix, checkoutName);
    keywordName: Pathname.T;
    rf: RCSFile.T;
    ts: TokScan.T;
    cmdCh: CHAR;
    revNum: RCSRevNum.T;
    diffBase: TEXT;
    revDate: RCSDate.T;
    author: TEXT;
    delta: RCSDelta.T;
    iter: RCSString.Iterator;
    textLine: RCSString.T;
    xTag: TEXT;
    fileAttr: FileAttr.T;
  BEGIN
    TRY
      Logger.Notice(trace, " Edit " & checkoutName);
      status.updateType := FileUpdater.UpdateType.Edit;
      CASE self.expand OF
      | RCSKeyword.ExpandMode.Default,
	RCSKeyword.ExpandMode.KeyValue,
	RCSKeyword.ExpandMode.KeyValueLocker,
	RCSKeyword.ExpandMode.Key =>
	  rf := RCSFile.Import(srcPath, self.oldRevNum, "nobody", "Exp",
	    self.oldLogLines);
      | RCSKeyword.ExpandMode.Old,
	RCSKeyword.ExpandMode.Binary,
	RCSKeyword.ExpandMode.Value =>
	  rf := RCSFile.Import(srcPath, self.oldRevNum, "nobody", "Exp");
      END;
      TRY
	fileAttr := RCSFile.GetAttr(rf);
	fileAttr := FileAttr.Override(fileAttr,
	  FileAttr.ForCheckout(self.rcsAttr, sfr.umask));
	fileAttr := FileAttr.MaskOut(fileAttr,
	  FileAttr.AttrTypes{FileAttr.AttrType.ModTime});

	revDate := ".";  (* In case we don't get any deltas. *)
	LOOP
	  ts := proto.getCmd(protoRd);
	  cmdCh := ts.getChar("edit command");
	  CASE cmdCh OF
	  | '.' =>
	      EXIT;
	  | 'D' =>  (* Add delta. *)
	      revNum := ts.getToken("revision number");
	      diffBase := ts.getToken("diffBase");
	      revDate := ts.getToken("revision date");
	      author := ts.getToken("author");
	      ts.getEnd("end of \"" & Text.FromChar(cmdCh) & "\" command");
	      Logger.Info(trace, "  Add delta " & revNum & " "
		& revDate & " " & author);
	      TRY
		delta := Receive.Delta(protoRd, rf, revNum, diffBase,
		  revDate, author);
	      EXCEPT RCSError.E(msg) =>
		RAISE FileUpdater.Error("Error adding delta: " & msg);
	      END;
	  ELSE
	    RAISE TokScan.Error("Invalid edit command \""
	      & Text.FromChar(cmdCh) & "\"");
	  END;
	END;

	TRY
	  iter := RCSDelta.GetText(delta);
	EXCEPT RCSError.E(msg) =>
	  RAISE FileUpdater.FixupNeeded("Cannot get edited text: " & msg);
	END;

	IF NOT Text.Equal(self.tag, ".") THEN
	  xTag := self.tag;
	ELSE
	  xTag := NIL;
	END;
	IF self.fromAttic THEN
	  keywordName := SupMisc.AtticName(name);
	ELSE
	  keywordName := name;
	END;
	iter := sfr.expander.expand(iter,
	  hideAttic := proto.v.hidesAtticInCVSHeader,
	  cvsRoot := sfr.keywordPrefix,
	  name := keywordName,
	  delta := delta,
	  tag := xTag,
	  mode := self.expand);

	WHILE iter.next(textLine) DO
	  Wr.PutText(wr, textLine.toText());
	END;

	status.fs := NEW(FileStatus.T,
	  name := name,
	  type := FileStatus.Type.CheckoutLive,
	  tag := self.tag,
	  date := self.date,
	  revNum := revNum,
	  revDate := revDate,
	  clientAttr := fileAttr,
	  serverAttr := self.rcsAttr);
	status.fromAttic := FALSE;
	status.modified := TRUE;
	status.wantSum := self.wantSum;
      FINALLY
	RCSFile.Close(rf);
      END;
    EXCEPT
    | OSError.E(l) =>
(* FIXME - This exception can come from the Close().  We should change it
   to raise RCSError.E instead, analogous to Wr.Close(). *)
	RAISE FileUpdater.Error("Cannot open: " & ErrMsg.StrError(l));
    END;
  END Update;

BEGIN
END CheckoutUpdater.
