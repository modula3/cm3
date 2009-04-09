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

MODULE CheckoutCreator;

IMPORT
  CVProto, FileAttr, FileStatus, FileUpdater, Logger, Pathname,
  RCSDate, RCSRevNum, Rd, Receive, SupFileRec, SupMisc, Thread,
  TokScan, Wr;

REVEAL
  T = Public BRANDED OBJECT
    tag: TEXT;
    date: TEXT;
    revNum: RCSRevNum.T;
    revDate: RCSDate.T;
    fileAttr: FileAttr.T;
    rcsAttr: FileAttr.T;
    isFixup: BOOLEAN;
  OVERRIDES
    init := Init;
    update := Update;
  END;

PROCEDURE Init(self: T;
	       tag: TEXT;
	       date: TEXT;
	       revNum: RCSRevNum.T;
	       revDate: RCSDate.T;
	       fileAttr: FileAttr.T;
	       rcsAttr: FileAttr.T;
	       isFixup: BOOLEAN): T =
  BEGIN
    self.tag := tag;
    self.date := date;
    self.revNum := revNum;
    self.revDate := revDate;
    self.fileAttr := fileAttr;
    self.rcsAttr := rcsAttr;
    self.isFixup := isFixup;
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
      RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted, TokScan.Error,
	      Wr.Failure} =
  VAR
    checkoutName := SupMisc.CheckoutName(name);
    wantSum: TEXT;
  BEGIN
    IF self.isFixup THEN
      Logger.Notice(trace, " Fixup " & checkoutName);
      (* FIXME - If this is an update rather than a creation, we should
	 not set the file's modTime to the revision date. *)
      status.updateType := FileUpdater.UpdateType.Fixup;
    ELSE
      Logger.Notice(trace, " Checkout " & checkoutName);
      status.updateType := FileUpdater.UpdateType.Checkout;
    END;

    self.fileAttr := FileAttr.Override(self.fileAttr,
      FileAttr.ForCheckout(self.rcsAttr, sfr.umask));
    self.fileAttr := FileAttr.MergeDefault(self.fileAttr);

    wantSum := Receive.Escaped(protoRd, wr, withChecksum := TRUE);

    status.fs := NEW(FileStatus.T,
      name := name,
      type := FileStatus.Type.CheckoutLive,
      tag := self.tag,
      date := self.date,
      revNum := self.revNum,
      revDate := self.revDate,
      serverAttr := self.rcsAttr,
      clientAttr := self.fileAttr);
    status.fromAttic := FALSE;
    status.modified := TRUE;
    status.wantSum := wantSum;
  END Update;

BEGIN
END CheckoutCreator.
