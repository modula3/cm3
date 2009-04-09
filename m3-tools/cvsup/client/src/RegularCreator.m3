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

MODULE RegularCreator;

IMPORT
  Attic, CVProto, FileAttr, FileStatus, FileUpdater, Logger, OSError,
  Pathname, Rd, Receive, SupFileRec, SupMisc, Thread, TokScan, Wr;

REVEAL
  T = Public BRANDED OBJECT
    attr: FileAttr.T;
    isFixup: BOOLEAN;
  OVERRIDES
    init := Init;
    update := Update;
  END;

PROCEDURE Init(self: T;
	       attr: FileAttr.T;
	       isFixup: BOOLEAN): T =
  BEGIN
    self.attr := attr;
    self.isFixup := isFixup;
    RETURN self;
  END Init;

PROCEDURE Update(self: T;
                 sfr: SupFileRec.T;
		 name: Pathname.T;
		 toAttic: BOOLEAN;
      <*UNUSED*> proto: CVProto.T;
		 trace: Logger.T;
		 protoRd: Rd.T;
	         wr: Wr.T;
		 VAR status: FileUpdater.Status)
      RAISES {FileUpdater.FixupNeeded, Rd.EndOfFile, Rd.Failure,
	      Thread.Alerted, TokScan.Error, Wr.Failure} =
  VAR
    srcPath := SupMisc.CatPath(sfr.clientPrefix, name);
    origPath := srcPath;
    traceKind: TEXT;
    wantSum: TEXT;
    defaultAttr: FileAttr.T := NIL;
  BEGIN
    TRY
      defaultAttr := Attic.FileAttrFromPathname(srcPath, follow := FALSE);
    EXCEPT OSError.E => (* Ignore *) END;
    IF self.isFixup THEN
      status.updateType := FileUpdater.UpdateType.Fixup;
      traceKind := "Fixup";
    ELSIF defaultAttr # NIL THEN
      status.updateType := FileUpdater.UpdateType.Replace;
      traceKind := "Replace";
    ELSE
      status.updateType := FileUpdater.UpdateType.Create;
      traceKind := "Create";
    END;
    IF toAttic THEN
      Logger.Notice(trace, " " & traceKind & " " & name & " -> Attic");
    ELSE
      Logger.Notice(trace, " " & traceKind & " " & name);
    END;

    IF defaultAttr # NIL THEN  (* Default from the existing file. *)
      self.attr := FileAttr.Merge(self.attr, defaultAttr);
    ELSE  (* Creating a new file. *)
      self.attr := FileAttr.Umask(self.attr, sfr.umask);
      self.attr := FileAttr.MergeDefault(self.attr);
    END;

    TRY
      wantSum := Receive.Counted(protoRd, wr, FileAttr.GetSize(self.attr),
	withChecksum := TRUE);
    EXCEPT Receive.Error(msg) =>
      RAISE FileUpdater.FixupNeeded(msg);
    END;

    status.fs := NEW(FileStatus.T,
      name := name,
      clientAttr := self.attr,
      serverAttr := self.attr);
    IF toAttic THEN
      status.fs.type := FileStatus.Type.FileDead;
    ELSE
      status.fs.type := FileStatus.Type.FileLive;
    END;
    status.fromAttic := srcPath # origPath;
    status.modified := TRUE;
    status.wantSum := wantSum;
  END Update;

BEGIN
END RegularCreator.
