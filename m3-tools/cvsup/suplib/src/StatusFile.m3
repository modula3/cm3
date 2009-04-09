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
 * $Id: StatusFile.m3,v 1.1.1.1 2009-04-09 17:02:00 jkrell Exp $ *)

MODULE StatusFile;

IMPORT
  ErrMsg, FileAttr, FileRd, FileStatus, FileStatusRaw, FileWr, FS,
  OSError, Pathname, Rd, SupFileRec, SupMisc, TempFiles, Thread, Time,
  Wr;

TYPE
  TRep = T OBJECT
    srcPath: Pathname.T;
    reader: FileStatus.Reader;
    tempPath: Pathname.T := NIL;
    destPath: Pathname.T := NIL;
    writer: FileStatus.Writer := NIL;
    curFS: FileStatus.T := NIL;
    dirty := FALSE;
    rdVersion, wrVersion := 0;
  OVERRIDES
    get := Get;
    delete := Delete;
    put := Put;
    close := Close;
  END;

PROCEDURE Open(sfr: SupFileRec.T;
               scanTime: Time.T := -1.0d0;
	       destDir: Pathname.T := NIL;
	       readFromDestDir := FALSE): T
  RAISES {FileStatus.Error, Thread.Alerted} =
  VAR
    self := NEW(TRep);
    basePath := SupMisc.CatPath(
      SupMisc.ResolvePath(sfr.clientBase, sfr.clientCollDir),
	SupMisc.CatPath(sfr.collection, SupMisc.StatusFileName(sfr)));
    attr: FileAttr.T;
  BEGIN
    self.srcPath := basePath;
    IF readFromDestDir AND destDir # NIL THEN
      self.srcPath := SupMisc.CatPath(destDir, self.srcPath);
    END;
    TRY
      self.reader := FileStatus.FromRd(FileRd.Open(self.srcPath));
    EXCEPT
    | FileStatus.Error(msg) =>
	RAISE FileStatus.Error("Error in \"" & self.srcPath
	  & "\": " & msg);
    | OSError.E =>  (* Just use an empty source. *)
	self.reader := FileStatus.FromNull();
    | Rd.Failure(list) =>
	RAISE FileStatus.Error("Read failure on \"" & self.srcPath
	  & "\": " & ErrMsg.StrError(list));
    END;
    self.rdVersion := self.reader.version();

    IF scanTime # -1.0d0 THEN  (* Open for writing, too. *)
      self.destPath := basePath;
      IF destDir # NIL THEN
	self.destPath := SupMisc.CatPath(destDir, self.destPath);
      END;
      self.tempPath := SupMisc.TempName(self.destPath);
      TRY
	SupMisc.MakeDirectories(self.tempPath, sfr.umask);
      EXCEPT OSError.E(list) =>
	RAISE FileStatus.Error("Cannot create directories leading to \""
	  & self.tempPath & "\": " & ErrMsg.StrError(list));
      END;
      TRY
	WITH wr = FileWr.Open(self.tempPath) DO
	  TempFiles.Note(self.tempPath);
	  attr := NEW(FileAttr.T).init(FileAttr.FileType.File);
	  attr := FileAttr.MergeDefault(attr);
	  attr := FileAttr.Umask(attr, sfr.umask);
	  EVAL FileAttr.Install(attr, self.tempPath);
	  self.writer := FileStatus.ToWr(wr, scanTime);
	  self.wrVersion := self.writer.version();
	  IF self.wrVersion # self.rdVersion
	  OR ROUND(scanTime) # ROUND(self.reader.scanTime()) THEN
	    self.dirty := TRUE;
	  END;
	END;
      EXCEPT
      | OSError.E(list) =>
	  RAISE FileStatus.Error("Cannot create \"" & self.tempPath
	    & "\": " & ErrMsg.StrError(list));
      | Wr.Failure(list) =>
	  RAISE FileStatus.Error("Write failure on \"" & self.tempPath
	    & "\": " & ErrMsg.StrError(list));
      END;
    ELSE
      self.wrVersion := self.rdVersion;
    END;

    RETURN self;
  END Open;

PROCEDURE Get(self: TRep;
              name: Pathname.T;
	      isDirUp := FALSE;
	      deleteTo := FALSE): FileStatus.T
  RAISES {FileStatus.Error, Thread.Alerted} =
  VAR
    c: [-1..1];
    key := NEW(FileStatus.T, name := name);
  BEGIN
    TRY
      IF isDirUp THEN
	key.type := FileStatus.Type.DirUp;
      ELSE
	key.type := FileStatus.Type.FileLive;  (* Anything except DirUp. *)
      END;
      IF self.curFS = NIL THEN
	self.curFS := self.reader.get();
      END;

      (* At this point, self.curFS is non-NIL and cooked. *)

      c := FileStatus.Compare(self.curFS, key);
      IF c < 0 THEN  (* Must scan forward. *)
	IF self.writer # NIL AND NOT deleteTo THEN
	  (* Write out cooked self.curFS. *)
	  self.writer.put(self.curFS);
	END;
	IF self.rdVersion = self.wrVersion THEN
	  (* Do it the fast way, with raw gets and puts. *)
	  TRY
	    LOOP
	      self.curFS := self.reader.getRaw();
	      c := FileStatus.Compare(self.curFS, key);
	      IF c >= 0 THEN EXIT END;
	      IF self.writer # NIL AND NOT deleteTo THEN
		self.writer.putRaw(self.curFS);
	      END;
	    END;
	  FINALLY
	    FileStatusRaw.MakeCooked(self.curFS, self.rdVersion);
	  END;
	ELSE  (* We have to convert file formats. *)
	  LOOP
	    self.curFS := self.reader.get();
	    c := FileStatus.Compare(self.curFS, key);
	    IF c >= 0 THEN EXIT END;
	    IF self.writer # NIL AND NOT deleteTo THEN
	      self.writer.put(self.curFS);
	    END;
	  END;
	END;
      END;

      (* At this point, self.curFS is again non-NIL and cooked. *)

      IF c # 0 THEN
	RETURN NIL;
      END;
      RETURN self.curFS;
    EXCEPT
    | FileStatus.Error(msg) =>
	RAISE FileStatus.Error("Error in \"" & self.srcPath & "\": " & msg);
    | Rd.EndOfFile =>
	self.curFS := NIL;
	RETURN NIL;
    | Rd.Failure(list) =>
	RAISE FileStatus.Error("Read failure on \"" & self.srcPath & "\": " &
	  ErrMsg.StrError(list));
    | Wr.Failure(list) =>
	RAISE FileStatus.Error("Write failure on \"" & self.tempPath & "\": " &
	  ErrMsg.StrError(list));
    END;
  END Get;

PROCEDURE Delete(self: TRep; name: Pathname.T; isDirUp := FALSE)
  RAISES {FileStatus.Error, Thread.Alerted} =
  VAR
    fs := self.get(name, isDirUp);
  BEGIN
    IF fs # NIL THEN
      self.curFS := NIL;
      self.dirty := TRUE;
    END;
  END Delete;

PROCEDURE Put(self: TRep; fs: FileStatus.T)
  RAISES {FileStatus.Error, Thread.Alerted} =
  VAR
    oldFS := self.get(fs.name, isDirUp := fs.type = FileStatus.Type.DirUp);
  BEGIN
    TRY
      IF oldFS # NIL THEN
	IF oldFS.type = FileStatus.Type.DirDown THEN
	  CASE fs.type OF
	  | FileStatus.Type.DirDown =>
	      (* OK *)
	  | FileStatus.Type.CheckoutLive,
	    FileStatus.Type.CheckoutDead,
	    FileStatus.Type.FileLive,
	    FileStatus.Type.FileDead =>
	      (* We are replacing a directory with a file.  Delete
		 all entries inside the directory we are replacing. *)
	      oldFS := self.get(fs.name, isDirUp := TRUE, deleteTo := TRUE);
	      <* ASSERT oldFS # NIL *>
	  | FileStatus.Type.DirUp =>
	      <* ASSERT FALSE *>	(* DirUp should never match DirDown *)
	  END;
	END;
	self.curFS := fs;
      ELSIF self.curFS = NIL THEN
	self.curFS := fs;
      ELSE
	self.writer.put(fs);
      END;
      self.dirty := TRUE;
    EXCEPT Wr.Failure(list) =>
      RAISE FileStatus.Error("Write failure on \"" & self.tempPath & "\": " &
	ErrMsg.StrError(list));
    END;
  END Put;

PROCEDURE Close(self: TRep)
  RAISES {FileStatus.Error, Thread.Alerted} =
  BEGIN
    TRY
      IF self.writer # NIL THEN
	IF self.dirty THEN
	  IF self.curFS # NIL THEN
	    self.writer.put(self.curFS);
	    self.curFS := NIL;
	  END;

	  (* Copy the rest of the source file to the destination. *)
	  TRY
	    IF self.rdVersion = self.wrVersion THEN
	      (* We can do it the fast way, by copying raw entries
	         without having to decode and encode them entirely.

		 But to start off, we have to get and put one
		 non-raw entry, in order to ensure that the directory
		 state is properly in sync.  This came up in real
		 life, where the server had deleted an entire
		 directory.  The client got through deleting the
		 DirDown entry plus several files, then got
		 interrupted either by the GUI or by hitting the
		 deletion limit.  The next putRaw thus tried to
		 output a file in a directory that the writer didn't
		 think it should have been in, causing an assertion
		 failure. *)
	      self.writer.put(self.reader.get());
	      LOOP
		self.writer.putRaw(self.reader.getRaw());
	      END;
	    ELSE  (* We have to decode and encode each entry. *)
	      LOOP
		self.writer.put(self.reader.get());
	      END;
	    END;
	  EXCEPT Rd.EndOfFile => (* Leave the loop. *) END;

	  self.writer.close();
	  TRY
	    TempFiles.Forget(self.tempPath);
	    FS.Rename(self.tempPath, self.destPath);
	  EXCEPT OSError.E(list) =>
	    RAISE FileStatus.Error("Cannot rename \"" & self.tempPath
	      & "\" to \"" & self.destPath & "\": " & ErrMsg.StrError(list));
	  END;
	ELSE  (* Not dirty, so we can just discard the tempfile. *)
	  self.writer.close();
	  TRY
	    TempFiles.Forget(self.tempPath);
	    FS.DeleteFile(self.tempPath);
	  EXCEPT OSError.E(list) =>
	    RAISE FileStatus.Error("Cannot delete \"" & self.tempPath
	      & "\": " & ErrMsg.StrError(list));
	  END;
	END;
      END;
      self.reader.close();
    EXCEPT
    | FileStatus.Error(msg) =>
	RAISE FileStatus.Error("Error in \"" & self.srcPath & "\": " & msg);
    | Rd.Failure(list) =>
	RAISE FileStatus.Error("Read failure on \"" & self.srcPath & "\": " &
	  ErrMsg.StrError(list));
    | Wr.Failure(list) =>
	RAISE FileStatus.Error("Write failure on \"" & self.tempPath & "\": " &
	  ErrMsg.StrError(list));
    END;
  END Close;

BEGIN
END StatusFile.
