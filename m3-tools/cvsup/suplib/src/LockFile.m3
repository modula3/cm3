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
 * $Id: LockFile.m3,v 1.1.1.1 2009-04-09 17:01:55 jkrell Exp $ *)

MODULE LockFile;

IMPORT
  File, Fmt, FS, OSError, OSErrorPosix, Pathname, Process, RegularFile,
  TempFiles, Text, Uerror;

REVEAL
  T = BRANDED OBJECT
    name: Pathname.T;
    file: RegularFile.T;
  END;

VAR  (* CONST *)
  EagainAtom := OSErrorPosix.ErrnoAtom(Uerror.EAGAIN);
  EnoentAtom := OSErrorPosix.ErrnoAtom(Uerror.ENOENT);

PROCEDURE Lock(name: Pathname.T): T
  RAISES {OSError.E} =
  VAR
    f: File.T;
    rf: RegularFile.T;
    ok: BOOLEAN;
    pidText: TEXT;
    pidBytes: REF ARRAY OF File.Byte;
    lf: T;
  BEGIN
    f := FS.OpenFile(name, truncate := FALSE);
    TRY
      IF f.status().type # RegularFile.FileType THEN
	OSErrorPosix.Raise0(Uerror.EEXIST);
      END;
      rf := NARROW(f, RegularFile.T);

      (* This little business works around a bug in "FilePosix.m3".  POSIX
	 says that if the file is already locked, the "fcntl" call can
	 return either "EACCES" or "EAGAIN", but the code in "FilePosix.m3"
	 only checks for the former. *)
      TRY
	ok := rf.lock();
      EXCEPT OSError.E(l) =>
	IF l.head = EagainAtom THEN ok := FALSE ELSE RAISE OSError.E(l) END;
      END;
      IF NOT ok THEN
	RETURN NIL;
      END;

      (* At this point, we own the lock.  Arrange for the file to be
	 cleaned up if we die prematurely. *)
      TempFiles.Note(name);

      (* Write our PID into the file, for identification.  We don't use
	 a "FileWr.T" for this, because if we did, we would have to keep
	 it open for the lifetime of the lock.  (Closing a "FileWr.T"
	 closes the underlying "File.T" as well, which would unlock it.)
	 To avoid that, we write the bytes directly to the file. *)
      pidText := Fmt.Pad(Fmt.Int(Process.GetMyID()), 10) & "\n";
      pidBytes := NEW(REF ARRAY OF File.Byte, Text.Length(pidText));
      FOR i := FIRST(pidBytes^) TO LAST(pidBytes^) DO
	pidBytes[i] := ORD(Text.GetChar(pidText, i));
      END;
      rf.write(pidBytes^);

      lf := NEW(T, name := name, file := rf);
      f := NIL;
      RETURN lf;
    FINALLY
      IF f # NIL THEN  (* Clean up the file descriptor on failure. *)
	f.close();
      END;
    END;
  END Lock;

PROCEDURE Unlock(lf: T)
  RAISES {OSError.E} =
  BEGIN
    TRY
      TempFiles.Forget(lf.name);
      FS.DeleteFile(lf.name);
    EXCEPT OSError.E(l) =>
      IF l.head # EnoentAtom THEN
	RAISE OSError.E(l);
      END;
    END;
    lf.file.unlock();
    lf.file.close();
  END Unlock;

BEGIN
END LockFile.
