(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Last modified on Thu Mar 30 14:12:11 PST 1995 by mcjones    *)
(*      modified on Sun Jan  8 14:13:46 PST 1995 by kalsow     *)
(*      modified on Wed Mar 16 14:33:40 PST 1994 by wobber     *)
(*      modified on Mon Feb  8 12:35:46 PST 1993 by mjordan    *)

UNSAFE MODULE FilePosix;

IMPORT Ctypes, File, FS, M3toC, OSError, OSErrorPosix, Pipe,
  RegularFile, SchedulerPosix, Terminal, Uerror, Unix, Ustat, Uuio,
  Word;

REVEAL 
  File.T = T BRANDED OBJECT OVERRIDES
    close := FileClose;
    status := FileStatus
  END;

TYPE IntermittentFile = File.T BRANDED OBJECT OVERRIDES
    read := IntermittentRead;
    write := IntermittentWrite
  END;

REVEAL
  Pipe.T = IntermittentFile BRANDED OBJECT END;
  Terminal.T = IntermittentFile BRANDED OBJECT END;
  RegularFile.T = RegularFile.Public BRANDED OBJECT OVERRIDES
    read := RegularFileRead;
    write := RegularFileWrite;
    seek := RegularFileSeek;
    flush := RegularFileFlush;
    lock := RegularFileLock;
    unlock := RegularFileUnlock
  END;

PROCEDURE FileTypeFromStatbuf(READONLY statbuf: Ustat.struct_stat)
  : File.Type =
  BEGIN
    CASE Word.And(statbuf.st_mode, Ustat.S_IFMT) OF
    | Ustat.S_IFCHR =>
        IF IsDevNull(statbuf)
          THEN RETURN RegularFile.FileType
          ELSE RETURN Terminal.FileType
        END
    | Ustat.S_IFPIPE, Ustat.S_IFPORT, Ustat.S_IFSOCK =>
        RETURN Pipe.FileType
    | Ustat.S_IFREG =>
        RETURN RegularFile.FileType
    | Ustat.S_IFDIR =>
        RETURN FS.DirectoryFileType
    ELSE
        RETURN RegularFile.FileType
    END
  END FileTypeFromStatbuf;

PROCEDURE New(fd: INTEGER; ds: DirectionSet): File.T RAISES {OSError.E} =
  VAR statbuf: Ustat.struct_stat; type: File.Type;
  BEGIN
    IF Ustat.fstat(fd, ADR(statbuf)) # 0 THEN OSErrorPosix.Raise() END;
    type := FileTypeFromStatbuf(statbuf);
    IF type = RegularFile.FileType THEN
      RETURN NEW(RegularFile.T, fd := fd, ds := ds)
    END;
    IF type = Terminal.FileType THEN
      RETURN NEW(Terminal.T, fd := fd, ds := ds)
    END;
    IF type = Pipe.FileType THEN
      RETURN NEW(Pipe.T, fd := fd, ds := ds)
    END;
    IF type = FS.DirectoryFileType THEN
      OSErrorPosix.Raise0(Uerror.EISDIR)
    END;
    (* Other... *)
    RETURN NEW(RegularFile.T, fd := fd, ds := ds)
  END New;

PROCEDURE NewPipe(fd: INTEGER; ds: DirectionSet): Pipe.T =
  BEGIN
    RETURN NEW(Pipe.T, fd := fd, ds := ds)
  END NewPipe;

(*---------------------------File methods------------------------------------*)

PROCEDURE FileClose(h: File.T) RAISES {OSError.E} =
  BEGIN
    IF Unix.close(h.fd) < 0 THEN OSErrorPosix.Raise() END
  END FileClose;

PROCEDURE FileStatus(h: File.T): File.Status RAISES {OSError.E} =
  VAR statBuf: Ustat.struct_stat;
    status: File.Status;
  BEGIN
    IF Ustat.fstat(h.fd, ADR(statBuf)) < 0 THEN OSErrorPosix.Raise() END;
    TYPECASE h OF
    | RegularFile.T => status.type := RegularFile.FileType
    | Pipe.T => status.type := Pipe.FileType
    | Terminal.T => status.type := Terminal.FileType
    ELSE <* ASSERT FALSE *>
    END;
    status.modificationTime := FLOAT(statBuf.st_mtime, LONGREAL);
    status.size := statBuf.st_size;
    RETURN status
  END FileStatus;  

(*---------------------------RegularFile methods-----------------------------*)

PROCEDURE RegularFileRead(
    h: RegularFile.T;
    VAR (*OUT*) b: ARRAY OF File.Byte;
    <*UNUSED*> mayBlock: BOOLEAN := TRUE)
  : INTEGER RAISES {OSError.E} =
  VAR p_b: ADDRESS := ADR(b[0]);
  BEGIN
    IF NOT(Direction.Read IN h.ds) THEN BadDirection(); END;
    WITH bytesRead = Uuio.read(h.fd, p_b, NUMBER(b)) DO
      IF bytesRead < 0 THEN OSErrorPosix.Raise() END;
      RETURN bytesRead
    END
  END RegularFileRead;

PROCEDURE RegularFileWrite(
    h: RegularFile.T;
    READONLY b: ARRAY OF File.Byte)
  RAISES {OSError.E} =
  VAR
    p_b: ADDRESS := ADR(b[0]);
    bytes := NUMBER(b);
    bytesWritten: INTEGER;  BEGIN
    IF NOT(Direction.Write IN h.ds) THEN BadDirection(); END;
    LOOP
      bytesWritten := Uuio.write(h.fd, p_b, bytes);
      IF bytesWritten < 0 THEN OSErrorPosix.Raise() END;
      (* Partial write if media is full, quota exceeded, etc. *)
      IF bytesWritten = bytes THEN EXIT END;
      <* ASSERT bytesWritten > 0 *>
      INC(p_b, bytesWritten);
      DEC(bytes, bytesWritten);
    END
  END RegularFileWrite;

PROCEDURE RegularFileSeek(
    h: RegularFile.T; origin: RegularFile.Origin; offset: INTEGER)
  : INTEGER RAISES {OSError.E} =
  BEGIN
    WITH result = Unix.lseek(h.fd, offset, ORD(origin)) DO
      IF result < 0 THEN OSErrorPosix.Raise() END;
      RETURN result
    END
  END RegularFileSeek;

PROCEDURE RegularFileFlush(h: RegularFile.T) RAISES {OSError.E} =
  BEGIN
    IF Unix.fsync(h.fd) < 0 THEN OSErrorPosix.Raise() END
  END RegularFileFlush;

PROCEDURE RegularFileLock(h: RegularFile.T): BOOLEAN RAISES {OSError.E} =
  (* The following initialization is a bit sleazy.  It doesn't
     initialize all the fields.  We depend on each Unix.i3 giving
     a static initialization for any other fields.
     (e.g. freebsd-2/Unix.i3) *)
  VAR flock := Unix.struct_flock {
    l_type := Unix.F_WRLCK,
    l_whence := Unix.L_SET,
    l_start := 0,
    l_len := 0, (* i.e., whole file *)
    l_pid := 0}; (* don't care *)
  BEGIN
    IF Unix.fcntl(h.fd, Unix.F_SETLK, LOOPHOLE(ADR(flock), Ctypes.long)) < 0
    THEN
      IF Uerror.errno = Uerror.EACCES OR
         Uerror.errno = Uerror.EAGAIN THEN
        RETURN FALSE
      END;
      OSErrorPosix.Raise()
    END;
    RETURN TRUE
  END RegularFileLock;

PROCEDURE RegularFileUnlock(h: RegularFile.T) RAISES {OSError.E} =
  (* The following initialization is a bit sleazy.  It doesn't
     initialize all the fields.  We depend on each Unix.i3 giving
     a static initialization for any other fields.
     (e.g. freebsd-2/Unix.i3) *)
  VAR flock := Unix.struct_flock {
    l_type := Unix.F_UNLCK,
    l_whence := Unix.L_SET,
    l_start := 0,
    l_len := 0, (* i.e., whole file *)
    l_pid := 0}; (* don't care *)
  BEGIN
    IF Unix.fcntl(h.fd, Unix.F_SETLK, LOOPHOLE(ADR(flock), Ctypes.long)) < 0
    THEN
      OSErrorPosix.Raise()
    END
  END RegularFileUnlock;

(*---------------------IntermittentFile methods------------------------------*)

PROCEDURE IntermittentRead(
    h: IntermittentFile;
    VAR (*OUT*) b: ARRAY OF File.Byte; 
    mayBlock := TRUE)
  : INTEGER RAISES {OSError.E} =
  VAR
    status, errno: INTEGER;
    old_mode := Unix.fcntl(h.fd, Unix.F_GETFL, 0);
    new_mode := Word.Or(old_mode, Unix.M3_NONBLOCK);
    p_b: ADDRESS := ADR (b[0]);
  BEGIN
    IF NOT(Direction.Read IN h.ds) THEN BadDirection(); END;

    LOOP
      (* Make the read call non-blocking; we cannot set/reset the mode
         at creation/close time, because this may leave the file in an
         unexpected state in the case of a core dump elsewhere. *)

      IF Unix.fcntl(h.fd, Unix.F_SETFL, new_mode) # 0 THEN
	OSErrorPosix.Raise()
      END;

      status := Uuio.read(h.fd, p_b, NUMBER(b));
      errno := Uerror.errno;

      IF Unix.fcntl(h.fd, Unix.F_SETFL, old_mode) # 0 THEN
	OSErrorPosix.Raise()
      END;

      IF status >= 0 THEN
        RETURN status
      ELSIF status = -1
         AND errno # Uerror.EWOULDBLOCK
         AND errno # Uerror.EAGAIN THEN
        OSErrorPosix.Raise0(errno)
      ELSIF NOT mayBlock THEN
        RETURN -1
      END;

      EVAL SchedulerPosix.IOWait(h.fd, TRUE)
    END
  END IntermittentRead;    

PROCEDURE IntermittentWrite(h: File.T; READONLY b: ARRAY OF File.Byte)
  RAISES {OSError.E} =
  VAR
    status, errno: INTEGER;
    old_mode := Unix.fcntl(h.fd, Unix.F_GETFL, 0);
    new_mode := Word.Or(old_mode, Unix.M3_NONBLOCK);
    p := LOOPHOLE (ADR(b[0]), Ctypes.char_star);
    n: Ctypes.int := NUMBER(b);
  BEGIN
    IF NOT(Direction.Write IN h.ds) THEN BadDirection (); END;

    LOOP
      (* Make the write call non-blocking; we cannot set/reset the mode
         at creation/close time, because this may leave the file in an
         unexpected state in the case of a core dump elsewhere. *)

      IF Unix.fcntl(h.fd, Unix.F_SETFL, new_mode) # 0 THEN
	OSErrorPosix.Raise()
      END;

      status := Uuio.write(h.fd, p, n);
      errno := Uerror.errno;

      IF Unix.fcntl(h.fd, Unix.F_SETFL, old_mode) # 0 THEN
	OSErrorPosix.Raise()
      END;

      IF status >= 0 THEN
        p := LOOPHOLE(LOOPHOLE(p, INTEGER) + status, Ctypes.char_star);
        n := n - status;
        IF n = 0 THEN EXIT END
      ELSIF status = -1
         AND errno # Uerror.EWOULDBLOCK
         AND errno # Uerror.EAGAIN THEN
        OSErrorPosix.Raise0(errno)
      END;

      EVAL SchedulerPosix.IOWait(h.fd, FALSE)
    END
  END IntermittentWrite;

(*-------------------------Support procedures--------------------------------*)

VAR
  null_done := FALSE;
  null_stat: Ustat.struct_stat;
  null_fd: INTEGER;

PROCEDURE IsDevNull(READONLY statbuf: Ustat.struct_stat): BOOLEAN RAISES {} =
  VAR result: INTEGER;
  BEGIN
    IF NOT null_done THEN
      null_done := TRUE;
      null_fd := Unix.open(
        M3toC.TtoS("/dev/null"), Unix.O_RDONLY, Unix.Mrwrwrw);
      IF null_fd < 0 THEN RETURN FALSE END;
      result := Ustat.fstat(null_fd, ADR(null_stat));
      EVAL Unix.close(null_fd);
      IF result # 0 THEN null_fd := -1 END
    END;
    RETURN null_fd >= 0 AND statbuf.st_rdev = null_stat.st_rdev
  END IsDevNull;

EXCEPTION IllegalDirection;

PROCEDURE BadDirection () =
  <*FATAL IllegalDirection*>
  BEGIN
    RAISE IllegalDirection;
  END BadDirection;

BEGIN
END FilePosix.
