(* Copyright (C) 1994, Digital Equipment Corporation               *)
(* All rights reserved.                                            *)
(* See the file COPYRIGHT for a full description.                  *)
(* Last modified on Tue Aug 30 09:45:18 PDT 1994 by mcjones        *)

(* POSIX-specific extensions to FS. *)

UNSAFE MODULE FSPosixExtras EXPORTS FSPosix;

IMPORT Atom, File, FS, M3toC, OSError, OSErrorPosix, Pathname, Pipe,
  RegularFile, Terminal, Unix, Ustat, Word;

PROCEDURE LinkStatus(p: Pathname.T): File.Status RAISES {OSError.E} = 
  VAR status: File.Status; statBuf: Ustat.struct_stat;
  BEGIN
    IF Ustat.lstat(M3toC.TtoS(p), ADR(statBuf)) < 0 THEN OSErrorPosix.Raise() END;
    (* StatBufToStatus(statBuf, status); *)
      status.type := (*FilePosix.*)FileTypeFromStatbuf(statBuf);
      status.modificationTime := FLOAT(statBuf.st_mtime, LONGREAL);
      status.size := statBuf.st_size;
    RETURN status
  END LinkStatus;

(* Copied from os/src/POSIX/FilePosix.i3, with one extra case added: *)
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
    | Ustat.S_IFLNK =>
        RETURN (*FSPosix.*)SymbolicLinkFileType
    ELSE
        RETURN RegularFile.FileType
    END
  END FileTypeFromStatbuf;

(* Copied from os/src/POSIX/FilePosix.i3: *)
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

BEGIN
  SymbolicLinkFileType := Atom.FromText("SymbolicLink")
END FSPosixExtras.
