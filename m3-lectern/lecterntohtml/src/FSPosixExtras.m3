(* Copyright (C) 1994, Digital Equipment Corporation               *)
(* All rights reserved.                                            *)
(* See the file COPYRIGHT for a full description.                  *)
(* Last modified on Tue Aug 30 09:45:18 PDT 1994 by mcjones        *)

(* POSIX-specific extensions to FS. *)

UNSAFE MODULE FSPosixExtras EXPORTS FSPosix;

IMPORT Atom, File, FS, M3toC, OSError, OSErrorPosix, Pathname, Pipe,
  Socket, RegularFile, Terminal, Unix, Ustat, Word;

PROCEDURE LinkStatus(p: Pathname.T): File.Status RAISES {OSError.E} = 
  VAR status: File.Status; statBuf: Ustat.struct_stat;
      p_str := M3toC.SharedTtoS(p);
      result := Ustat.lstat(p_str, ADR(statBuf));
  BEGIN
    M3toC.FreeSharedS(p, p_str);
    IF result < 0 THEN OSErrorPosix.Raise() END;
      status.type := FileTypeFromStatbuf(statBuf);
      status.modificationTime := FLOAT(statBuf.st_mtime, LONGREAL);
      status.size := ORD(statBuf.st_size);
    RETURN status
  END LinkStatus;

PROCEDURE FileTypeFromStatbuf(READONLY statbuf: Ustat.struct_stat)
  : File.Type =
  VAR stat : INTEGER;
  BEGIN
    stat := Word.And(statbuf.st_mode, Ustat.S_IFMT);
    IF stat = Ustat.S_IFCHR  THEN
      IF IsDevNull(statbuf)
        THEN RETURN RegularFile.FileType
        ELSE RETURN Terminal.FileType
      END
    ELSIF stat = Ustat.S_IFIFO THEN 
      RETURN Pipe.FileType
    ELSIF stat = Ustat.S_IFSOCK THEN 
      RETURN Socket.FileType
    ELSIF stat = Ustat.S_IFREG THEN 
      RETURN RegularFile.FileType
    ELSIF stat = Ustat.S_IFDIR  THEN
      RETURN FS.DirectoryFileType
    ELSIF stat = Ustat.S_IFLNK  THEN
      RETURN SymbolicLinkFileType
    ELSE
      RETURN RegularFile.FileType
    END;
  END FileTypeFromStatbuf;

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
        M3toC.FlatTtoS("/dev/null"), Unix.O_RDONLY, Unix.Mrwrwrw);
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
