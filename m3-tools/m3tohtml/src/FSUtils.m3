(*--------------------------------------------------------------------------*)
MODULE FSUtils;

IMPORT Pathname, File, FS, RegularFile, OSError, Process;

(*--------------------------------------------------------------------------*)
PROCEDURE Exists(fn : Pathname.T) : BOOLEAN =
  VAR s : File.Status;
  BEGIN
    TRY
      s := FS.Status(fn);
    EXCEPT ELSE
      RETURN FALSE;
    END;
    RETURN TRUE;
  END Exists;

(*--------------------------------------------------------------------------*)
PROCEDURE IsDir(fn : Pathname.T) : BOOLEAN =
  VAR s : File.Status;
  BEGIN
    TRY
      s := FS.Status(fn);
    EXCEPT ELSE
      RETURN FALSE;
    END;
    RETURN s.type = FS.DirectoryFileType;
  END IsDir;

(*--------------------------------------------------------------------------*)
PROCEDURE IsFile(fn : Pathname.T) : BOOLEAN =
  VAR s : File.Status;
  BEGIN
    TRY
      s := FS.Status(fn);
    EXCEPT ELSE
      RETURN FALSE;
    END;
    RETURN s.type = RegularFile.FileType;
  END IsFile;

(*--------------------------------------------------------------------------*)
PROCEDURE MakeDir(path : Pathname.T) =
  VAR
    arcs  : Pathname.Arcs;
    iarcs : Pathname.Arcs;
    ipath : Pathname.T;
  BEGIN
    TRY
      arcs  := Pathname.Decompose(path);
      iarcs := NEW(Pathname.Arcs).init(arcs.size());
    EXCEPT
      Pathname.Invalid => Process.Crash("internal error: invalid pathname");
    END;
    FOR i := 0 TO arcs.size() - 1 DO
      iarcs.addhi(arcs.get(i));
      TRY
        ipath := Pathname.Compose(iarcs);
      EXCEPT
        Pathname.Invalid => Process.Crash("internal error: invalid pathname");
      END;
      IF arcs.get(i) # NIL THEN
        IF NOT IsDir(ipath) THEN
          IF Exists(ipath) THEN
            Process.Crash("cannot create directory, file exists " & ipath);
          END;
          TRY
            FS.CreateDirectory(ipath);
          EXCEPT
            OSError.E => Process.Crash("cannot create directory " & ipath);
          END;
        END;
      END;
    END;
  END MakeDir;

(*--------------------------------------------------------------------------*)
BEGIN
END FSUtils.
