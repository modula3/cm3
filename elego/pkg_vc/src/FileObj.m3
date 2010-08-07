(*---------------------------------------------------------------------------*)
MODULE FileObj;

IMPORT APN AS APN, File, RegularFile, Terminal;
IMPORT (* FSFixed AS *) FS;

(*---------------------------------------------------------------------------*)
PROCEDURE Exists(fn : APN.T) : BOOLEAN =
  VAR s : File.Status;
  BEGIN
    TRY
      s := FS.Status(fn.denotation());
    EXCEPT ELSE
      RETURN FALSE;
    END;
    RETURN TRUE;
  END Exists;

(*---------------------------------------------------------------------------*)
PROCEDURE IsDir(fn : APN.T) : BOOLEAN =
  VAR s : File.Status;
  BEGIN
    TRY
      s := FS.Status(fn.denotation());
    EXCEPT ELSE
      RETURN FALSE;
    END;
    RETURN s.type = FS.DirectoryFileType;
  END IsDir;

(*---------------------------------------------------------------------------*)
PROCEDURE IsFile(fn : APN.T) : BOOLEAN =
  VAR s : File.Status;
  BEGIN
    TRY
      s := FS.Status(fn.denotation());
    EXCEPT ELSE
      RETURN FALSE;
    END;
    RETURN s.type = RegularFile.FileType;
  END IsFile;

(*---------------------------------------------------------------------------*)
PROCEDURE IsTerminal(fn : APN.T) : BOOLEAN =
  VAR s : File.Status;
  BEGIN
    TRY
      s := FS.Status(fn.denotation());
    EXCEPT ELSE
      RETURN FALSE;
    END;
    RETURN s.type = Terminal.FileType;
  END IsTerminal;


BEGIN
END FileObj.
