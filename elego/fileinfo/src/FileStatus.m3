(*--------------------------------------------------------------------------*)
MODULE FileStatus;

IMPORT FmtTime;

(*--------------------------------------------------------------------------*)
PROCEDURE Repr(self : T) : TEXT =
  VAR t : TEXT;
  BEGIN
    IF self.exists THEN
      t := "exists, ";
    ELSE
      t := "does not exist, ";
    END;
    IF self.isDir THEN
      t := t & "dir, ";
    END;
    IF self.isFile THEN
      t := t & "file, ";
    END;
    IF self.readable THEN
      t := t & "readable, ";
    END;
    IF self.writeable THEN
      t := t & "writeable, ";
    END;
    IF self.traversable THEN
      t := t & "traversable, ";
    END;
    RETURN t & "mtime = " & FmtTime.Long(self.modified);
  END Repr;

(*--------------------------------------------------------------------------*)
BEGIN
END FileStatus.
