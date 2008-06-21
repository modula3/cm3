
MODULE DirectoryListing EXPORTS Main;

IMPORT Atom, Params, FS, IO, OSError, File, RegularFile, Terminal, Text;

PROCEDURE ProcessArg (x: TEXT) =
  VAR status: File.Status;
  BEGIN
    TRY
      status := FS.Status (x);
    EXCEPT OSError.E =>
      IO.Put ("\"" & x & "\" is not a file or directory.\n");
      RETURN;
    END;
    
    IF (status.type = FS.DirectoryFileType) THEN
      ListDirectory (x);
    ELSE
      ListFile (x, status);
    END;
  END ProcessArg;

PROCEDURE ListDirectory (x: TEXT) =
  VAR i: FS.Iterator;  nm: TEXT;  status: File.Status;
  BEGIN
    IO.Put ("\n");
    IO.Put (x);
    IO.Put ("\n-----------\n");
    TRY
      i := FS.Iterate (x);
      WHILE i.nextWithStatus (nm, status) DO
        IF Text.Equal (nm, ".") OR Text.Equal (nm, "..") THEN
          (* ignore *)
        ELSE
          ListFile (nm, status);
        END;
      END;
    EXCEPT OSError.E =>
      IO.Put ("unexpected error while reading directory \"" & x & "\"\n");
    END;
  END ListDirectory;

PROCEDURE ListFile (nm: TEXT;  READONLY status: File.Status) =
  VAR len := Text.Length (nm);
  BEGIN
    IO.Put (nm);
    WHILE (len < 20) DO IO.Put (" ");  INC (len); END;
    IO.Put (" ");
    IF (status.type = FS.DirectoryFileType) THEN
      IO.Put ("directory\n");
    ELSIF (status.type = RegularFile.FileType) THEN
      IO.Put ("file  (length =");
      IO.PutInt (status.size);
      IO.Put (")\n");
    ELSIF (status.type = Terminal.FileType) THEN
      IO.Put ("terminal\n");
    ELSE
      IO.Put ("?? unknown file type: " & Atom.ToText (status.type));
    END;
    
  END ListFile;

BEGIN
  IF Params.Count <= 1 THEN
    ProcessArg (".");
  ELSE
    FOR i := 1 TO Params.Count - 1 DO
      ProcessArg (Params.Get (i));
    END;
  END;
END DirectoryListing.
