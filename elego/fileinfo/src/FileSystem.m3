(*--------------------------------------------------------------------------*)
MODULE FileSystem;

(*--------------------------------------------------------------------------*)
IMPORT Text, FileStatus, APN, RegularFile, Terminal,
       File, OSError, Pathname;
IMPORT (* FSFixed AS *) FS;

(*--------------------------------------------------------------------------*)
PROCEDURE Lookup (p : APN.T) : FileStatus.T =
  BEGIN
    RETURN LookupS (p.denotation());
  END Lookup;

(*--------------------------------------------------------------------------*)
PROCEDURE LookupS (p : Pathname.T) : FileStatus.T =
  VAR res : FileStatus.T;
      stat : File.Status;
  BEGIN
    res := NEW (FileStatus.T);
    TRY
      stat := FS.Status (p);
    EXCEPT
      OSError.E => res.exists := FALSE;
      RETURN res
    END;
    res.exists := TRUE;
    res.isFile := stat.type = RegularFile.FileType;
    res.isDir  := stat.type = FS.DirectoryFileType;
    res.isTerm := stat.type = Terminal.FileType;
    res.modified := stat.modificationTime;
    res.size := stat.size;
    RETURN res;
  END LookupS;

(*--------------------------------------------------------------------------*)
REVEAL
  Iterator = IteratorPublic BRANDED OBJECT
    actualIterator : FS.Iterator 
  OVERRIDES
    next := Next;
    nextWithStatus := NextWithStatus;
    nextWithStatusS := NextWithStatusS;
    close := Close;
  END;

(*--------------------------------------------------------------------------*)
PROCEDURE Next (sf : Iterator; VAR (*OUT*) n : Text.T) : BOOLEAN =
  BEGIN
    RETURN sf.actualIterator.next (n)
  END Next;

(*--------------------------------------------------------------------------*)
PROCEDURE NextWithStatus (
    sf : Iterator; 
    VAR (*OUT*) n : APN.T; 
    VAR (*OUT*) s : FileStatus.T) : BOOLEAN =

  VAR
    thereIsOne : BOOLEAN;
    stat : File.Status;
    t : TEXT;
  BEGIN
    TRY
      thereIsOne := sf.actualIterator # NIL (*C*)AND
                    sf.actualIterator.nextWithStatus(t, stat) ;
    EXCEPT
      OSError.E => RETURN FALSE;
    END;
    IF thereIsOne THEN
      n := APN.New(t);
      s := NEW (FileStatus.T);
      s.exists := TRUE;
      s.isFile := stat.type = RegularFile.FileType;
      s.isDir  := stat.type = FS.DirectoryFileType;
      s.isTerm := stat.type = Terminal.FileType;
      s.modified := stat.modificationTime;
      s.size := stat.size;
    END;
    RETURN thereIsOne
  END NextWithStatus;

(*--------------------------------------------------------------------------*)
PROCEDURE NextWithStatusS (
    sf : Iterator; 
    VAR (*OUT*) n : Pathname.T; 
    VAR (*OUT*) s : FileStatus.T) : BOOLEAN =

  VAR
    thereIsOne : BOOLEAN;
    stat : File.Status;
  BEGIN
    TRY
      thereIsOne := sf.actualIterator # NIL (*C*)AND
                    sf.actualIterator.nextWithStatus(n, stat) ;
    EXCEPT
      OSError.E => RETURN FALSE;
    END;
    IF thereIsOne THEN
      s := NEW (FileStatus.T);
      s.exists := TRUE;
      s.isFile := stat.type = RegularFile.FileType;
      s.isDir  := stat.type = FS.DirectoryFileType;
      s.isTerm := stat.type = Terminal.FileType;
      s.modified := stat.modificationTime;
      s.size := stat.size;
    END;
    RETURN thereIsOne
  END NextWithStatusS;

(*--------------------------------------------------------------------------*)
PROCEDURE Close (sf : Iterator) =
  BEGIN
    IF sf.actualIterator # NIL THEN sf.actualIterator.close () END
  END Close;

(*--------------------------------------------------------------------------*)
PROCEDURE GetIterator (p : APN.T) : Iterator =
  BEGIN
    RETURN GetIteratorS(p.denotation());
  END GetIterator;

(*--------------------------------------------------------------------------*)
PROCEDURE GetIteratorS (p : Pathname.T) : Iterator =
  VAR res : Iterator;
  BEGIN
    res := NEW (Iterator);
    TRY
      res.actualIterator := FS.Iterate (p)
    EXCEPT
      OSError.E => res.actualIterator := NIL
    END;
    RETURN res
  END GetIteratorS;

(*--------------------------------------------------------------------------*)
  BEGIN
  END FileSystem.
 
