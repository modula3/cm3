UNSAFE MODULE IsamIO;

IMPORT RegularFile, FilePosix, FS, OSError;
FROM File IMPORT Byte;
IMPORT IO;

CONST
  MaxBlock = 10000;    (* arbitrary max size of record to read/write *)
  debug = FALSE;

TYPE ArrRef = REF ARRAY [0 .. MaxBlock] OF Byte;

PROCEDURE D (lab: TEXT; f: FileRef; in: BOOLEAN; ref, len: INTEGER) =
  BEGIN
    IF debug THEN
      IO.Put(lab & " for file " & f.name & " ");
      IF ref >= 0 THEN
        IF in THEN IO.Put("read "); ELSE IO.Put("wrote : "); END;
        IO.PutInt(len);
        IO.Put(" bytes ");
        IO.Put("at offset : ");
        IO.PutInt(ref);
      END;
      IO.Put("\n");
    END;
  END D;

PROCEDURE PutBlock (f: FileRef; ref, len: INTEGER; src: REFANY)
  RAISES {OSError.E} =
  VAR out: ArrRef;
  BEGIN
    <* ASSERT len < MaxBlock *>
    D("PutBlock ", f, FALSE, ref, len);

    EVAL f.rh.seek(RegularFile.Origin.Beginning, ref);
    out := LOOPHOLE(src, ArrRef);
    f.rh.write(SUBARRAY(out^, 0, len));
  END PutBlock;

PROCEDURE GetBlock (f: FileRef; ref, len: INTEGER; dest: REFANY)
  RAISES {OSError.E,Eof} =
  VAR
    in : ArrRef;
    ret: CARDINAL;
  BEGIN
    <* ASSERT len < MaxBlock *>
    D("GetBlock", f, TRUE, ref, len);

    EVAL f.rh.seek(RegularFile.Origin.Beginning, ref);
    in := LOOPHOLE(dest, ArrRef);
    ret := f.rh.read(SUBARRAY(in^, 0, len));
    IF ret = 0 THEN
      RAISE Eof;
    END;
  END GetBlock;

PROCEDURE BlockRead (f: FileRef; dest: REFANY; len: INTEGER)
  RAISES {OSError.E,Eof} =
  VAR ref: CARDINAL;
  BEGIN
    ref := f.rh.seek(RegularFile.Origin.Current, 0);
    D("BlockRead", f, TRUE, ref, len);
    GetBlock(f, ref, len, dest);
  END BlockRead;

PROCEDURE BlockWrite (f: FileRef; src: REFANY; len: INTEGER)
  RAISES {OSError.E} =
  VAR ref: CARDINAL;
  BEGIN
    ref := f.rh.seek(RegularFile.Origin.Current, 0);
    D("BlockWrite", f, FALSE, ref, len);
    PutBlock(f, ref, len, src);
  END BlockWrite;

PROCEDURE SeekEOF (f: FileRef): INTEGER RAISES {OSError.E} =
  BEGIN
    D("SeekEOF", f, FALSE, -1, -1);
    RETURN f.rh.seek(RegularFile.Origin.End, 0);
  END SeekEOF;

PROCEDURE Flush (f: FileRef) RAISES {OSError.E} =
  BEGIN
    D("Flush", f, FALSE, -1, -1);
    f.rh.flush();
  END Flush;

PROCEDURE FileExists (fname: TEXT): BOOLEAN =
  BEGIN
    TRY
      WITH h = FS.Status(fname) DO EVAL h; END;
    EXCEPT
    | OSError.E => RETURN FALSE;
    END;
    RETURN TRUE;
  END FileExists;

PROCEDURE Delete (f: FileRef) RAISES {OSError.E} =
  BEGIN
    D("Delete", f, FALSE, -1, -1);
    FS.DeleteFile(f.name);
  END Delete;

PROCEDURE Create (fname: TEXT): FileRef RAISES {OSError.E} =
  VAR fr := NEW(FileRef);
  BEGIN
    WITH h = FS.OpenFile(fname, truncate := TRUE) DO
      fr.rh := FilePosix.New(h.fd, FilePosix.ReadWrite);
      fr.name := fname;
    END;
    RETURN fr;
  END Create;

PROCEDURE Open (fname: TEXT): FileRef RAISES {OSError.E} =
  VAR fr := NEW(FileRef);
  BEGIN
    WITH h = FS.OpenFile(fname, truncate := FALSE) DO
      fr.rh := FilePosix.New(h.fd, FilePosix.ReadWrite);
      fr.name := fname;
    END;
    RETURN fr;
  END Open;

PROCEDURE Close (f: FileRef) RAISES {OSError.E} =
  BEGIN
    D("Close", f, FALSE, -1, -1);
    f.rh.close();
  END Close;

BEGIN
END IsamIO.
