INTERFACE IsamIO;

IMPORT RegularFile,OSError;

EXCEPTION Eof;

TYPE
  FileRef = REF IsamFile;
  IsamFile = RECORD
    rh : RegularFile.T;
    name : TEXT;
  END;

PROCEDURE Create(fname : TEXT) : FileRef RAISES {OSError.E};
PROCEDURE Open(fname : TEXT) : FileRef RAISES {OSError.E};
PROCEDURE Close (f: FileRef) RAISES {OSError.E};
PROCEDURE Flush(f : FileRef) RAISES {OSError.E};
PROCEDURE FileExists(fname : TEXT) : BOOLEAN;
PROCEDURE Delete(f : FileRef) RAISES {OSError.E};
PROCEDURE SeekEOF (f: FileRef) : INTEGER RAISES {OSError.E};
PROCEDURE GetBlock(f : FileRef; ref, len: INTEGER; dest: REFANY) RAISES{OSError.E,Eof};
PROCEDURE PutBlock(f : FileRef; ref, len: INTEGER; src : REFANY) RAISES{OSError.E};
PROCEDURE BlockRead (f: FileRef; dest: REFANY; len: INTEGER) RAISES {OSError.E,Eof};
PROCEDURE BlockWrite (f: FileRef; src : REFANY; len: INTEGER) RAISES {OSError.E};

END IsamIO.
