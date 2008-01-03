(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* "M3DirFindFile" provides a file finder for a search path
specified as a list of directories (an "M3PathElemList.T"). *)

INTERFACE M3DirFindFile;

IMPORT OSError, Rd;
IMPORT M3Extension, M3FindFile, M3PathElem, M3PathElemList;
FROM M3FindFile IMPORT Failed;

TYPE
  Finder <: FinderPublic;
  FinderPublic = M3FindFile.T OBJECT
  METHODS
    init(exts: M3Extension.TSet;
         dirs: M3PathElemList.T;
         oldFinder: Finder := NIL;
         errorHandler: ErrorHandler := NIL
        ): Finder RAISES {OSError.E};

    dirOf(name: TEXT; ext: M3Extension.T
         ): M3PathElem.T RAISES {Failed};

    dirs(): M3PathElemList.T;

    iterate(): Iter;

    setProperty(name: TEXT; ext: M3Extension.T; 
                value: REFANY) RAISES {Failed};
    getProperty(name: TEXT; ext: M3Extension.T
               ): REFANY RAISES {Failed}; 

    merge(f1, f2: Finder): Finder;
  END;

  ErrorHandler = OBJECT
  METHODS
    callback(dir: M3PathElem.T; ec: OSError.Code): BOOLEAN;
  END;

  TFinder <: TFinderPublic;
  TFinderPublic = Finder OBJECT
  METHODS
    init(
        exts: M3Extension.TSet;
        rd: Rd.T;
        oldFinder: Finder := NIL): Finder RAISES {OSError.E};
  END;

TYPE 
  Iter <: IterPublic;
  IterPublic = OBJECT
  METHODS
    next(VAR (*out*) unitName: TEXT;
         VAR (*out*) ext: M3Extension.T;
         VAR (*out*) dir: M3PathElem.T): BOOLEAN;
    close();
  END;

END M3DirFindFile.

(* "NEW(Finder).init(...)" returns a finder object for the given list
of directories. The "find" method of the returned finder object will
look for the file corresponding to the name and extension in each of
the directories in the list. The search is ordered so if there are two
file names in different directories which match a given name and
extension the file name earliest on the directory list will be
returned. If "oldFinder # NIL", it is used to propagate information
from directories marked as {\it read-only} to the new finder. If an
error occurs reading any of the directories, then if "errorHandler = NIL"
"OSError.E" will be raised. If "errorHandler # NIL", 
"errorHandler.callback(dir, ec)" will be called, where "dir" represents
the directory element that could not be opened and "ec" represents
the code that would have been passed with the "OSError.E" exception.
Processing continues if a result of "TRUE" is returned from the
callback, or aborts otherwise with a result of "NIL" from "Init".

The "exts" method returns the value of "exts" passed to "init".  The
"dirOf" method returns the directory, as an "Elem", of the pathname
returned by the "find" method.  The "dirs" method returns the list of
directories that were returned by the internal call of "Read" when the
finder was created.

The "iterate" method returns an iterator on the files associated with
the finder. The "next" method will return FALSE if the iteration is
exhausted, else it returns "TRUE" and sets the "VAR" parameters to the
unit name, extension and directory of the next file, respectively.

The "setProperty" method associates "value" with file defined by the
pair "name,ext", raising "M3FindFile.Failed" if no such file exists.
Any existing value is overwritten. The "getProperty" returns the value
associated with "name,ext", or "NIL" if no such value has been
associated.


"NEW(T).merge(f1, f2)" merges "f1", with "f2", and returns the finder.
Viewing a finder as defining a set of pairs, "name, ext", the new
finder is defined by the union of "f1" and "f2". Where the finders
overlap, the information from "f1" is taken. It is legal for "f2" to
be "NIL", in which case the result is a copy of "self".

The effect of altering the directories given in the path list while
the find object is in use is undefined (e.g. the implementation of
"Find" may build a cache which may become out of date). Create a new
finder if the directory list or the contents of the directory may have
changed.

A "TFinder" is very like a "Finder", except that the list of directory
names and the set of filenames in those directories is explicitly
encoded on the reader "rd" passed to "NEW(TFinder).init", according to
the following syntax:

| TSpec = { Dir {File} } .
| Dir = @Pathname .
| File = Filename .

*)
