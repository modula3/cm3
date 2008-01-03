(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* "M3DepFindFile" provides a file-finder with basic support
for dependency analysis in terms of file timestamps. *)

INTERFACE M3DepFindFile;

IMPORT Time, TextList, Rd, OSError;
IMPORT M3Extension, M3PathElem, M3DirFindFile;

TYPE T <: Public;
  Public = M3DirFindFile.TFinder OBJECT
  METHODS
    init(exts: M3Extension.TSet;
         rd: Rd.T;
         oldFinder: T := NIL): T RAISES {OSError.E};
    rescan(): T;
    validateDir(dir: TEXT): M3PathElem.T;
    interfaces(oldt: T; VAR (*out*) u: UpdateRec;
               inDir: M3PathElem.T := NIL);
    modules(oldt: T; VAR (*out*) u: UpdateRec;
            inDir: M3PathElem.T := NIL);
    infoOf(name: TEXT; ext: M3Extension.T): Info;
  END;

TYPE
  Update = {Deleted, Added, Changed};
  UpdateRec = ARRAY Update OF TextList.T;

  Info = RECORD 
    pathName: TEXT := NIL;
    timeStamp: Time.T := 0.0d0 
  END;

END M3DepFindFile.

(* "NEW(T).init(exts, rd, oldFinder)" first creates a finder by
calling:

| M3DirFindFile.TFinder.Init(exts, rd, oldFinder) 

It then reads and stores the timestamps of the files that can be
accessed from the finder.

The "rescan" method makes a copy of itself, and then updates the
timestamps on the files in the mutable directories, that is those
search path elements for which "elem.readOnly()" returns "FALSE".

The "validate" method checks if "dir" is in the list associated with 
its finder and returns the associated "M3PathElem.T", if so. 
Otherwise it returns "NIL".

The "interfaces" method returns the units in that are newer than in
"oldt".  If "oldt = NIL", all interfaces are returned.  If "inDir #
NIL", then the search is restricted to units in directory "inDir".

The "infoOf" method returns the pathname and file system time stamp for
the unit denoted by " name, ext".  The "pathName" field will be "NIL" if
such a unit does not exist.  The "timeStamp" field will be "NIL" if it
cannot be read. *)
