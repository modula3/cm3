(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE M3DepFindFile;

IMPORT Fmt, Err, TextList, Time, Pathname, OSError, File, FS, Atom, Rd;
IMPORT M3FindFile, M3Extension, M3DirFindFile, M3PathElem;

<*FATAL M3FindFile.Failed*>

REVEAL
  T = Public BRANDED OBJECT
  OVERRIDES
    init := Init;
    rescan := Rescan;
    validateDir := ValidateDir;
    interfaces := Interfaces;
    modules := Modules;
    infoOf := InfoOf;
  END;

PROCEDURE Init(
    newFinder: T;
    exts: M3Extension.TSet;
    rd: Rd.T;
    oldFinder: T := NIL)
    : T
    RAISES {OSError.E} = 
  BEGIN
    EVAL M3DirFindFile.TFinder.init(newFinder, exts, rd, oldFinder);
    Scan(newFinder);
    RETURN newFinder;
  END Init;

PROCEDURE Rescan(f: T): T=
  VAR t := NEW(T).merge(f, NIL);
  BEGIN
    Scan(t);
    RETURN t;
  END Rescan; 
  
PROCEDURE Scan(f: T) RAISES {}=
  VAR
    iter := f.iterate();
    unitName: TEXT;
    dir: M3PathElem.T;
    ext: M3Extension.T;
  BEGIN
    WHILE iter.next(unitName, ext, dir) DO
      IF ext IN M3Extension.Ints OR ext IN M3Extension.Mods THEN
	IF f.getProperty(unitName, ext) = NIL OR NOT dir.readOnly() THEN
	  VAR fullName := Pathname.Join(dir.text(),
	                                unitName, M3Extension.ToText(ext));
              status : File.Status;
              fs := NEW(REF Time.T);
          BEGIN
	    TRY	        
              status := FS.Status(fullName);
              fs^ := status.modificationTime;
              f.setProperty(unitName, ext, fs);
            EXCEPT OSError.E(e) =>
	      Err.Print(
	          Fmt.F("problem reading timestamp for %s - %s\n",
		        fullName, Atom.ToText(e.head)), Err.Severity.Warning);
            END;
          END;
        END;
      END; (* if *)	
    END; (* while *)
  END Scan;

PROCEDURE ValidateDir(t: T; dirName: TEXT): M3PathElem.T RAISES {}=
  VAR
    dirs := t.dirs();
    dirElem := M3PathElem.FromText(dirName, dirName);
  BEGIN
    WHILE dirs # NIL DO
      IF dirs.head = dirElem THEN RETURN dirs.head
      ELSE dirs := dirs.tail
      END;
    END;
    RETURN NIL;
  END ValidateDir;

PROCEDURE Interfaces(
    t, oldt: T;
    VAR (*out*) u: UpdateRec;
    inDir: M3PathElem.T := NIL)
    RAISES {}=
  BEGIN
    Units(oldt, t, u, inDir, M3Extension.Ints);
  END Interfaces;

PROCEDURE Modules(
    t, oldt: T;
    VAR (*out*) u: UpdateRec;
    inDir: M3PathElem.T := NIL)
    RAISES {}=
  BEGIN
    Units(oldt, t, u, inDir, M3Extension.Mods);
  END Modules;

PROCEDURE Units(oldt, t: T;
    VAR (*out*) u: UpdateRec;
    thisDirElem: M3PathElem.T;
    wantedExts: M3Extension.TSet;)
    RAISES {}=
  VAR
    iter: M3DirFindFile.Iter; 
    ext: M3Extension.T;
    unitName: TEXT;
    new: BOOLEAN;
    dirElem, oldDirElem: M3PathElem.T;
  BEGIN
    FOR a := FIRST(Update) TO LAST(Update) DO u[a] := NIL END;
    (* generate 'Deleted' array *)
    IF oldt # NIL THEN
      iter := oldt.iterate();
      WHILE iter.next(unitName, ext, dirElem) DO
        IF ext IN wantedExts THEN
	  TRY
	    EVAL t.find(unitName, ext);
	  EXCEPT
	  | M3FindFile.Failed =>
	      u[Update.Deleted] := TextList.AppendD(u[Update.Deleted],
	                                           TextList.List1(unitName));
          END;
 	END; (* if *)  
     END; (* while *)
    END; (* if *)
    (* generate 'Changed' and 'Added' *)
    iter := t.iterate();
    WHILE iter.next(unitName, ext, dirElem) DO
      IF ext IN wantedExts THEN
        IF thisDirElem = NIL OR dirElem = thisDirElem THEN
           new := TRUE;
           IF oldt # NIL THEN
             TRY
	       oldDirElem := oldt.dirOf(unitName, ext);
	       (* Ok, existed before, check directory *)
	       IF dirElem = oldDirElem THEN
	         new := FALSE;
	         (* ok, check timestamp *)
                 VAR
	           f1 := NARROW(oldt.getProperty(unitName, ext),
                             REF Time.T);
	           f2 := NARROW(t.getProperty(unitName, ext),
                             REF Time.T);
	         BEGIN
	           IF (f1 # f2) AND (f1^ # f2^) THEN
	      	     u[Update.Changed] := TextList.AppendD(u[Update.Changed],
                                              TextList.List1(unitName));
	           END; (* if *)
                 END;
               ELSE
                 (* Changed directory! Treat this as Deleted + Added *)
	         u[Update.Deleted] := TextList.AppendD(u[Update.Deleted],
                                          TextList.List1(unitName));
	       END; (* if *)
             EXCEPT
             | M3FindFile.Failed => (* new is TRUE *)
	     END;
          END;
    
	  IF new THEN
	    (* brand new unit *)
	    u[Update.Added] := TextList.AppendD(u[Update.Added],
                                   TextList.List1(unitName));
	  END;
        END;
      END; (* if *)
    END; (* while *)
  END Units;

PROCEDURE InfoOf(t: T; name: TEXT; ext: M3Extension.T): Info=
  VAR info: Info;
  BEGIN
    TRY
      VAR
        dirElem := t.dirOf(name, ext);
      BEGIN
        info.pathName := Pathname.Join(dirElem.text(),
                                       name, M3Extension.ToText(ext));
        info.timeStamp := NARROW(t.getProperty(name, ext), REF Time.T)^;
      END;
    EXCEPT M3FindFile.Failed =>
    END;
    RETURN info;
  END InfoOf;

BEGIN

END M3DepFindFile.
