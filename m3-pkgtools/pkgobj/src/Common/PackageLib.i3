(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* PackageLib.def *)
(* Last modified on Wed May 19 18:18:37 PDT 1993 by wobber *)

INTERFACE PackageLib;

IMPORT FileSys, OSError, PackageObj, Thread;

EXCEPTION Stop;

TYPE
  FN = FileSys.FN;

  DiffType = {
    Same, NoSrc, NoDest, SrcNewer, SrcOlder,
    LinksDiffer, LengthsDiffer, TypesDiffer, ModesDiffer};

  DiffClosure = OBJECT METHODS
    report(dir: FN; diff: DiffType; elem: PackageObj.DirElem)
      RAISES {Stop, Thread.Alerted};
  END;

  EnumClosure = OBJECT METHODS
    acquire() : (*dir*) FileSys.FN;
    release();
  END;
  
CONST
  VersionFile = ".version";
  ExportLinkFile = ".exportLinks";

PROCEDURE Enumerate (e: EnumClosure): PackageObj.DirEnum
    RAISES {OSError.E};

PROCEDURE Compare(src, dest: PackageObj.DirEnum; cl: DiffClosure)
    RAISES {Thread.Alerted};

PROCEDURE SetDirDates(dir: FN; enum: PackageObj.DirEnum)
    RAISES {OSError.E};

PROCEDURE EmptySource() : PackageObj.Source;

END PackageLib.
