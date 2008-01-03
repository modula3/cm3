(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* PkgErr.m3 *)
(* Last modified on Thu Sep 23 10:41:15 PDT 1993 by wobber *)

MODULE PkgErr;

IMPORT Atom, AtomList, FileSys, OSError;

PROCEDURE Msg(a: AtomList.T) : TEXT =
  VAR res: TEXT;
  BEGIN
    IF a.head = NIL THEN
      res := "";
    ELSE
      res := Atom.ToText(a.head);
    END;
    IF a.tail # NIL THEN
      res := res & "(" & Msg(a.tail) & ")";
    END;
    RETURN res;
  END Msg;

PROCEDURE MapOSError(code: OSError.Code) : TL =
  BEGIN
    CASE FileSys.ClassifyError(code) OF
    | FileSys.ErrorClass.Lookup => RETURN AtomList.Cons(NoSuchFile, code);
    | FileSys.ErrorClass.NoRoomInFS => RETURN AtomList.Cons(NoRoomInFS, code);
    | FileSys.ErrorClass.IO => RETURN AtomList.Cons(IOError, code);
    | FileSys.ErrorClass.Access => RETURN AtomList.Cons(AccessViolation, code);
    ELSE
    END;
    RETURN code;
  END MapOSError;

PROCEDURE Raise(a: Atom.T; e: TL := NIL) RAISES {E} =
  BEGIN
    RAISE E(AtomList.Cons(a, e));
  END Raise;

PROCEDURE MakeErr(t: TEXT) : Atom.T =
  BEGIN
    RETURN Atom.FromText(ErrPrefix & t);
  END MakeErr;

BEGIN
  AccessViolation := MakeErr("AccessViolation");
  IOError := MakeErr("IOError");
  NoRoomInFS := MakeErr("NoRoomInFS");
  ImportError := MakeErr("ImportError");
  InvalidCredentials := MakeErr("InvalidCredentials");
  BadParameter := MakeErr("BadParameter");
  ServerProblem := MakeErr("ServerProblem");
  NoSuchPackage := MakeErr("NoSuchPackage");
  NoSuchFile := MakeErr("NoSuchFile");
  NoSuchDir := MakeErr("NoSuchDir");
  Aborted := MakeErr("Aborted");
 
  SourceFailed := MakeErr("SourceFailed");

  NoSuchSite := MakeErr("NoSuchSite");
  NoReplicas := MakeErr("NoReplicas");

  PackageNameInUse := MakeErr("PackageNameInUse");
  BadVersionStamp := MakeErr("BadVersionStamp");
  StaleVersion := MakeErr("StaleVersionn");
  OldLocalVersion := MakeErr("OldLocalVersion");
  OutstandingVersion := MakeErr("OutstandingVersion");
  PackageManagedRemotely := MakeErr("PackageManagedRemotely");
  PackageNotManaged := MakeErr("PackageNotManaged");
  PackageMultiplyManaged := MakeErr("PackageMultiplyManaged");
  RemoteLockProblem := MakeErr("RemoteLockProblem");
  LockServerDown := MakeErr("LockServerDown");
  NoSuchDir := MakeErr("NoSuchDir");
  DirNameInUse := MakeErr("DirNameInUse");
  ParentDirExists := MakeErr("ParentDirExists");
  DirNotEmpty := MakeErr("DirNotEmpty");
END PkgErr.
