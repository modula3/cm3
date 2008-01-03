(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* PkgErr.i3 *)
(* Last modified on Thu Sep 23 10:41:00 PDT 1993 by wobber *)

INTERFACE PkgErr;

IMPORT Atom, AtomList, OSError;

CONST ErrPrefix = "PkgErr.";

TYPE T = Atom.T;       (* includes OSError.E and NetObj.T codes as well *)
                       (* MapError converts to local form where possible *)
     TL = AtomList.T;

VAR  (* errors for all interfaces *)
  IOError, NoRoomInFS, ImportError, Aborted,
  InvalidCredentials,  BadParameter, ServerProblem,
  AccessViolation, NoSuchPackage, NoSuchFile, NoSuchDir: T;

VAR (* PackageObj errors *)
  SourceFailed: T;

(* Siphon errors *)

VAR NoSuchSite, NoReplicas: T;

(* Lock errors *)

VAR
    (* errors raised by PackageDB implementation *)
  PackageNameInUse,       (* can't create, package exists *)
  BadVersionStamp,        (* version timestamps didn't agree !! *)
  StaleVersion,           (* can't commit, (ver < curVer) *)
  OldLocalVersion,        (* can't lock, local version not up to date *)
  OutstandingVersion,     (* can't unlock, local version # last *)
    (* errors specific to PackageLock.def *)
  PackageManagedRemotely, (* Remove,Unlock: package not managed here *)
    (* errors specific to siphon installations *)
  PackageNotManaged,      (* no managing site - database inconsistency *)
  PackageMultiplyManaged, (* multiple managing site - database inconsistency *)
  RemoteLockProblem,      (* problem with lock server at remote site *)
  LockServerDown,         (* problem with lock server somewhere *)
  DirNameInUse,           (* can't create, parent dir exists *)
  ParentDirExists,        (* can't create, parent directory exists *)
  DirNotEmpty : T;        (* can't delete, directory not empty *)


EXCEPTION E(TL);

PROCEDURE MapOSError(code: OSError.Code) : TL;
   (* maps several OS errors into a machine-independent form *)

PROCEDURE Msg(t: TL) : TEXT;
   (* produces a message suitable for user consumption *)

PROCEDURE Raise(a: T; e: TL := NIL) RAISES {E};
   (* raise the supplied error "a", appending "e" *)

END PkgErr.
