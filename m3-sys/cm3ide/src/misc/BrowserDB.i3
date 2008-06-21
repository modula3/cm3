(* Copyright 1995-96 Critical Mass, Inc. All rights reserved.    *)

(* Browser provides the in-memory database of the browsable packages *)
   
INTERFACE BrowserDB;

IMPORT IntRefTbl, Thread;
IMPORT OS, PkgRoot, Wx;

PROCEDURE Init ();
(* Initializes the database. *)
 
PROCEDURE Refresh (wx: Wx.T := NIL)
  RAISES {Thread.Alerted};
(* Atomically updates the database by rescanning the file system. *)
 
PROCEDURE ScanRoot (root: PkgRoot.T;  wx: Wx.T := NIL)
  RAISES {Thread.Alerted};
(* Scan the contents of "root" and slam it into the current database. *)

PROCEDURE ScanOne (pkg: TEXT;  root: PkgRoot.T;  wx: Wx.T)
  RAISES {Thread.Alerted};
(* Scan one new package and slam it into the current database.  This is DANGEROUS!*)

VAR (*READONLY*)
  db          : DataBase;
  n_updates   : INTEGER     := 0;
  last_update : OS.FileTime := OS.NO_TIME;

TYPE
  DataBase = RECORD
    packages    : IntRefTbl.T; (* package name -> LIST(Pkg.T) *)
    libs        : IntRefTbl.T; (* name -> LIST(Derived.T) *)
    pgms        : IntRefTbl.T; (* name -> LIST(Derived.T) *)
    units       : IntRefTbl.T; (* name -> LIST(Source.T) *)
    exporters   : IntRefTbl.T; (* name -> LIST(impl name)*)
    importers   : IntRefTbl.T; (* name -> LIST(unit name)*)
    types       : IntRefTbl.T; (* uid  -> Type.Info *)
    type_names  : IntRefTbl.T; (* name -> LIST(Type.T) *)
    objects     : IntRefTbl.T; (* uid  -> Type.ObjectInfo *)
  END;

END BrowserDB.
