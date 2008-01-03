(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* PSShip.i3 *)
(* Last modified on Fri May 28 13:43:51 PDT 1993 by wobber *)

INTERFACE PSShip;

IMPORT PackageObj, FileSys, PkgErr, PSGet;

TYPE T <: PackageObj.Ship;

PROCEDURE New(
   pn: PackageObj.PN; pkgPath, realPath: FileSys.FN; get: PSGet.T;
   ignoreContent: BOOLEAN; options: PackageObj.ShipOptions): T;

PROCEDURE CommitVersion(
   get: PSGet.T; version: PackageObj.Version): BOOLEAN
   RAISES {PkgErr.E};

END PSShip.
