(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* PathMap.i3 *)
(* Last modified on Fri May 21 14:14:13 PDT 1993
 by wobber *)

INTERFACE PathMap;

IMPORT FileSys, NetPath, PkgErr, SiteServer, TextList;

TYPE FN = FileSys.FN;

PROCEDURE Init (siteT: SiteServer.T);
  (* initializes volume related data, waits for LockServer to come up *)

PROCEDURE GetReps () : REF ARRAY OF NetPath.Dir;
   (* Returns the set of repositories known at the local replica *)

(* These procedures raise PkgErr.E(BadParameter) if their arguments
   are not legit.  If the target isn't in the map, PkgErr.E(NoSuchDir)
   is raised. *)

PROCEDURE GetVols(dir: NetPath.Dir) : TextList.T RAISES {PkgErr.E};
   (* Returns a list of local directory pathnames all of
      which hold part of the repository "dir". *)

PROCEDURE GetBestVol(dir: NetPath.Dir) : FN RAISES {PkgErr.E};
   (* Returns the local directory pathnames which
      holds part of the repository "dir" and which has the
      most available disk space on its physical medium.
      Any returned pathname includes the local repository prefix. *)

PROCEDURE MapDir(dir: NetPath.Dir) : FN RAISES {PkgErr.E};
   (* maps the repository "dir" into a file name. *)

PROCEDURE MapPkg(pn: NetPath.PN) : FN RAISES {PkgErr.E};
   (* maps the package name  "pn" into a file name. *)

PROCEDURE MapExport(path: NetPath.T) : FN RAISES {PkgErr.E};
   (* maps the export path "path" into a file name, some parent
     of "path" must be in the map. *)

END PathMap.
