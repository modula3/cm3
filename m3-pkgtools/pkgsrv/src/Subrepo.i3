(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* Subrepo.i3 *)
(* Last modified on Tue May  4 17:42:42 PDT 1993 by wobber  *)
(*      modified on Fri Jun 26 18:01:16 GMT+2:00 1992 by prusker *)

INTERFACE Subrepo;

IMPORT LockOps, PackageObj;

FROM LockOps IMPORT SiteName;

TYPE
  R = {Yes, No, Dontknow};

PROCEDURE Query(site: SiteName);
    (* can be called to dynamically update the cache  *)

PROCEDURE Has(site: SiteName; pkg: PackageObj.PN): R;
    (* returns an indication of whether "site" holds
       the subrep of "pkg". *)

PROCEDURE HasDir(site: SiteName; dir: PackageObj.Dir): R;
    (* returns an indication of whether "site" holds
       the subrep "dir". *)

END Subrepo.
