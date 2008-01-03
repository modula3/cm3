(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* SiteObj.i3 *)
(* Last modified on Mon Nov 30 11:28:13 PST 1992 by wobber *)

INTERFACE SiteObj;

IMPORT NetObj, Site, Thread;

CONST
  SiteObjName = "SiphonSite";

TYPE
  ST = NetObj.T OBJECT METHODS
    get() : Site.T  RAISES {NetObj.Error, Thread.Alerted};
  END;

PROCEDURE Import(loc: TEXT) : ST RAISES {Site.Error, Thread.Alerted};
  (* See the description of Site.EnvVarName to understand
     how "loc" is interpreted. *)

PROCEDURE SetServerST(st: ST);
  (* Forces Site.Get to use "st" in lieu of Import(Site.EnvVarName). *)

END SiteObj.
