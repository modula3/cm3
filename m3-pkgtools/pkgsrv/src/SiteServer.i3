(* Copyright 1989 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* SiteServer.i3 - global package/siphon/lock server params *)
(* Last modified on Mon May 24 12:14:02 PDT 1993 by wobber *)
(*      modified on Mon Nov 18 14:44:49 GMT+1:00 1991 by prusker *)

INTERFACE SiteServer;

IMPORT SiteObj, NetPathRefTbl, FileSys, TextList;

EXCEPTION BadDB(TEXT);

TYPE
  T = SiteObj.ST OBJECT METHODS
    repInfo() : RepInfo;
  END;

  RepInfo = REF RECORD
    mu: MUTEX;                   (* acquire this for table access *)
    reps: NetPathRefTbl.T;       (* maps NetPath.T to RepData *)
    exports: NetPathRefTbl.T;    (* maps NetPath.T to FileSys.FN *)
  END;

      (* in the "exports" table, a NIL value means that we don't
         export links for this path ... this is not an error condition *)

  RepData = REF RECORD
    fn: FileSys.FN;               (* root path in FS for this rep *)
    vols: TextList.T;             (* list of potential FS volume paths *)
  END;


PROCEDURE Init(configFile: TEXT) : T RAISES {BadDB};

END SiteServer.
