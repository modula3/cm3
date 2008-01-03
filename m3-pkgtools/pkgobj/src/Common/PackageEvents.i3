(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* PackageEvents.i3 *)
(* Last modified on Fri Mar  4 15:35:04 PST 1994 by wobber *)

INTERFACE PackageEvents;

(* Objects of the following types are passed to the report method 
   of a PackageObj.Monitor object during package ship preparation. *)

IMPORT NetPath;

TYPE Int32  = BITS 32 FOR [-16_7FFFFFFF-1..16_7FFFFFFF];

TYPE
  FileET = {New, Updated, Removed, ChangeMode};
  FileReport = REF RECORD
    type: BITS 32 FOR FileET;
    fromSibling: BITS 32 FOR BOOLEAN := FALSE;
    path: NetPath.T;
    bytesTransferred: Int32 := 0;
    elapsedMSec: Int32 := 0;
  END;

  LinkET = {Installed, Removed, NoDir, Denied, Bad};
  LinkReport = REF RECORD
    type: BITS 32 FOR LinkET;
    pad: Int32 := 0;
    path: NetPath.T;
  END;

  PrepareReport = REF RECORD
    keptBackup: BITS 32 FOR BOOLEAN := FALSE;
    pad: Int32 := 0;
    filesPulled: Int32 := 0;
    filesPulledSibling: Int32 := 0;
    filesFoundInCache: Int32 := 0;
    filesUnchanged: Int32 := 0;
  END;

    (* If "keptBackup", a backup package will be kept
            at commit time ... however this will not be the case
            unless some file has been added or changed ... deletions
            alone won't trigger a backup. *)

END PackageEvents.
