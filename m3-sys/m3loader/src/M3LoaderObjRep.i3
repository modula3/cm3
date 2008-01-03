(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Wed Nov  9 18:09:59 PST 1994 by isard      *)

INTERFACE M3LoaderObjRep;

IMPORT SortedIntIntTbl, SortedIntRefTbl;
IMPORT M3Loader, M3LoaderObj, M3ID, M3LoaderAccess;

REVEAL
  M3Loader.ObjModule <: ObjRep;

TYPE
  ObjRep = M3LoaderObj.PublicObj BRANDED OBJECT
      segment       : ARRAY M3LoaderAccess.SegType OF M3LoaderAccess.Segment;
      segsize       : ARRAY M3LoaderAccess.SegType OF INTEGER;
      relocs        : REF ARRAY OF Relocation := NIL;
      nrelocs       : INTEGER;
      start,
      length        : INTEGER;
      whole_file    : BOOLEAN := FALSE;
      labels        : SortedIntRefTbl.Default := NIL;
    END;

TYPE
  ProcLabel = REF RECORD
    name     : M3ID.T;
    baseline : INTEGER := 0;
    lines    : SortedIntIntTbl.Default := NIL;
  END;

TYPE
  Relocation = RECORD
    symbol   : M3ID.T;
    seg      : M3LoaderAccess.SegType;
    offs     : INTEGER;
    type     : INTEGER;
 END;

END M3LoaderObjRep.
