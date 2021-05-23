
(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Thu Nov 10 14:53:39 PST 1994 by isard      *)

INTERFACE M3LoaderObj;

IMPORT IntRefTbl, RegularFile;
IMPORT M3Loader, M3LoaderProcess, M3ID;

REVEAL
  M3Loader.LibModule <: PublicLib;

REVEAL
  M3Loader.ObjModule <: PublicObj;

TYPE
  PublicObj = OBJECT
    name           : M3ID.T;
    library        : M3Loader.LibModule := NIL;
    loaded         := FALSE;
    type           := FileType.Obj;
    imports        : REF ARRAY OF M3ID.T := NIL;
    exports        : REF ARRAY OF Symbol := NIL;
    linker_defined : REF ARRAY OF LinkDef := NIL;
    dllname        := M3ID.NoID;
    dllordinal     : INTEGER := -1;
  METHODS
    fixup () RAISES {M3Loader.LoadError};
    discard (process: M3LoaderProcess.T);
    initialise_data ();
    return_init_data ();
    relocate (addr: INTEGER; relind: INTEGER; load: BOOLEAN);
    show_function (addr: INTEGER): BOOLEAN;
  END;

TYPE
  PublicLib = OBJECT
    name       : M3ID.T;
    file       : RegularFile.T;
    exports    : IntRefTbl.Default := NIL;
    dll        : M3LoaderProcess.DllLib := NIL;
    nmembers   : INTEGER;
    members    : REF ARRAY OF M3Loader.ObjModule;
    next       : M3Loader.LibModule;
  END;

TYPE
  FileType = {Obj, Lib};

TYPE
  Symbol = REF RECORD
    name      :M3ID.T;
    address   : INTEGER := 0;
    parent    : M3Loader.ObjModule := NIL;
  END;

TYPE
  LinkDef = REF RECORD
    name: M3ID.T;
    size: INTEGER;
  END;

END M3LoaderObj.
