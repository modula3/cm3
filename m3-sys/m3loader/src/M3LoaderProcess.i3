(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Thu Nov 10 15:01:20 PST 1994 by isard      *)

INTERFACE M3LoaderProcess;

IMPORT M3ID, M3LoaderAccess, M3Loader;

TYPE
  T <: Public;

TYPE
  Public = OBJECT
    METHODS
      allocate (size: INTEGER; type: M3LoaderAccess.SegType):
                M3LoaderAccess.Segment RAISES {AllocateError};
      free (seg: M3LoaderAccess.Segment) RAISES {AllocateError};
      start (dlllibs: DllLib; nlibs: INTEGER; cmdline, cwd, exename: TEXT;
             console: BOOLEAN): BOOLEAN;
      initialise_data ();
      restore_data ();
      call (addr: INTEGER; objs: M3Loader.ObjList);
      kill_off_last ();
    END;

EXCEPTION
  AllocateError;

TYPE
  DllSymbol = REF RECORD
    mangledname := M3ID.NoID;
    name        := M3ID.NoID;
    ordinal     := -1;
    address     : INTEGER := 0;
    next        : DllSymbol;
  END;

TYPE
  DllLib = REF RECORD
    name    := M3ID.NoID;
    nsyms   := 0;
    dllsyms : DllSymbol := NIL;
    next    : DllLib := NIL;
  END;

PROCEDURE New (): T;

END M3LoaderProcess.
