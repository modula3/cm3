(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Thu Nov  3 15:08:20 PST 1994 by isard      *)

INTERFACE M3Loader;

IMPORT Time;

TYPE
  T <: Public;

TYPE
  Public = OBJECT METHODS
    load_lib     (lib: TEXT): LibModule RAISES {LoadError};
    unload_lib   (handle: LibModule);

    load_obj     (obj: TEXT): ObjModule RAISES {LoadError};
    unload_obj   (handle: ObjModule; silent := FALSE);

    call         (cmd_line, cwd, exename: TEXT; console: BOOLEAN;
                  start_time: Time.T);

    shutdown     ();

    show_stuff   ();
  END;

TYPE
  ObjModule <: ROOT;

TYPE
  LibModule <: ROOT;

TYPE
  ObjList = REF RECORD
    obj       : ObjModule;
    next      : ObjList;
  END;

EXCEPTION
  LoadError;

PROCEDURE New(): T;

END M3Loader.
