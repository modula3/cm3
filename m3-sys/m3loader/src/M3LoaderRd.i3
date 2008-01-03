(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Wed Nov  9 17:47:53 PST 1994 by isard      *)

INTERFACE M3LoaderRd;

IMPORT M3Loader, M3LoaderProcess, RegularFile;

TYPE T <: Public;

TYPE
  Public = OBJECT
    METHODS
      read_object (filename: TEXT): M3Loader.ObjModule
        RAISES {M3Loader.LoadError};
      fill_object (obj: M3Loader.ObjModule; file: RegularFile.T): BOOLEAN;
      scan_library (filename: TEXT): M3Loader.LibModule
        RAISES {M3Loader.LoadError};
    END;

PROCEDURE New (t: M3LoaderProcess.T): T;

END M3LoaderRd.
