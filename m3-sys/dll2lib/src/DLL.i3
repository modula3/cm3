(* Copyright 1996-2000 Critical Mass, Inc. All rights reserved.    *)
(* See file COPYRIGHT-CMASS for details. *)

INTERFACE DLL;

EXCEPTION Error (TEXT);

TYPE
  ExportList = REF ARRAY OF ExportDesc;

TYPE
  ExportDesc = RECORD
    ord    :  INTEGER;
    offset : INTEGER;
    name   : TEXT;
  END;

PROCEDURE GetExports (file: TEXT): ExportList  RAISES {Error};

END DLL.
