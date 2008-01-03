(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(* Last modified on Thu Apr 22 15:12:11 PDT 1993 by wobber *)

INTERFACE OSSupport;

IMPORT FileWr, OSError;

TYPE
  T <: FileWr.T;

(* operating system dependent stuff for the smalldb package *)

PROCEDURE Sync(wr: T) RAISES {OSError.E};
   (* Flushes "wr".  The attempt to write all modified data
      to stable storage ... to whatever extent permitted by
      the OS.  *)
   (* "wr" must be a file writer *)

PROCEDURE Truncate(wr: T) RAISES {OSError.E};
   (* Truncates the specified writer at the currernt position. *)
   (* "wr" must be a file writer *)

END OSSupport.


