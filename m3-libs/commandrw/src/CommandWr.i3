INTERFACE CommandWr;

(* A Wr stream which feeds stdin of a shell program.  This can be used e.g.
   to write gzip compressed files without temporary files. *)

IMPORT Wr, OSError, File;

TYPE T <: Wr.T;

PROCEDURE Open
  (command: TEXT; READONLY args: ARRAY OF TEXT; stdout: File.T := NIL; ): T
  RAISES {OSError.E};

END CommandWr.
