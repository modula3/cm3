INTERFACE CommandRd;

(* Treat the output on stdout of a shell program as Rd stream.  This can be
   used e.g.  to read gzip compressed files without temporary files. *)

IMPORT Rd, OSError;

TYPE T <: Rd.T;

PROCEDURE Open (command: TEXT; READONLY args: ARRAY OF TEXT; ): T
  RAISES {OSError.E};

END CommandRd.
