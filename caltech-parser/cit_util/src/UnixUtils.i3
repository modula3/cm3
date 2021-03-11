(* $Id$ *)

INTERFACE UnixUtils;
IMPORT OSError;
IMPORT Pathname;

PROCEDURE SymLink(name1, name2: Pathname.T) RAISES {OSError.E};
  (* create a Unix symlink name1 -> name2.  Currently error handling
     is not done properly (please check the source code) *)

END UnixUtils.
