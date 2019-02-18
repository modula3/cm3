(* $Id$ *)

INTERFACE Chmod;
IMPORT Pathname;

EXCEPTION Error;

PROCEDURE chmod(path : Pathname.T; mode : CARDINAL) RAISES { Error };

END Chmod.
