(*--------------------------------------------------------------------------*)
INTERFACE FSUtils;

IMPORT Pathname;

(*--------------------------------------------------------------------------*)
PROCEDURE Exists(fn : Pathname.T) : BOOLEAN;
  (* <=> `fn' exists *)

(*--------------------------------------------------------------------------*)
PROCEDURE IsDir(fn : Pathname.T) : BOOLEAN;
  (* <=> `fn' is a directory *)

(*--------------------------------------------------------------------------*)
PROCEDURE IsFile(fn : Pathname.T) : BOOLEAN;
  (* <=> `fn' is an ordinary file *)

(*--------------------------------------------------------------------------*)
PROCEDURE MakeDir(path : Pathname.T);
  (* Build all directories in `path', if they do not exist, or crash. *)

END FSUtils.
