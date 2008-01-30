(*--------------------------------------------------------------------------*)
INTERFACE DirStack;

IMPORT Pathname;

EXCEPTION Error(TEXT);

(*--------------------------------------------------------------------------*)
PROCEDURE PushDir(dir : Pathname.T) RAISES {Error};
  (* Push the current directory onto the directory stack and change
     the working directory to `dir'. *)

(*--------------------------------------------------------------------------*)
PROCEDURE PopDir() RAISES {Error};
  (* Pop the top directory from the stack and change the current
     working directory to it. *)

(*--------------------------------------------------------------------------*)
PROCEDURE GetWorkingDir() : Pathname.T RAISES {Error};
  (* Return the current working directory. *)

(*--------------------------------------------------------------------------*)
PROCEDURE SetWorkingDir(dir : Pathname.T) RAISES {Error};
  (* Set the current working directory. *)

END DirStack.
