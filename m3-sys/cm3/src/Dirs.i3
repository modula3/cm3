(* Copyright 1996 Critical Mass, Inc. All rights reserved.    *)

INTERFACE Dirs;

VAR (*READONLY*) (* full paths to respective directories *)
  initial : TEXT := NIL;
  derived : TEXT := NIL;
  source  : TEXT := NIL;
  package : TEXT := NIL;

VAR (*READONLY*) (* relative paths from derived directory *)
  to_initial : TEXT := NIL;
  to_source  : TEXT := NIL;
  to_package : TEXT := NIL;

PROCEDURE SetUp (target: TEXT);
(* Move to the specified target directory and set the
   global variables to reflect the move. *)

PROCEDURE CleanUp ();
(* If "Setup" created the derived directory and it's still
   empty, remove it. *)

PROCEDURE MkDir (dir: TEXT);
(* Create directory "dir". *)

END Dirs.

