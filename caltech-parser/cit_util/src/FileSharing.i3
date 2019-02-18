(* $Id$ *)

INTERFACE FileSharing;

VAR SimultaneousReadersAndWritersAreOK : BOOLEAN; (* CONST *)
(* this "VAR" is FALSE for Windows and TRUE for Unix *)

END FileSharing.
  
