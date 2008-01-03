(* Copyright 1996-2000 Critical Mass, Inc. All rights reserved.    *)
(* See file COPYRIGHT-CMASS for details. *)

INTERFACE MxConfig;

CONST Filename = "cm3.cfg";

PROCEDURE FindFile (): TEXT;
(* Returns a path to the current configuration file.  If no
   configuration file is found, "NIL" is returned. *)

PROCEDURE Get (param: TEXT): TEXT;
(* Returns the defined value of "param" in current configuration file.
   If no configuration file is found, "param" is not defined, or it
   cannot be converted to a text value, "NIL" is returned. *)
   
END MxConfig.

(* The configuration file is located by finding the first
   readable instance of "Filename" in the following places:
   \begin{enumerate}
   \item the current directory (".")
   \item the immediate source directory ("./src")
   \item a sibling source directory ("../src")
   \item the directory specified by the M3CONFIG environment variable.
   \item the directory containing the current executable (if $argv[0]$
         contains any path elements)
   \item the directories named by the PATH environment variable.
   \end{enumerate}
*)

