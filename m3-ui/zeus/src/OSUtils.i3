(* Copyright 1989 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* OSUtils.def                                                 *)
(* Last modified on Wed Jul 15 19:13:13 PDT 1992 by johnh      *)
(*      modified on Thu Apr 30 10:34:12 PDT 1992 by birrell    *)

(* Stolen from Postcard. *)
(* Miscellanous OS operations for Postcard.  The implementation is likely to
   be system-specific. *)

INTERFACE OSUtils;

(* *)
(* File system operations *)
(* *)

EXCEPTION FileError(TEXT);


PROCEDURE Delete(path: TEXT) RAISES { FileError };
  (* Deletes a file (but not a directory).  File system errors are reported
     by raising "FileError" with a human-sensible description of the error. *)

PROCEDURE MakeDir(path: TEXT) RAISES { FileError };
  (* Creates the directory.  File system errors are reported by raising
     "FileError" with a human-sensible description of the error. *)
 
END OSUtils.
