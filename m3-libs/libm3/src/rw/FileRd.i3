(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Last modified on Wed Dec 15 15:06:26 PST 1993 by mcjones    *)
(*      modified on Sat Aug  3 00:54:38 1991 by kalsow         *)
(*      modified on Fri Aug 17 01:56:52 1990 by muller         *)

(* A "FileRd.T", or file reader, is a reader on a "File.T".
   \index{buffered file I/O}
   \index{file!buffered I/O}
*)

INTERFACE FileRd;

IMPORT Rd, File, OSError, Pathname;

TYPE
  T <: Public;
  Public = Rd.T OBJECT METHODS
    init(h: File.T): T RAISES {OSError.E}
  END;
(* If "r" is a file reader and "h" is a file handle, the call
   "r.init(h)" initializes "r" so that reading "r" reads characters
   from "h", and so that closing "r" closes "h". *)

(* If "h" is a regular file handle, "r.init(h)" causes "r" to be a
   nonintermittent, seekable reader and initializes "cur(r)" to
   "cur(h)".

   For any other file handle "h", "r.init(h)" causes "r" to be an
   intermittent, nonseekable reader and initializes "cur(r)" to zero. 

   If a subsequent reader operation on "r" raises "Rd.Failure", the
   associated exception argument is the "AtomList.T" argument
   accompanying an "OSError.E" exception from a file operation on "h".
   *)

PROCEDURE Open(p: Pathname.T): T RAISES {OSError.E};
(* Return a file reader whose source is the file named "p".  If the file
   does not exist, "OSError.E" is raised with an implementation-defined
   code. *)

(* The call "Open(p)" is equivalent to

| RETURN NEW(T).init(FS.OpenFileReadonly(p))

*)

END FileRd.
