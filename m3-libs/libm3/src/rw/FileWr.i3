(* Copyright (C) 1989, 1992, Digital Equipment Corporation     *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Last modified on Wed Dec 15 15:06:14 PST 1993 by mcjones    *)
(*      modified on Mon Feb 24 11:32:41 PST 1992 by muller     *)
(*      modified on Sat Aug  3 00:45:49 1991 by kalsow         *)

(* A "FileWr.T", or file writer, is a writer on a "File.T".
   \index{buffered file I/O}
   \index{file!buffered I/O}
*)
     
INTERFACE FileWr;

IMPORT Wr, File, OSError, Pathname;

TYPE
  T <: Public;
  Public = Wr.T OBJECT METHODS
    init(h: File.T; buffered: BOOLEAN := TRUE): T
      RAISES {OSError.E}
  END;
(* If "w" is a file writer and "h" is a file handle, the call
   "w.init(h)" initializes "w" so that characters output to "w" are
   written to "h" and so that closing "w" closes "h". *)

(* If "h" is a regular file handle and "b" is a Boolean, "w.init(h, b)"
   causes "w" to be a buffered seekable writer and initializes "cur(w)"
   to "cur(h)".

   For any other file handle "h", "w.init(h, b)" causes "w" to be
   a nonseekable writer, buffered if and only if "b" is "TRUE", and
   initializes "cur(w)" to zero.

   If a subsequent writer operation on "w" raises "Wr.Failure", the
   associated exception argument is the "AtomList.T" argument
   accompanying an "OSError.E" exception from a file operation on "h".
   *)

PROCEDURE Open(p: Pathname.T): T RAISES {OSError.E};
(* Return a file writer whose target is the file named "p".  If the
   file does not exist, it is created.  If the file exists, it is
   truncated to a size of zero. *)

(* The call "Open(p)" is equivalent to the following:

| RETURN NEW(T).init(FS.OpenFile(p))

*)

PROCEDURE OpenAppend(p: Pathname.T): T RAISES {OSError.E};
(* Return a file writer whose target is the file named "p".  If the
   file does not exist, it is created.  If the file exists, the writer is
   positioned to append to the existing contents of the file. *)

(* The call "OpenAppend(p)" is equivalent to the following:

| WITH h = FS.OpenFile(p, truncate := FALSE) DO
|   EVAL h.seek(RegularFile.Origin.End, 0);
|   RETURN NEW(T).init(h)
| END

*)

END FileWr.
