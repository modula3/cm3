(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Last modified on Wed Feb  1 08:10:00 PST 1995 by kalsow     *) 
(*      modified on Tue Nov  9 09:51:28 PST 1993 by mcjones    *) 
(*      modified on Thu Jan 28 13:15:55 PST 1993 by mjordan    *) 
(*      modified on Tue Feb 12 03:22:32 1991 by muller         *)

(* "Stdio" provides streams for standard input, standard output, and
   standard error. These streams correspond to file handles returned
   by the "GetStandardFileHandles" procedure in the "Process" interface.
   \index{standard I/O!streams}
*)

INTERFACE Stdio;

IMPORT Rd, Wr;

VAR
  stdin: Rd.T;
  stdout: Wr.T;
  stderr: Wr.T;
  bufferedStderr: Wr.T;

END Stdio.

(* The initialization of these streams depends on the underlying
   operating system.

   If the standard error stream is directed to a terminal, it will be
   unbuffered, so that explicit "Wr.Flush" calls are unnecessary for
   interactive programs.  A buffered version of the standard error
   stream is also provided, but programs should not use both "stderr"
   and "bufferedStderr".

   If the streams are directed to or from random-access files, they
   will be seekable.

   It is possible that "stderr" is equal to "stdout".  Therefore,
   programs that perform seek operations on "stdout" should take care
   not to destroy output data when writing error messages. *)

