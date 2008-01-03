(* Copyright 1989 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* OSUtils.i3                                                  *)
(* Last modified on Mon May 20 12:17:43 PDT 1996 by mcjones    *)
(*      modified on Mon Jun  6 17:16:32 PDT 1994 by birrell    *)

(* Miscellanous OS operations for BuildLectern. *)

INTERFACE OSUtils;


IMPORT File, FileRd, OSError, Pipe, Rd;

(* *)
(* DupRd *)
(* *)

TYPE DupRd <: FileRd.T;
  (* Just like a FileRd.T, except that the .close operation
     doesn't close the underlying file. *)


(* *)
(* Fifo *)
(* *)

PROCEDURE CreateFifo(p: TEXT) RAISES {OSError.E};
  (* Create a fifo named by "p".  This fifo may be opened (by the name
     "p") for writing by a single client.  The server must call
     OpenFifo to obtain a file handle for reading.  When the client is
     finished writing, the server must close the file handle and then
     call DeleteFifo.  This interface is not reentrant; only a single
     CreateFifo/OpenFifo/DeleteFifo transaction may be in progress within
     the current process at any one time. *)

PROCEDURE OpenFifo(): File.T RAISES {OSError.E};
  (* Return an open file handle for the fifo created by the latest
     call of CreateFifo, for use by a server process to read from.
     When server is finished, the file handle must be closed and
     "DeleteFifo" must be called. *)

PROCEDURE DeleteFifo() RAISES {OSError.E};
  (* Delete the fifo created by the latest call on CreateFifo; a
     open file handle on this fifo should be been closed first. *)

  (* Posix: CreateFifo calls mknode, OpenFifo calls
     FS.OpenFileReadonly, and DeleteFifo calls FS.DeleteFile.

     Win32: CreateFile calls CreateNamedPipe, OpenFifo calls
     ConnectNamedPipe, and DeleteFifo is a no-op. *)


(* *)
(* Sub-processes *)
(* *)

PROCEDURE RunFilter(READONLY argv: ARRAY OF TEXT; stdin: File.T;
                    VAR stdoutR: Pipe.T): Rd.T
                    RAISES { OSError.E };
  (* Run the command described by argv.  argv[0] is command name, rest
     is parameters.  The child's stdin is the given file, and its
     stdout and stderr are separate pipes.  The read side of the child's
     stdout pipe is assigned to "stdoutR".  Raises Error if pipe creation fails
     or if process creation is known to fail.  The result is a reader on
     "stdoutR".  Closing the reader closes the child's stdout and stderr
     pipes, and waits for the child to terminate. *)

END OSUtils.
