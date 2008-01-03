(* Copyright (C) 1992, Digital Equipment Corporation. *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* Last modified on Wed Dec 15 15:10:34 PST 1993 by mcjones *)

(* A process is the execution of a program by one or more threads within
   an address space.  A process may hold a variety of resources such as
   file handles. *)

INTERFACE Process;

IMPORT File, OSError, Pathname;

TYPE T <: REFANY;
(* A "Process.T", or process handle, provides access to a child process. *)

PROCEDURE Create(
    cmd: Pathname.T;
    READONLY params: ARRAY OF TEXT;
    env: REF ARRAY OF TEXT := NIL;
    wd: Pathname.T := NIL;
    stdin, stdout, stderr: File.T := NIL): T
  RAISES {OSError.E};
(* Create a new process and cause it to execute the program with
   pathname "cmd", parameters "params", environment variables
   "env", working directory "wd", and standard file handles "stdin",
   "stdout", and "stderr".  Return the handle of the new process. *)

(* If "cmd" consists of a single (relative) arc name, then it is
   looked up in an operating-system dependent way (see below).
   Otherwise, "cmd" is looked up in the normal fashion as an absolute
   pathname or as a pathname relative to the current working directory
   (not "wd").

   A process can examine its own parameters via the interface
   "Params".  The parameter "params[i]" passed to "Create" will
   correspond to the value of "Params.Get(i+1)" in the newly created
   process (because "Params.Get(0)" returns the command name).  (See
   the "Params" interface for the way SRC Modula-3 treats parameters
   beginning with the characters "@M3".)
   \index{parameters of a process}
   \index{process!parameters}

   If "env" is not "NIL", it consists of a reference to an array of
   texts that must have the form "name=value".  If "env" is "NIL", it
   defaults to the environment variables of the caller's process.  A
   process can examine its own environment variables via the interface
   "Env".
   \index{environment variables}
   \index{process!environment variables}

   If "wd" is "NIL", it defaults to the working directory of the
   caller's process.
   \index{working directory}
   \index{process!working directory}

   If any of "stdin", "stdout", or "stderr" are "NIL", the
   corresponding file handle of the new process is "NIL".  A process
   can obtain its own standard file handles by calling the procedure
   "GetStandardFileHandles" defined later in this interface.
   \index{standard I/O!file handles}
   \index{process!standard I/O handles}
   \index{I/O!standard handles}

   The sharing established by passing a "File.T" to a new process
   requires care.  For example, seeks done by either process affect
   both, and passing a "Pipe.T" increments a reference count of the
   underlying channel.  See the end of this interface for an example
   of using "Create" with pipes.

   \paragraph*{POSIX.} "Create" forks a child process, which executes
   the specified command.  If "cmd" consists of a single (relative)
   arc name, "Create" searches each of the directories specified by
   the PATH environment variable for a file named "cmd" that is
   executable by the current (effective) user.  If the attempt to
   execute the command returns the Unix error ENOEXEC, then the child
   process executes "/bin/sh" with the original arguments prefixed by
   the pathname determined earlier.

   \paragraph*{Win32.} "Create" calls "Win32.CreateProcess".  If "cmd"
   consists of a single (relative) arc name, "Win32.CreateProcess"
   first appends ".EXE" if "cmd" includes neither an extension nor a
   final period, and then searches for this name in the following
   sequence of directories: the working directory; the Windows system
   directory; the Windows directory; the directories listed in the
   PATH environment variable.
*)

TYPE ExitCode = [0 .. 16_7FFFFFFF];

(* An exit code (or status) of zero normally means successful
   termination, and a non-zero value normally indicates an error, but
   the exact conventions vary between systems and programs. *)

PROCEDURE Wait(p: T): ExitCode;
(* Wait until the process with handle "p" terminates, then free the
   operating system resources associated with the process and return
   an exit code indicating the reason for its termination.  It is a
   checked runtime error to call "Wait" twice on the same process
   handle. *)

(* \paragraph*{POSIX.} The value returned by "Wait" is equal to the
   "status" result of the "wait" system call.

   \paragraph*{Win32.} The value returned by "Wait" is "c MOD
   (LAST(ExitCode) + 1)" where "c" is the value returned by
   "Win32.GetExitCodeProcess". *)

PROCEDURE Exit(n: ExitCode := 0);
(* Call the registered exitors and terminate the program with exit
   code "n".  Terminating a Modula-3 program by ``falling off the
   end'' is equivalent to calling "Exit(0)". *)

(* \index{terminating execution} *)

PROCEDURE Crash(msg: TEXT);
(* Call the registered exitors and terminate the program with the
   error message "msg".  If possible, invoke a debugger or generate a
   core dump. *)

(* Modula-3 implementations that don't convert checked runtime errors
   into exceptions should call "Crash" to abort the program.

   Some Modula-3 implementations catch external events (e.g. Unix
   signals) or internal interrupts (e.g. floating-point underflow) and
   call "Crash".  Consult your local installation guide for more
   information. *)

PROCEDURE RegisterExitor(p: PROCEDURE());
(* Register the procedure "p" to be called when "Exit" or "Crash" is
   called. *)

(* Each registered exitor is called at most once.  Exitors are called
   in reverse of the order they were registered.  A facility
   implementing a class of objects should register only a single
   exitor, which can consult a private data structure to determine
   which of its objects need cleanup. "RegisterExitor" should be
   called at module initialization time (not when the first object is
   created) to guarantee the correct registration order. *)

TYPE ID = [0 .. 16_7FFFFFFF];
CONST NullID: ID = 0;

(* An "ID" or process identifier is assigned to each process when it
   is created.  At any moment, no two processes on the same computer
   have the same identifier, but identifiers can be reused over time.
   No process is ever assigned the identifier "NullID". *)

PROCEDURE GetID(p: T): ID;
(* Return the process identifier of the process with handle "p". *)

(* \index{process!identifier} *)

PROCEDURE GetMyID(): ID;
(* Return the process identifier of the caller's process. *)

PROCEDURE GetStandardFileHandles(
    VAR (*OUT*) stdin, stdout, stderr: File.T);
(* Return the standard input/output handles that were supplied when
   this process was created. *)

(* \index{standard I/O!file handles}
   \index{process!standard I/O handles}
   \index{I/O!standard handles}
*)


PROCEDURE GetWorkingDirectory(): Pathname.T
  RAISES {OSError.E};
(* Return an absolute pathname for the working directory of the
   caller's process. *)

(* \index{working directory}
   \index{process!working directory}
*)

PROCEDURE SetWorkingDirectory(path: Pathname.T)
  RAISES {OSError.E};
(* Change the working directory of this process to "path". *)

END Process.

(* \paragraph*{Example.} A typical use of "Create" is to run a filter
   process that reads from standard input and writes a transformed
   version to standard output.  The first step is to create two sets
   of pipes to carry the standard input and standard output of the new
   process.  (If desired, standard error can be handled in the same
   way as standard output.)

| VAR hrChild, hwChild, hrSelf, hwSelf: Pipe.T;
| BEGIN
|   Pipe.Open(hr := hrChild, hw := hwSelf);
|   Pipe.Open(hr := hrSelf, hw := hwChild);

   The next step is to create the process, passing the appropriate
   pipes, and then to close the original instances of these pipes.
   (The pipes must be closed to maintain the correct reference counts
   on the underlying channels.)

|   WITH p = Process.Create(..., hrChild, hwChild, NIL) DO
|     TRY
|       TRY hrChild.close(); hwChild.close()
|       EXCEPT OSError.E => (*SKIP*)
|       END;

   Now comes the actual writing and reading, which is conveniently
   performed using I/O streams:

|       WITH wr = NEW(FileWr.T).init(hwSelf),
|            rd = NEW(FileRd.T).init(hrSelf) DO
|         `Write "wr" (and perhaps read "rd")`

   Closing "wr" causes the filter to encounter end-of-file on its
   standard input, which should cause it to flush its standard output
   and terminate.  This in turn causes this process to read
   end-of-file.

|         TRY Wr.Close(wr)
|         EXCEPT Wr.Failure, Thread.Alerted => (*SKIP*)
|         END;
|         `Read "rd" to end-of-file`;
|         TRY Rd.Close(rd)
|         EXCEPT Rd.Failure, Thread.Alerted => (*SKIP*)
|         END
|       END

   The last step is to clean up the process.

|     FINALLY EVAL Process.Wait(p)
|     END
|   END
| END

*)
