(* Copyright 1999-2002 elego Software Solutions GmbH, Berlin, Germany.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *)
(*---------------------------------------------------------------------------*)
(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*---------------------------------------------------------------------------*)

INTERFACE System;

IMPORT Thread, AtomList, TextSeq, Rd, Wr, File, Process;
IMPORT ProcessEnv, MsgIF;

(*---------------------------------------------------------------------------*)
EXCEPTION
  ExecuteError(TEXT);
  Error(TEXT);

(*---------------------------------------------------------------------------*)
PROCEDURE AtomListToText(l : AtomList.T) : TEXT;
  (* Convert an AtomList.T to a text, inserting spaces between all
     elements. *)

(*---------------------------------------------------------------------------*)
PROCEDURE ParListToText(params : TextSeq.T) : TEXT;
  (* Convert a parameter list to text. *)

(*---------------------------------------------------------------------------*)
PROCEDURE ExecWithFileHandles(pgm : TEXT; params : TextSeq.T;
                              stdin, stdout, stderr : File.T;
                              env : ProcessEnv.T := NIL;
                              msgif : MsgIF.T := NIL;
                              wd : TEXT := NIL;
                              ) : INTEGER
  RAISES {ExecuteError};
  (* Execute `pgm' with `params' and the given standard file handles.
     If any of the handles is NIL, the handle of the current process
     is used.
     IF `wd' is non-NIL, it is used as the working directory of the
     process.
  *)

(*---------------------------------------------------------------------------*)
PROCEDURE Exec(pgm : TEXT; params : TextSeq.T; env : ProcessEnv.T := NIL;
               msgif : MsgIF.T := NIL; wd : TEXT := NIL;
               pstdin  : File.T := NIL;
               pstdout : File.T := NIL;
               pstderr : File.T := NIL) : INTEGER
  RAISES {ExecuteError};
  (* Execute `pgm' with `params' as parameters. `Params' may contain
     simple input and output redirections. If param[i] is any of
     `>', `1>', `<', `2>', `&>', `>>', `1>>', `2>>', `&>>', then param[i+1]
     is treated as the corresponding filename that is to be used as
     source or sink of bytes. The redirections have the following meaning:
|
|     < fn   : read stdin from file fn
|     > fn   : write stdout into file fn
|     1> fn  : write stdout into file fn
|     2> fn  : write stderr into file fn
|     &> fn  : write stdout and stderr into file fn
|     >> fn  : append stdout to file fn
|     1>> fn : append stdout to file fn
|     2>> fn : append stderr to file fn
|     &>> fn : append stdout and stderr to file fn
|
     IF `wd' is non-NIL, it is used as the working directory of the
     process.
  *)

(*---------------------------------------------------------------------------*)
PROCEDURE Execute(cmd : TEXT; env : ProcessEnv.T := NIL;
                  msgif : MsgIF.T := NIL; wd : TEXT := NIL) : INTEGER
  RAISES {ExecuteError, Thread.Alerted};
  (* Split `cmd' into program name and parameters and try to execute it
     directly. No input and output redirections are performed.
     IF `wd' is non-NIL, it is used as the working directory of the
     process.
  *)

(*---------------------------------------------------------------------------*)
PROCEDURE RdExecute(cmd : TEXT; VAR rd : Rd.T; wd : TEXT := NIL;
                    env : ProcessEnv.T := NIL;
                    msgif : MsgIF.T := NIL) : Process.T
   RAISES {ExecuteError, Thread.Alerted};
  (* Split `cmd' into program name and parameters and try to execute it
     directly. Via `rd' the stdout and stderr streams of the created
     program-process (the return result) can be read.
     `wd' is the working directory of the program.

     Command sequentialization (as in 'ExecuteList') is not supported
     because of the asynchronous execution of the user processes.
     (Problems: SystemExecute exception handling and transparent merging
     of serveral pipes (for each user process) to the rd parameter)
  *)

(*---------------------------------------------------------------------------*)
PROCEDURE PipeTo(cmd : TEXT; VAR wr : Wr.T; wd : TEXT := NIL;
                 env : ProcessEnv.T := NIL; msgif : MsgIF.T := NIL) : Process.T
  RAISES {ExecuteError, Thread.Alerted};
  (* Split `cmd' into program name and parameters and try to execute it
     directly. Create a writer `wr' that is connected to the stdin of the
     created process so we can pipe input data into it. *)

(*---------------------------------------------------------------------------*)
PROCEDURE Filter(cmd : TEXT; VAR rd : Rd.T; VAR wr : Wr.T; wd : TEXT := NIL;
                 env : ProcessEnv.T := NIL; msgif : MsgIF.T := NIL) : Process.T
  RAISES {ExecuteError, Thread.Alerted};
  (* Split `cmd' into program name and parameters and try to execute it
     directly. Create a writer `wr' that is connected to the stdin of the
     created process so we can pipe input data into it, and a reader
     `rd' that gathers the stdout of the process. *)

(*---------------------------------------------------------------------------*)
PROCEDURE ExecuteShell(cmd : TEXT; shell := "/bin/sh";
                       env : ProcessEnv.T := NIL;
                       msgif : MsgIF.T := NIL;
                       wd : TEXT := NIL) : INTEGER
  RAISES {ExecuteError};
  (* Try to execute the given `cmd' via `shell', that is, call
     `shell -c cmd'.
     IF `wd' is non-NIL, it is used as the working directory of the
     process.
  *)

(*---------------------------------------------------------------------------*)
PROCEDURE ExecuteList(cmd : TEXT; env : ProcessEnv.T := NIL;
                      msgif : MsgIF.T := NIL; wd : TEXT := NIL) : INTEGER
  RAISES {ExecuteError, Thread.Alerted};
  (* Parse `cmd', split it into single commands at every `;', `|', `&&', and
     `||', and execute every command via `Exec'. The concatenation
     characters have the usual Bourne Shell meaning.
     Token may be grouped by single or double quotes.

     Since Exec() is called internally, all input and output redirections
     that are described above will be performed.

     IF `wd' is non-NIL, it is used as the working directory of the
     process.
  *)

(*---------------------------------------------------------------------------*)
PROCEDURE Hostname() : TEXT;
  (* return the name of the local computer *)

PROCEDURE Wait(p: Process.T) : Process.ExitCode RAISES {Error};
  (* Like Process.Wait, but the POSIX-Implementation is overridden to RAISE on
     instead of asserting false. WARNING: It's an unchecked runtime-error to call
     `Wait` or Process.Wait for a pid that's already waited for by
     `Wait` (on POSIX).

     On WIN32 `Wait` is just a wrapper for Process.Wait. *)

(*---------------------------------------------------------------------------*)

END System.
