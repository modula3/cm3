(***************************************************************************)
(*                      Copyright (C) Olivetti 1989                        *)
(*                          All Rights reserved                            *)
(*                                                                         *)
(* Use and copy of this software and preparation of derivative works based *)
(* upon this software are permitted to any person, provided this same      *)
(* copyright notice and the following Olivetti warranty disclaimer are     *) 
(* included in any copy of the software or any modification thereof or     *)
(* derivative work therefrom made by any person.                           *)
(*                                                                         *)
(* This software is made available AS IS and Olivetti disclaims all        *)
(* warranties with respect to this software, whether expressed or implied  *)
(* under any law, including all implied warranties of merchantibility and  *)
(* fitness for any purpose. In no event shall Olivetti be liable for any   *)
(* damages whatsoever resulting from loss of use, data or profits or       *)
(* otherwise arising out of or in connection with the use or performance   *)
(* of this software.                                                       *)
(***************************************************************************)
(**)
(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE Err;

(* This interface provides a standard mechanism for simple error reporting. The
error messages are written to an error stream. This error stream is a vague
concept and may be implemented different ways on different systems (some window
systems might have a standard error window for example).  The implementation is
not important providing the programmer can rely on the fact that any messages
written using "Err" get through to the user if it is at all possible.

Error messages are classified into five severities and a continuation:
 
  Continue   a continuation of the previous message.

  Comment    not an error at all; just a message (e.g. program
             identification, copyright notice, etc.).

  Warning    the program has performed in a manner, or detected something,
             which may cause surprise to its user, but has otherwise
             executed correctly (e.g. a message from a compiler flagging
             an unused variable).

  Error      the program has detected a definite error and may have had to
             take drastic recovery action in order to proceed (e.g. a
             gross syntax error detected by a compiler, leading to no
             code being generated). 

  Fatal      a fatal but foreseen error which brings the program to a
             dead stop (e.g. failing to find a vital file or running
             out of workspace).

  Disaster   a fatal but foreseen error which brings the program to a
             dead halt and for which the most appropriate action is to
             enter the debugger, generate a postmortem dump, etc.
             (e.g. the program detects an internal inconsistency
             in its data structures or fails an assertion test).

After an error of severity "Fatal" the program is terminated by
executing "RTShutdown.Exit"; after an error of severity "Disaster",
the program terminates by raising an uncatchable exception. This
causes a postmortem dump, entry to the debugger, or other
system-specific debugging action.

Messages are constructed by taking the user supplied message and
prepending the program name and the severity of the error. The program
name is omitted for continuations and the severity is omitted for
continuations and comments. The user can specify whether a newline
should be appended to the message.

The program name can be set explicitly, but a default value is
otherwise used, acquired by calling "Params.Get(0)". *)


TYPE
  Severity = {Continue, Comment, Warning, Error, Fatal, Disaster};

PROCEDURE Print(msg: TEXT; severity: Severity := Severity.Fatal; 
                newline: BOOLEAN := TRUE); 
(* Constructs and prints an error message.  "severity" is explained
above. "msg" is the user supplied error message. "newline" indicates
whether a newline should be appended before printing. *)

PROCEDURE SetProgramName(n: TEXT): TEXT;
(* Sets the program name used in error messages to "n". The previous
   value is returned as result. *)

END Err.
