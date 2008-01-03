(* Copyright (C) 1989, 1990, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)

(* Last modified on Thu Oct 21 17:05:11 PDT 1993 by sfreeman *)

(* common type for client interface to local J-Video server.  The video and
   audio types are subclassed from it. *)

INTERFACE Jv;

IMPORT Atom, OSError, Thread;

TYPE
  T <: Public;
  Public =
    MUTEX OBJECT
    METHODS
      (* LL > self *)
      init (pipeName: TEXT): T RAISES {OSError.E};
      (* establish connection with local server.  "pipeName" is the name of
         the named unix socket which the client should connect to *)
      close ();
      (* close connection to local server, ignoring all exceptions *)
    END;

VAR ServerFailure: Atom.T;
(* may be part of OSError.E list *)

(* LL >= t *)
PROCEDURE Send (t: T; buf: ADDRESS; nbytes: CARDINAL)
  RAISES {OSError.E, Thread.Alerted};

PROCEDURE Recv (t: T; buf: ADDRESS; nbytes: CARDINAL)
  RAISES {OSError.E, Thread.Alerted};

END Jv.
