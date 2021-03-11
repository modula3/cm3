(*                                                                           *)
(*  Debug.i3                                                                 *)
(*                                                                           *)
(*  Debugging output and aborting the program.                               *)
(*                                                                           *)
(*  Copyright (c) 2000 California Institute of Technology                    *)
(*  All rights reserved.                                                     *)
(*  Department of Computer Science                                           *)
(*  Pasadena, CA 91125.                                                      *)
(*                                                                           *)
(*  Author: Mika Nystrom <mika@cs.caltech.edu>                               *)
(*                                                                           *)
(*  Permission to use, copy, modify, and distribute this software            *)
(*  and its documentation for any purpose and without fee is hereby          *)
(*  granted, provided that the above copyright notice appear in all          *)
(*  copies. The California Institute of Technology makes no representations  *)
(*  about the suitability of this software for any purpose. It is            *)
(*  provided "as is" without express or implied warranty. Export of this     *)
(*  software outside of the United States of America may require an          *)
(*  export license.                                                          *)
(*                                                                           *)
(* $Id$ *)

INTERFACE Debug;

(* Debugging.

   Main code entry points:  Debug.Out      --  normal debugging, with level
                            Debug.Warning  --  warnings
                            Debug.Error    --  program termination is optional

   Controlled by
 
     DEBUGLEVEL env. variable
     DEBUG<this>, NODEBUG<this> env. variables
     DEBUGFILTER env. variable
   
     @M3debugtrace=<filename>[,<filename>...]  RT arg
     @M3debugappend=<filename>[,<filename>...]  RT arg
*)

IMPORT Fmt, Wr, Pathname;
IMPORT OSError;

CONST DefaultLevel = 10;

PROCEDURE Out(t : TEXT; 
              minLevel : CARDINAL := DefaultLevel; 
              (* no print at lower levels*)

              cr:=TRUE;                  (* carriage return yes/no *)
              this : TEXT := NIL         (* debug if "DebugThis for this" *)
  );

PROCEDURE OutFilePos(file : Pathname.T; pos : CARDINAL; t : TEXT; minLevel : CARDINAL := 10; cr:=TRUE);
  (* used by m3texthack generated code *)

PROCEDURE ToHex(t : TEXT) : TEXT;
  (* routine to convert t to printable characters *)
PROCEDURE HexOut(t : TEXT; minLevel : CARDINAL := 10; cr:=TRUE; toHex:=ToHex);
(* produce output in ASCII using ToHex or another readability routine *)

PROCEDURE S(t: TEXT; minLevel : CARDINAL := 5; cr:=TRUE);
PROCEDURE Warning(t : TEXT);
PROCEDURE Error(t : TEXT; exit := TRUE);

PROCEDURE Check(b : BOOLEAN; msg : TEXT; exit := TRUE);
  (* if b is TRUE, do nothing
     if b is FALSE, same as Error(msg,exit) *)

PROCEDURE UnNil(text : TEXT) : TEXT;

(* apart from these procedures, the debug level is also set from the
   env. variable DEBUGLEVEL *)

(* if nothing else, it defaults to zero *)

PROCEDURE RaiseLevel(newLevel : CARDINAL);
PROCEDURE LowerLevel(newLevel : CARDINAL);
PROCEDURE SetLevel(newLevel : CARDINAL);
PROCEDURE GetLevel() : CARDINAL;

(* output hook: support for raw terminals, etc. *)
TYPE
  OutHook = PROCEDURE(t: TEXT);
PROCEDURE RegisterHook(out: OutHook; level:=0);
PROCEDURE RegisterErrorHook(err: OutHook);

PROCEDURE AddStream(newStream : Wr.T);
  (* another mechanism for changing output: AddStream.
     This will change only the default output method; if you override it
     using output hook above, this will be ignored *)
  
PROCEDURE RemStream(deadStream : Wr.T);
  (* no-op if not in current set of streams *)

PROCEDURE FmtAddress(p : ADDRESS; base : Fmt.Base := 16) : TEXT;
PROCEDURE FmtPointer(p : REFANY; base : Fmt.Base := 16) : TEXT;

(* should we debug "this"?  if the environment variable "DEBUG" & this
   is defined, then yes; else no.

   Also: if DEBUGEVERYTHING is defined and NODEBUG & this is not defined,
   then yes

   Also: if DEBUG & this is defined but DEBUGNOTHING is defined, then no
   (unless DEBUGEVERYTHING and not NODEBUG & this)...

*)
PROCEDURE DebugThis(this : TEXT; default := FALSE) : BOOLEAN;

TYPE Options = { PrintPID, PrintThreadID, PrintTime };

PROCEDURE SetOptions(options : SET OF Options);
PROCEDURE GetOptions() : SET OF Options;

(* override environment variables programmatically *)

PROCEDURE SetEnv(var : TEXT);

PROCEDURE ClearEnv(var : TEXT);

PROCEDURE HaveEnv(var : TEXT) : BOOLEAN;
  (* checks overridden value first, and if no override, checks
     actual system environment. *)

PROCEDURE SetDebugTimeZone(tz : TEXT) RAISES { OSError.E };

PROCEDURE SetTimedDebugFlush(wr : Wr.T);
  (* flush this stream on a timer rather than automatically on every write *)

END Debug.
