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
(* *)
(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)


INTERFACE Command;

IMPORT Rd, Wr;

(* (Very) simple interactive command line interpreter. The user writes commands
which are bound to text names. The user then calls "Interact" which fires up
the CLI. The CLI prompts to "Stdio.stdout" and waits for input. If an input
line starts with a name the user has bound the users procedure is called back.
e.g:
   Command.Bind("foo", Foo);
   Interact();
Interact will now put out a prompt; suppose somebody then types in "foo pig":
--> foo pig
Interact will then call "Foo". "Foo" can get at the other arguments on the line
(i.e. "pig" in this case) by using calls provided in this interface. When "Foo"
returns "Interact" will put out another prompt and wait for more input.
  There are a few built in commands e.g. quit and a simple help facility
Short forms of commands are supported, as is a user-specified prompt.
*)


PROCEDURE Bind(
    name: TEXT;
    proc: PROCEDURE() RAISES{};
    help: TEXT := NIL)
    RAISES {};
(* Bind "proc" to the given name. "help" is a help message that will be printed
out if the built in help command is executed; it should be a short text and
should NOT be terminated by a newline. If "help" is NIL no help information
will be printed for the command. Capitalisation in "name" is used to generate
the short form.
*)

TYPE
  Closure = OBJECT METHODS apply() RAISES {} END;

PROCEDURE BindClosure(name: TEXT; c: Closure; help: TEXT := NIL) RAISES {};
(* More general version of "Bind"; instead of a simple callback the "apply"
method of the given object is called. As any subtype of "Closure" can be passed
in this allows "apply" to have some local state *)


PROCEDURE SetPrompt(p: TEXT) RAISES {};
(* Set the prompt to p & "> " *)

PROCEDURE Interact(s: Rd.T := NIL) RAISES {Rd.Failure, Wr.Failure};
(* Outputs a prompt to "Stdio.stdout" and then waits for input on
"Stdio.stdin".  Reads up to the next newline. If the first word of
input is matches the name of a bound procedure or closure the
corresponding procedure or closure is called. When the callback
returns another prompt is given. If the input is null or contains only
whitespace another prompt is given. Otherwise a brief message is
written to "Stdio.stdout", explaining how to get help, and another
prompt is given. If "s # NIL" this stream is read before "Stdio.stdin"
*)


(* The following are procedures which can be used within callback
procedures to get at arguments given on the "command line" i.e. the
line of input given in response to the prompt *)

PROCEDURE Argument(VAR arg: TEXT): BOOLEAN RAISES {}; 
(* Get the next argument as a text; returns FALSE if there is no
argument.  Arguments are normally delimited by whitespace but they can
be quoted e.g.  "blah blah" is a single argument. There is no way of
"ungetting" an argument *)

PROCEDURE GetArg(VAR a: TEXT): BOOLEAN RAISES {};
(* Just like "Argument" but complains to "Stdio.stdout" if it fails *)

PROCEDURE CardinalArgument(VAR card: CARDINAL): BOOLEAN RAISES {};
(* Uses "Argument" and then converts the result to a cardinal; returns FALSE
if either "Argument" or the conversion fails *)

PROCEDURE CardGetArg(VAR card: CARDINAL): BOOLEAN RAISES {};
(* Just like "CardinalArgument" but complains to "stdout" if it fails *)

PROCEDURE IntegerArgument(VAR integer: INTEGER): BOOLEAN RAISES {};
(* Uses "Argument" and then converts the result to an integer; returns FALSE
if either "Argument" or the conversion fails *)

PROCEDURE IntGetArg(VAR int: INTEGER): BOOLEAN RAISES {};
(* Just like "IntegerArgument" but complains to "stdout" if it fails *)

PROCEDURE RestOfLine(): TEXT RAISES {};
(* Gets the rest of the input line, excluding the newline *)


(* Convenience functions; just call the corresponding "Wr" routines with
"Stdio.stdout" as the output stream *)

PROCEDURE Put(t: TEXT) RAISES {};
PROCEDURE PutF(fmt: TEXT; t1, t2, t3, t4, t5: TEXT := NIL) RAISES {};
PROCEDURE PutFN(fmt: TEXT; READONLY array: ARRAY OF TEXT) RAISES {};

END Command.
