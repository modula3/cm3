(* Copyright (C) 1995, Digital Equipment Corporation        *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(*                                                          *)
(* Last modified on Thu Feb  9 08:34:18 PST 1995 by kalsow  *)

INTERFACE Template;

IMPORT Wr, Thread, EventFile;

PROCEDURE Generate (READONLY evt: EventFile.T;  algorithm, view: TEXT;
                    template: TEXT;  wr: Wr.T)
  RAISES {Wr.Failure, Thread.Alerted};
(* Macro expand "template" using values from "evt",
   leaving the results on "wr". *)

END Template.

(*
  These are the symbols recognized by during macro expansion:

|  #{                   start repeated block of all events;
|                         must be alone at head of line
|  #{_OUTPUT            start repeated block of output events only
|  #{_UPDATE            start repeated block of update events only
|  #{_FEEDBACK          start repeated block of feedback events only
|  #}                   end repeated block; must be alone at head of line
|  #|                   introduces between-args material; must be alone
|                          at head of line and inside a per-arg block
|  #(_ALGNAME_)         algorithm name
|  #(_VIEWNAME_)        view name (for -Obliq or -Obliq3D)
|  $                    import from template follows; must fill the line
|  #(_IMPORTS_)         imports from event file & template go here
|  #(_ALGDATA_)         data fields for algorithm object
|  #(_EVENT_)           name of event
|  #(_EVENTSTYLE_)      "OutputEvent", "FeedbackEvent", or "UpdateEvent"
|  #(_EVENTPRIO_)       Integer priority, in 0..9
|  #(_ARGSTR_)          unprocessed arg string, with no trailing semis
|  #(_NONULL_ARGSTR_)   ... w/ a dummy integer declaration if null
|  #(_SEMI_ARGSTR_)     ... w/ leading semi if non-null
|  #(_ARGSTR_SEMI_)     ... w/ trailing semi if non-null
|  #(_ARGTYPES_)        types of args, no names
|  #(_COMMA_ARGTYPES_)  ... w/ leading comma
|  #(_SPACED_ARGTYPES_) types of args, with spaces, not commas, between
|  #(_ARGNAMES_)        names of args, no types, as for a call
|  #(_COMMA_ARGNAMES_)  ... w/ leading comma
|  #(_ARGMODE_)         single argument mode (VALUE=>"")
|  #(_ARGNAME_)         single argument name
|  #(_ARGTYPE_)         single argument type
|  #(_ARGFMT_)          a function that gives a printable text when
|                          applied to the argument

*)
