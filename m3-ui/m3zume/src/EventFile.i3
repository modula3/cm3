(* Copyright (C) 1995, Digital Equipment Corporation        *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(*                                                          *)
(* Last modified on Tue Feb  7 13:41:15 PST 1995 by kalsow  *)

INTERFACE EventFile;

IMPORT Rd;

TYPE
  T = RECORD
    imports  : Import;
    alg_data : TEXT;
    events   : Event;
  END;

TYPE
  Import = REF RECORD
    next      : Import;
    interface : TEXT;
  END;

TYPE
  EventKind = { Output, Update, Feedback };

CONST
  EventKindName = ARRAY EventKind OF TEXT {
    "OutputEvent", "UpdateEvent", "FeedbackEvent"
  };

TYPE
  Event = REF RECORD
    next     : Event;
    priority : TEXT;
    name     : TEXT;
    args     : EventArg;
    kind     : EventKind;
  END;

TYPE
  ArgMode = { Value, Readonly };

TYPE
  EventArg = REF RECORD
    next    : EventArg;
    mode    : ArgMode;
    name    : TEXT;
    type    : TEXT;
    printer : TEXT;
  END;
  
PROCEDURE Parse (rd: Rd.T;  VAR(*OUT*) t: T): TEXT;
(* Parse the event file contained in "rd" and leave it's AST
   in "t".  If everything went well, return "NIL".  Otherwise,
   return a message describing the problem. *)

END EventFile.

(*
The syntax of event files is:

   event_file  ::= { import } [ algdata ] { event }
   import      ::= "IMPORT" [ name_list ] ";"
   algdata     ::= "ALGDATA" ...
   event       ::= event_type [ priority ] name [ event_args ] ";"
   event_type  ::= "OUTPUT" | "PROCEDURE" | "UPDATE" | "FEEDBACK"
   priority    ::= "[" digit "]"
   event_args  ::= "(" [ arg { ";" arg } ] ")"
   arg         ::= [ arg_mode ] name_list ":" arg_type [ arg_printer ]
   arg_mode    ::= "VALUE" | "READONLY"
   arg_type    ::= name
   arg_printer ::= "[" [ name ] "]"

   name_list   ::= name { "," name }
   name        ::= alpha { letter }
   letter      ::= alpha | digit | "." | "_"
   alpha       ::= "a" | "b" | ... | "z" | "A" | "B" | .. | "Z"
   digit       ::= "0" | "1" | ... | "9"

Event files use Modula-3 comment conventions. (ie. nesting "(*..*)"s)
*)
