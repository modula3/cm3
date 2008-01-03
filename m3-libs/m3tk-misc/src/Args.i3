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

INTERFACE Args;

(* The following describes the argument decoding provided by this interface.

  Arguments are decoded according to a template. A template is an ordered
sequence of keys, specified by the user. Argument decoding binds argument
values to keys. After decoding the user can discover the values bound to keys
via enquiry functions.

  A template is created by handing a template text to the procedure
'NewTemplate'. A template text contains a number of key descriptions, separated
by spaces. Here are some example key descriptions:

1)  "duo=d/2"
2)  "trio=t/03/p"
3)  "needed=necessary=n/4/r/p"
4)  "zero=z/0"
5)  "normal"
6)  "include=i/l/x"

  The first section of a description, everything preceding the '/' character,
is a list of alternative names for the key. These names are separated by the
'=' character. So example 1 has two alternative names: "duo" and "d". Names
are alphanumeric and must start with a letter.
  The alternative names are optionally followed by three items separated by the
'/'. These are:

A) The quota. Argument decoding will bind values to keys. These values can be
thought of as arrays of texts. The quota specifies the length of the array
which will be bound to the key.
  If the quota is a number starting with 0 the quota is an upper bound on the
length of the array;  otherwise the quota gives the exact length. There are
three special cases:
a) an omitted quota (as in example 5) is equivalent to the quota "/1" i.e. a
quota of exactly one argument.
b) the quota string "/f" or "/flag" is equivalent to "/0"
c) the quota string "/l" or "/list" is equivalent to "/0n" where n is the
largest possible cardinal. This results in a key to which can be bound a value
array of any length.
  Note that a quota of zero is a reasonable thing to have. Because values are
arrays there is a difference between a key being left unbound and a key being
bound to an array of length 0.

B) The "required" flag. If a key description contains "/r" or "/required" it
indicates that a successful argument decode must bind a value to that key. For
example suppose a template contained the keys described in examples 1 and 3
i.e. "duo" and "needed". To successfully decode arguments according to that
template a value array of length 4 MUST be bound to the key called "needed".
This contrasts with "duo"; after a successful decoding an array of length 2
may be bound to "duo" OR "duo" could be left unbound.

C) The "positional" flag. If a key description contains "/p" or "/positional"
it indicates that a value can be bound to the key positionally or by keyword.
Keys without the "positional" flag can only be bound by keyword. The difference
between keyword and positional binding is explained later in this interface.

D) The "prefix" flag. If a key description contains "/x" it indicates that
all instances of keywords starting with this prefix will be treated as
values to be associated with the keyword.

  Arguments are represented by arrays of texts. The decoding process recognizes
three classes of argument texts:
a) NIL. NIL argument texts act as spacers but are otherwise ignored.
b) Keywords. An argument text which starts with the character '-' and is
followed by one letter and then a number (possibly zero) of alphanumerics is
considered to be a keyword.
c) Simple values. All non NIL argument texts which are not keywords are simple
values.

  Argument decoding takes simple argument values, puts them into arrays and
binds the arrays to keys. NIL and keyword argument texts provide information
but are not bound to keys. Binding is done in two ways:

A) Keyword binding. A keyword is just a name prefixed by '-'. If the name
matches a key name then any simple values immediately following the keyword
are bound to the named key. For example take the argument list
  {"-duo", "a", "b", "-zero", "-t", "c", "d", "e" -Idir1 -Idir2}
and a template built from the template string
  "duo=d/2 trio=t/03/p zero=z/0 normal include=i/l/x"
The result of doing keyword binding is
   Key         Binding
  duo         {"a", "b"}
  trio        {"c", "d", "e"}
  zero        {}
  normal      no binding
  include     {"dir1", "dir2"}

It is an error if an argument array contains more than one keyword which
matches a single key. Keyword matching is case insensitive.
  Note that the special treatement of keywords makes it impossible to specify
simple values which look like keywords. To avoid this problem any non NIL
argument value which starts with '--' has its initial '-' lopped off. So
"-t" is a keyword. "--t" is the simple value "-t". Both "-3" and "--3" are
equivalent simple values ("-3" does not look like a keyword because the first
character after the '-' is a digit).

B) Positional binding. Positional binding takes place after keyword binding.
As described earlier only keys with the "positional" flag are bound
positionally; these keys are ordered according to their position in the
original template text. The positional keys are taken in that order and any
values left in the argument array are bound to them until:
a) there are no more values
b) the next positional key is already bound
For example take a template text containing the example key descriptions i.e.
  "duo=d/2 trio=t/03/p needed=necessary=n/4/r/p normal zero=z/0"
and the argument list
  {"a", "b", "c", "d", "e", "f", "g"}
Positional binding would result in the following:
   Key         Binding
  duo         no binding
  trio        {"a", "b", "c"}
  needed      {"d", "e", "f", "g"}
  zero        no binding
  normal      no binding
Note that binding is done greedily; "trio" gets 3 simple values even though it
would be possible to give it less because it does not have an exact quota.

  This has been an overview of argument decoding. See the comments later in
this interface for more details. *)

(* Operations on argument arrays *)

TYPE
  T = ARRAY OF TEXT;
(* Arguments are represented by arrays of texts *)

<*INLINE*> PROCEDURE Copy(READONLY args: T): REF T RAISES {};
(* Creates a new array on the heap, copies 'args' into it and returns a
reference to the new array *)

<*INLINE*> PROCEDURE Concatenate(READONLY args1, args2: T): REF T RAISES {};
(* Creates a new array on the heap which is a concatenation of 'args1' and
'args2'. Returns a reference to the new array *)



(* Template creation *)

TYPE
  Template <: REFANY;

EXCEPTION
  BadTemplate;

PROCEDURE NewTemplate(t: TEXT): Template RAISES {BadTemplate};
(* 'NewTemplate' constructs and returns a template corresponding to the
template description text 't'.

  A template description is a series of key descriptions separated by spaces.
Leading and trailing spaces in a template description are ignored. A key
description has the syntax:

  key description ::= name{=name}{/item}
  item ::= quota | required | positional | prefix
  quota ::= number | "f" | "flag" | "l" | "list"
  required ::= "r" | "required"
  positional ::= "p" | "positional"
  prefix ::= "x" | "prefix"

  A name is a series of alphanumeric characters; it must be non null and must
start with an alphabetic character.

  A quota number is a decimal number. If the number starts with '0' the quota
is inexact - a value array of up to 'number' in length can be bound to the key.
If the number does not start with '0' the quota is exact; a value array of
exactly 'number' in length is the only acceptable binding for the key.
  The special quotas "f" and "flag" are equivalent to "0". The special quotas
"l" and "list" are equivalent to "0n" where n is the largest possible cardinal.
  It is legal, though pointless, to have many quota items in a key description.
All but the last one are ignored.

  The required flag (i.e "r" or "required") indicates that argument decoding
must bind an appropriate value to the key to be successful.
  It is legal, though pointless, to specify the required flag more than once.
It is legal, but stupid, to specify the required flag for a key with quota "0".

  The positional flag (i.e. "p" or "positional") indicates that the key can be
bound positionally. It is legal, but stupid, to specify the positional flag for
a key with quota "0".

  The prefix flag (i.e. "x") indicates that all keywords beginning with this
prefix should be treated as values and bound to the keyword.

  If the template description text given to 'NewTemplate' does not comply with
the above syntax the exception 'BadTemplate' is raised. *)



(* Argument decoding *)

TYPE
  Handle <: REFANY;

PROCEDURE Decode(
    template: Template;
    VAR args: T;
    all := TRUE)
    : Handle
    RAISES {};
(* 'Decode' decodes the arguments in 'args' according to 'template'. It returns
a handle. Enquiry functions are provided for handles so that the user can
discover if the decode was successful and, if it was, what bindings were made.

  'Decode' updates 'args'.  Whenever a keyword is found which corresponds to a
key in 'template' it is replaced by NIL. Whenever an argument simple value is
put into an array to be bound to a key it is replaced by NIL.
  If the argument 'all' is FALSE 'Decode' does keyword binding but does not do
positional binding; any unbound arguments are left in 'args'.
  If the argument 'all' is TRUE 'Decode' does keyword binding, then positional
binding and finally checks that all elements of 'args' are NIL. After a
'Decode' with 'all' TRUE every element of 'args' will be NIL.

  Argument decoding can proceed in several phases. The first phases use keyword
binding and each phase removes the keywords it matches plus the argument values
it binds. The final, positional, phase matches any remaining arguments.

  The general picture - remove all arguments bound to keywords and then process
the rest positionally - is simple. However the results may not always be as
expected. Consider the following example. Take the template
   "duo=d/2 trio=t/03/p zero=z/0 normal"
and the following argument array
   {"a", "b", "-normal", "c", "d", "-zero"}
The keyword "-normal" is followed by 2 argument values but only one of them
can be bound. What happens to the spare "d"? One reasonable position is that
it is left to be bound positionally. This is simple to implement but makes
argument arrays hard to understand for the user - it is not possible to pick
out the positional and keyword arguments without reference to the template
description.

  This module takes the view that any argument value that appears between two
keywords cannot be bound positionally. The above example would fail to decode
because it is an attempt to bind 2 values to the key "normal" which has a quota
of 1.

  The result of this view is that positional arguments can only appear:
a) at the start of the argument array, preceding any keywords
b) at the end of the argument array, after the last keyword or argument bound
to a keyword.
This is not a completely consistent position - there can still be confusion as
to which is the last argument value bound to a keyword and which is the first
of the final positional arguments. The case for allowing positional arguments
to appear after keyword arguments is practical; most Unix programs adhere to
that style so lots of people are used to it.

  Note that when positional binding is done there may be two blocks of
positional arguments - one at the beginning and one at the end of the argument
array. The gap between these blocks acts as a separator e.g. Take the template
   "duo=d/2 trio=t/03/p needed=necessary=n/4/r/p zero=z/0 normal"
and the argument array
   {"a", "b", "-zero", "c", "d", "e", "f"}
After argument decoding the value bound to "trio" will be {"a", "b"} - it
cannot grab the next "c", even though its quota would allow it, because of the
gap left by "-zero". The value {"c", "d", "e", "f"} is bound to "needed". *)
  


(* Argument handle enquiry functions *)

PROCEDURE Good(h: Handle): BOOLEAN RAISES {};
(* Did the argument decoding succeed? *)

EXCEPTION
  BadEnquiry;

PROCEDURE Value(
    h: Handle;
    keyword: TEXT)
    : REF ARRAY OF TEXT
    RAISES {BadEnquiry};
(* If 'keyword' is the name of a key in the template used when creating 'h'
'Value' returns the value bound to that key or NIL if the key was not bound.
'Value' will always return a value for a required key. 'keyword' can be any
of a keys alternative names and may be prefixed with '-'.
  If 'keyword' is not a valid key name 'Value' will raise 'BadEnquiry'. It is
a checked runtime error to call 'Value' if 'h' is not 'Good' *)

PROCEDURE Flag(h: Handle; keyword: TEXT): BOOLEAN RAISES {BadEnquiry};
(* A specialised version of 'Value'. 'keyword' must be the name of a key with
quota zero. If it is 'Flag' returns whether or not the key was bound. If it is
not 'BadEnquiry' is raised. It is a checked runtime error to call 'Flag' if 'h'
is not 'Good' *)

PROCEDURE Single(h: Handle; keyword: TEXT): TEXT RAISES {BadEnquiry};
(* A specialised version of 'Value'. 'keyword' must be the name of a key with
an exact quota of one. If this is so 'Single' returns either NIL, if the key
was not bound, or the first element of its value array if it was. Note that a
required key is guaranteed to be bound.
  If 'keyword' is not the name of a key with an exact quota of one 'BadEnquiry'
is raised. It is a checked runtime error to call 'Single' if 'h' is not 'Good'
*)

PROCEDURE Errors(h: Handle; indent: CARDINAL := 0): TEXT RAISES {};
(* If 'h' is not a 'Good' handle 'Errors' returns a text describing the errors
which occurred during argument decoding. The text is long and is split into
several lines. The 'indent' parameter gives the number of spaces to be inserted
at the start of each line.
  It is a checked runtime error to call 'Errors' if 'h' is 'Good' *)



(* Updating an argument handle *)

EXCEPTION
  BadBinding;

PROCEDURE Bind(
    h: Handle;
    keyword: TEXT;
    v: REF ARRAY OF TEXT;
    override := FALSE)
    RAISES {BadBinding};
(* Binds the value 'v' to the key named by 'keyword' in the handle 'h'. If
override is FALSE and the value currently bound to the key is non NIL 'Bind'
does nothing (so 'Bind' can be used to provide default values for keys).
  If 'override' is TRUE or the key is not currently bound 'v' is bound to the
key, providing it is suitable.
  'BadBinding' is raised if 'keyword' is not a valid key name or if 'v' is not
suitable for the named key because:
a) 'v' is NIL and key is required
b) the size of 'v' does not match the quota for the key
  It is a checked runtime error to call 'Bind' if 'h' is not 'Good' *)



(* Standard flags *)

CONST
  StandardTemplateDescription = "help/f id=identify/f";

PROCEDURE Standard(VAR args: T; VAR help, identify: BOOLEAN) RAISES {};
(* A convenience routine. It calls 'Decode' on 'args' with a template built
from 'StandardTemplateDescription'. 'all' is FALSE so the only effect is to
see if "-help" or "-identify" or "-id" appear in 'args'. If "-help" appears
'help' is set to TRUE; if "-identify" or "-id" appear 'identify' is set to
TRUE. Otherwise 'help' and 'identify' are set to FALSE.
  Most programs are expected to call 'Standard' and check to see if they need
to print out help or identification (i.e. name and version) information *)



(* Command line arguments *)

PROCEDURE CommandLine(): REF T RAISES {};
(* Returns the arguments on the command line when the current program was
invoked. The program name is not included; element zero of the array is the
first argument (the program name can be obtained from the 'Params'
interface). Each call of this procedure returns a newly allocated array.  The
contents of the array - i.e. the texts - are only allocated once. *)

END Args.
