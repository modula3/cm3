(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

INTERFACE BuiltinSpecials2;

PROCEDURE Register ();
(* Registers "Pickle2.Special"s for the types described below.
   "Register" is called by "Pickle2" during its initialization,
   Pickle clients do not need to call it. *)

END BuiltinSpecials2.

(*
Some common Modula-3 types require special handling during
pickling.  This module provides that handling for the
following types:  Text8Literal.T, Text16Literal.T,
Atom.T, and RefList.T.

Values of type "Text8Literal.T" and "Text16Literal.T" contain
an overly large array of characters.  But, these values are generated
by the compiler and are not heap allocated.  The pickle specials
take care of the bogus array.  Note that subtypes of these types
are pickled by the default pickle writers.

Values of type "Atom.T" with any given name are expected to be unique
within the process.  The pickle specials make sure that this
property is maintained.

Values of type "RefList.T" (and any other type derived from "List.ig")
may contain long chains of nodes which cause the recursive pickler
to overflow its stack.  The pickle specials iterate over the list
without recursing on the stack.  Note that subtypes of "RefList.T"
are handled by the default pickle writers.

*)
