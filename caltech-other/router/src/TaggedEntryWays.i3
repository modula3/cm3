(* $Id$ *)
INTERFACE TaggedEntryWays;
IMPORT EntryWays;

TYPE
  T = EntryWays.Tagged;

CONST Brand = "TaggedEntryWays";

(* not really useful, mainly for satisfying generics *)
PROCEDURE Equal(READONLY a, b : T) : BOOLEAN;

END TaggedEntryWays.
