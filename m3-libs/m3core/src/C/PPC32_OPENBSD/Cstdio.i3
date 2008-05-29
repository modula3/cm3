(* This file is the same or almost the same across all platforms, and
little used. Before commiting content here, let's factor the commonality
and eliminate the dead. *)

INTERFACE Cstdio;

TYPE
    FILE = RECORD END;
    FILE_star = UNTRACED REF FILE;

END Cstdio.
