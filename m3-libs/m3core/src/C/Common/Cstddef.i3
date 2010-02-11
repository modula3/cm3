(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE Cstddef;
IMPORT Word;

TYPE
  size_t = Word.T;
  ssize_t = INTEGER;
  ptrdiff_t = INTEGER;

END Cstddef.
