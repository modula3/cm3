(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu May  6 09:24:47 PDT 1993 by muller                   *)

UNSAFE MODULE Cstdio;

BEGIN
  iob[0] := ADR (stdin_file);
  iob[1] := ADR (stdout_file);
  iob[2] := ADR (stderr_file);
END Cstdio.
