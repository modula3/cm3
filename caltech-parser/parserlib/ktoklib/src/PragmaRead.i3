(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id$ *)

INTERFACE PragmaRead;
IMPORT Rd;
IMPORT Pragma;
TYPE
  T <: Public;
  Public = OBJECT
  METHODS
    init(): T;
    add(p: Pragma.T; name: TEXT);
    (* name is some string beginning with '%', or "" *)
    apply(rd: Rd.T); (* parse a file recognizing added pragmas *)
    error(rd: Rd.T; message: TEXT); (* default: FileRdErr.E *)
  END;
END PragmaRead.
