(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id$ *)

INTERFACE FileReWr;
IMPORT Wr, Pathname;
TYPE
  T <: Wr.T;
PROCEDURE Open(p: Pathname.T): T;

  (* If the file exists and the contents match what is written,
     then don't touch the file. Otherwise write the file. *)
  
  (* hint: Don't forget to call Wr.Close when you're done *)

  (* must not use in multiple threads *)

END FileReWr.
